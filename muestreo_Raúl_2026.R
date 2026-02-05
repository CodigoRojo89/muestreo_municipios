# ============================================================
# PLAN DE MUESTREO: Asignación municipal + Cuotas sexo x edad
# ============================================================

suppressWarnings({
  suppressMessages({
    library(dplyr)
    library(tidyr)
    library(tibble) # <-- para print con n = Inf
    library(readr)  # para exportar CSV (opcional)
  })
})

# ----------------------------
# 1) Funciones base (MAS + FPC)
# ----------------------------
srs_n0 <- function(p, Z, d, deff = 1) {
  deff * (Z^2) * p * (1 - p) / (d^2)
}

srs_n_fpc <- function(N, p, Z, d, deff = 1) {
  n0 <- srs_n0(p, Z, d, deff)
  n0 / (1 + (n0 - 1) / N)
}

moe_achieved <- function(N, n, p, Z, deff = 1) {
  n <- pmax(1, pmin(n, N - 1))  # evita problemas numéricos
  Z * sqrt(deff * p * (1 - p) / n) * sqrt((N - n) / (N - 1))
}

# ----------------------------------------------------------------------------
# 2) Cálculo "baseline" por municipio (usa parámetros por grupo + FPC)
# ----------------------------------------------------------------------------
calcular_baseline_por_mpio <- function(df_mpios, param_grupos) {
  stopifnot(all(c("municipio","N","grupo") %in% names(df_mpios)))
  stopifnot(all(c("grupo","p","Z","d","deff","rr") %in% names(param_grupos)))
  
  df_mpios %>%
    dplyr::left_join(param_grupos, by = "grupo") %>%
    dplyr::mutate(
      n_teorica  = srs_n_fpc(N = N, p = p, Z = Z, d = d, deff = deff),
      n_ajustada = n_teorica / rr,
      n_base     = pmin(N, n_ajustada)
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "municipio","grupo","N","p","Z","d","deff","rr",
      "n_teorica","n_ajustada","n_base"
    )))
}

# -----------------------------------------------------------
# 3) Asignación con topes y suma objetivo exacta
# -----------------------------------------------------------
asignar_con_cap_y_total <- function(targets, caps, total_objetivo) {
  if (sum(caps) < total_objetivo) {
    warning(paste0("Capacidad total (", sum(caps),
                   ") < total objetivo (", total_objetivo,
                   "). Se asignará el máximo posible."))
    total_objetivo <- sum(caps)
  }
  
  asign <- pmin(floor(targets), caps)
  resto <- pmax(0, targets - asign)
  faltan <- total_objetivo - sum(asign)
  
  if (faltan <= 0) return(asign)
  
  while (faltan > 0) {
    cap_rem <- pmax(0, caps - asign)
    if (sum(cap_rem) == 0) break
    idx <- order(resto, cap_rem, decreasing = TRUE)
    progreso <- 0
    for (i in idx) {
      if (faltan == 0) break
      if (asign[i] < caps[i]) {
        asign[i] <- asign[i] + 1
        faltan <- faltan - 1
        progreso <- progreso + 1
      }
    }
    if (progreso == 0) break
  }
  
  if (faltan > 0) warning("No fue posible alcanzar el total objetivo exactamente por falta de capacidad.")
  asign
}

# ----------------------------------------------------------
# 4) Asegurar columnas de parámetros (si el join no trajo alguna)
# ----------------------------------------------------------
ensure_params_cols <- function(df,
                               defaults = list(p = 0.5, Z = 1.96, d = 0.05, deff = 1.5, rr = 0.95)) {
  for (nm in names(defaults)) {
    if (!nm %in% names(df)) df[[nm]] <- defaults[[nm]]
    df[[nm]] <- dplyr::coalesce(df[[nm]], defaults[[nm]])
  }
  df
}

# ----------------------------------------------------------
# 5) Estrategias de asignación municipal del N_final_total
# ----------------------------------------------------------
asignar_muestra_municipal <- function(df_mpios, param_grupos,
                                      N_final_total,
                                      estrategia = c("escala_baseline","proporcional_N","proporcional_N_por_grupo")) {
  
  estrategia <- match.arg(estrategia)
  stopifnot(is.numeric(N_final_total), N_final_total > 0)
  stopifnot(all(c("municipio","N","grupo") %in% names(df_mpios)))
  stopifnot(all(c("grupo","p","Z","d","deff","rr") %in% names(param_grupos)))
  
  caps <- df_mpios$N
  
  if (estrategia == "escala_baseline") {
    base <- calcular_baseline_por_mpio(df_mpios, param_grupos)
    
    if (sum(base$n_base, na.rm = TRUE) <= 0) {
      warning("La suma de n_base es 0; se usará asignación proporcional a N.")
      targets <- df_mpios$N / sum(df_mpios$N) * N_final_total
      out <- df_mpios
    } else {
      factor <- N_final_total / sum(base$n_base, na.rm = TRUE)
      targets <- base$n_base * factor
      out <- dplyr::left_join(df_mpios, base, by = dplyr::all_of(c("municipio","N","grupo")))
    }
    
    asign <- asignar_con_cap_y_total(targets = targets, caps = caps, total_objetivo = N_final_total)
    out <- out %>% dplyr::mutate(target_escalado = targets, n_final = asign)
    
  } else if (estrategia == "proporcional_N") {
    targets <- df_mpios$N / sum(df_mpios$N) * N_final_total
    asign <- asignar_con_cap_y_total(targets = targets, caps = caps, total_objetivo = N_final_total)
    out <- df_mpios %>% dplyr::mutate(target_escalado = targets, n_final = asign)
    
  } else if (estrategia == "proporcional_N_por_grupo") {
    macro <- df_mpios %>%
      dplyr::group_by(grupo) %>%
      dplyr::summarise(N_grupo = sum(N), .groups = "drop") %>%
      dplyr::mutate(n_grupo_target = N_final_total * N_grupo / sum(N_grupo))
    
    out <- df_mpios %>%
      dplyr::left_join(macro, by = "grupo") %>%
      dplyr::group_by(grupo) %>%
      dplyr::group_modify(~{
        d <- .x
        caps_g <- d$N
        targets_g <- d$N / sum(d$N) * unique(d$n_grupo_target)
        asign_g <- asignar_con_cap_y_total(targets_g, caps_g, round(unique(d$n_grupo_target)))
        d %>% dplyr::mutate(target_escalado = targets_g, n_final = asign_g)
      }) %>%
      dplyr::ungroup()
    
    # Ajuste fino por redondeo entre grupos para asegurar suma exacta
    delta <- N_final_total - sum(out$n_final)
    if (delta != 0) {
      resto <- out$target_escalado - floor(out$target_escalado)
      cap_rem <- out$N - out$n_final
      signo <- ifelse(delta > 0, 1, -1)
      idx <- order(resto * signo, cap_rem * signo, decreasing = TRUE)
      i <- 1
      while (delta != 0 && i <= length(idx)) {
        j <- idx[i]
        if (signo == 1 && out$n_final[j] < out$N[j]) {
          out$n_final[j] <- out$n_final[j] + 1; delta <- delta - 1
        } else if (signo == -1 && out$n_final[j] > 0) {
          out$n_final[j] <- out$n_final[j] - 1; delta <- delta + 1
        }
        i <- i + 1
      }
      if (delta != 0) warning("No se pudo ajustar exactamente la suma total tras el redondeo por grupo.")
    }
  }
  
  out %>%
    dplyr::left_join(param_grupos, by = "grupo") %>%
    ensure_params_cols() %>%
    dplyr::mutate(d_alcanzado = moe_achieved(N = N, n = n_final, p = p, Z = Z, deff = deff)) %>%
    dplyr::select(dplyr::any_of(c(
      "municipio","grupo","N","n_final","d_alcanzado","target_escalado","p","Z","d","deff","rr"
    )))
}

# --------------------------------------------------------------------
# 6) Cuotas por sexo y edad (H/M x 18–29, 30–44, 45–59, 60+)
# --------------------------------------------------------------------
validar_y_normalizar_dist <- function(dist_ref,
                                      nivel_tolerancia = 1e-6,
                                      normalizar_si_no_suma = TRUE) {
  req_cols <- c("municipio","sexo","edad_rango")
  if (!all(req_cols %in% names(dist_ref))) {
    stop("dist_ref debe incluir columnas: municipio, sexo, edad_rango, y prop o N_estrato.")
  }
  if (!("prop" %in% names(dist_ref)) && !("N_estrato" %in% names(dist_ref))) {
    stop("dist_ref debe incluir 'prop' (0-1) o 'N_estrato' (conteos).")
  }
  
  df <- dist_ref %>%
    mutate(across(all_of(c("municipio","sexo","edad_rango")), as.character))
  
  if ("N_estrato" %in% names(df)) {
    if (!is.numeric(df$N_estrato)) stop("N_estrato debe ser numérica.")
    df <- df %>%
      group_by(municipio) %>%
      mutate(prop = N_estrato / sum(N_estrato)) %>%
      ungroup()
  } else {
    if (!is.numeric(df$prop)) stop("'prop' debe ser numérica.")
  }
  
  check <- df %>%
    group_by(municipio) %>%
    summarise(suma = sum(prop), .groups = "drop")
  
  if (any(abs(check$suma - 1) > nivel_tolerancia)) {
    if (normalizar_si_no_suma) {
      warning("Las proporciones no suman 1 por municipio; se normalizarán internamente.")
      df <- df %>%
        group_by(municipio) %>%
        mutate(prop = prop / sum(prop)) %>%
        ungroup()
    } else {
      stop("Las proporciones no suman 1 por municipio.")
    }
  }
  
  df
}

redondeo_mayores_restos <- function(target, total_objetivo) {
  n_floor <- floor(target)
  resto   <- target - n_floor
  faltan  <- total_objetivo - sum(n_floor)
  asign   <- n_floor
  if (faltan > 0) {
    idx <- order(resto, decreasing = TRUE)
    asign[idx[seq_len(faltan)]] <- asign[idx[seq_len(faltan)]] + 1
  }
  asign
}

ajustar_minimos_por_celda <- function(asign, min_por_celda, total_objetivo) {
  if (is.null(min_por_celda) || min_por_celda <= 0) return(asign)
  k <- length(asign)
  min_requerido <- min_por_celda * k
  if (total_objetivo < min_requerido) {
    warning("Total < k*min_por_celda; se reparte lo más parejo posible.")
    base <- floor(total_objetivo / k)
    extra <- total_objetivo - base * k
    asign <- rep(base, k)
    if (extra > 0) asign[seq_len(extra)] <- asign[seq_len(extra)] + 1
    return(asign)
  }
  deficit <- pmax(0, min_por_celda - asign)
  asign <- asign + deficit
  sobra <- sum(asign) - total_objetivo
  if (sobra <= 0) return(asign)
  idx <- order(asign, decreasing = TRUE)
  i <- 1
  while (sobra > 0 && i <= length(idx)) {
    j <- idx[i]
    if (asign[j] > min_por_celda) { asign[j] <- asign[j] - 1; sobra <- sobra - 1 }
    i <- i + 1
  }
  asign
}

asignar_cuotas_sexo_edad <- function(muestra_municipal, dist_ref, min_por_celda = 0) {
  stopifnot(all(c("municipio","n_final") %in% names(muestra_municipal)))
  dist_norm <- validar_y_normalizar_dist(dist_ref)
  
  targets <- dist_norm %>%
    inner_join(muestra_municipal %>% dplyr::select(dplyr::all_of(c("municipio","n_final"))), by = "municipio") %>%
    mutate(objetivo = prop * n_final)
  
  cuotas <- targets %>%
    group_by(municipio) %>%
    group_modify(~{
      d <- .x
      objetivo <- d$objetivo
      total <- unique(d$n_final)
      asign1 <- redondeo_mayores_restos(objetivo, total)
      asign2 <- ajustar_minimos_por_celda(asign1, min_por_celda, total)
      d$cuota <- asign2
      d
    }) %>%
    ungroup() %>%
    dplyr::select(dplyr::all_of(c("municipio","sexo","edad_rango","prop","n_final","cuota")))
  
  check <- cuotas %>%
    group_by(municipio) %>%
    summarise(total_cuotas = sum(cuota), n_final = unique(n_final), .groups = "drop") %>%
    mutate(ok = (total_cuotas == n_final))
  
  if (any(!check$ok)) {
    warning("Hay municipios donde la suma de cuotas no iguala n_final.")
    print(as_tibble(check %>% filter(!ok)), n = Inf)
  }
  
  list(cuotas = cuotas, verificacion = check)
}

# ----------------------------------------------------------------
# 7) Orquestador de todo el flujo
# ----------------------------------------------------------------
plan_muestra <- function(df_mpios, param_grupos,
                         N_final_total,
                         estrategia = c("escala_baseline","proporcional_N","proporcional_N_por_grupo"),
                         dist_ref,
                         min_por_celda = 0) {
  
  estrategia <- match.arg(estrategia)
  
  muestra_municipal <- asignar_muestra_municipal(
    df_mpios = df_mpios,
    param_grupos = param_grupos,
    N_final_total = N_final_total,
    estrategia = estrategia
  )
  
  res_cuotas <- asignar_cuotas_sexo_edad(
    muestra_municipal = muestra_municipal %>% dplyr::select(dplyr::all_of(c("municipio","n_final"))),
    dist_ref = dist_ref,
    min_por_celda = min_por_celda
  )
  
  list(
    muestra_municipal = muestra_municipal,
    cuotas = res_cuotas$cuotas,
    verificacion_cuotas = res_cuotas$verificacion,
    total_asignado = sum(muestra_municipal$n_final)
  )
}

# ---------------------------
# 8) TUS DATOS (3 grupos)
# ---------------------------

# Grupo 1
g1 <- data.frame(
  municipio = c("Celaya","Irapuato","León","Salamanca","Silao de la Victoria","Guanajuato"),
  N         = c(365309, 409113, 1177073, 194601, 132270, 136247),
  grupo     = "G1",
  stringsAsFactors = FALSE
)

# Grupo 2
g2 <- data.frame(
  municipio = c("Dolores Hidalgo","San Miguel de Allende","Pénjamo","Valle de Santiago"),
  N         = c(102896, 117311, 105043, 103620),
  grupo     = "G2",
  stringsAsFactors = FALSE
)

# Grupo 3 (corregido "Manuel Doblado")
g3 <- data.frame(
  municipio = c("Atarjea","Doctor Mora","San José Iturbide","San Luis de la Paz","Santa Catarina","Tierra Blanca",
                "Victoria","Xichú","Abasolo","Ocampo","San Diego de la Unión","San Felipe","Acámbaro","Coroneo",
                "Cuerámaro","Huanímaro","Jerécuaro","Manuel Doblado","Moroleón","Pueblo Nuevo","Salvatierra",
                "Santiago Maravatío","Tarandacuao","Uriangato","Yuriria","Apaseo el Alto","Apaseo el Grande",
                "Comonfort","Cortazar","Jaral del Progreso","Purísima del Rincón","Romita",
                "San Francisco del Rincon","Santa Cruz de Juventino Rosas","Tarimoro","Villagrán"),
  N = c(3493,17302,57476,81306,3674,12514,13404,6947,61603,17149,26285,74571,76891,7345,20691,14272,33101,
        27261,35309,8480,67301,4884,7858,44660,47798,42387,75734,53561,68329,26666,53941,42824,86566,54345,25443,44088),
  grupo = "G3",
  stringsAsFactors = FALSE
)

df_mpios <- bind_rows(g1, g2, g3)

# ---------------------------
# 9) Parámetros por grupo
# ---------------------------
param_grupos <- tibble::tibble(
  grupo = c("G1","G2","G3"),
  p     = c(0.5, 0.5, 0.5),
  Z     = c(1.96, 1.96, 1.96),
  d     = c(0.03, 0.05, 0.07),
  deff  = c(1.7, 1.6, 1.5),  # ajusta si quieres
  rr    = c(0.95, 0.95, 0.95)
)

# ---------------------------
# 10) Dist. de referencia para cuotas (PLANTILLA UNIFORME)
# ---------------------------
dist_ref <- tidyr::expand_grid(
  municipio = unique(df_mpios$municipio),
  sexo = c("H","M"),
  edad_rango = c("18-29","30-44","45-59","60+")
) %>%
  group_by(municipio) %>%
  mutate(prop = 1 / n()) %>%
  ungroup()

# ---------------------------
# 11) Ejecutar plan
# ---------------------------
N_final_total <- 5500
estrategia <- "escala_baseline"
min_por_celda <- 0

plan <- plan_muestra(
  df_mpios = df_mpios,
  param_grupos = param_grupos,
  N_final_total = N_final_total,
  estrategia = estrategia,
  dist_ref = dist_ref,
  min_por_celda = min_por_celda
)

# ---------------------------
# 12) Vistas y verificaciones (convertimos a tibble para print n = Inf)
# ---------------------------

cat("\nTotal asignado:", plan$total_asignado, "(objetivo:", N_final_total, ")\n\n")

cat("--- Totales por grupo ---\n")
plan$muestra_municipal %>%
  group_by(grupo) %>%
  summarise(N_poblacion = sum(N), n_asignado = sum(n_final), .groups = "drop") %>%
  as_tibble() %>%
  print(n = Inf)

cat("\n--- Muestra municipal con margen de error alcanzado (en %): ---\n")
plan$muestra_municipal %>%
  arrange(grupo, desc(n_final)) %>%
  mutate(d_alcanzado_pct = round(100 * d_alcanzado, 2)) %>%
  select(municipio, grupo, N, n_final, d_alcanzado_pct) %>%
  as_tibble() %>%
  print(n = Inf)

cat("\n--- Primeras filas de cuotas (sexo x edad): ---\n")
plan$cuotas %>%
  arrange(municipio, sexo, edad_rango) %>%
  as_tibble() %>%
  print(n = 40)

cat("\n--- Verificación de sumas de cuotas = n_final municipal: ---\n")
plan$verificacion_cuotas %>%
  as_tibble() %>%
  print(n = Inf)

# ---------------------------
# 13) Exportar (opcional)
# ---------------------------
# readr::write_csv(plan$muestra_municipal, "muestra_municipal_5500.csv")
# readr::write_csv(plan$cuotas,           "cuotas_sexo_edad_5500.csv")
``