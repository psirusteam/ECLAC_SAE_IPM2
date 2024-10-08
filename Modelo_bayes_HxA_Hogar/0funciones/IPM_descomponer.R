calculate_ipm_sample <- function(k, set_data, vars_nbi, weight_fep = "fep") {
  k_pct <- k / 100
  select <- dplyr::select
  # Crear nuevas variables a nivel individual
  for (nbi_ii in names(vars_nbi)) {
    set_data <- set_data %>%
      mutate(!!paste0("g0_", nbi_ii) := round(vars_nbi[nbi_ii] * !!sym(nbi_ii), 5))
  }
  
  temp <- set_data %>%
    summarise_at(.vars = vars(matches("g0")),
                 .funs = list(max = max, min = min)) %>%
    data.frame()
  
  if (!all(round(rowSums(temp), 2) == 1)) {
    warning("The sum of the weights does not equal 1.")
  }
  
  set_data <- set_data %>%
    mutate(sum_g0 = rowSums(select(., starts_with("g0_")), na.rm = TRUE))
  
  # Crear la variable de pobre multidimensional (PMD)
  set_data <- set_data %>%
    mutate(
      !!paste0("PMD_", k) := ifelse(sum_g0 >= k_pct & !is.na(sum_g0), 1, 0),
      !!paste0("cen_sum_g0_", k) := ifelse(!!sym(paste0("PMD_", k)) == 1, sum_g0, 0)
    )
  
  # Diseño de la set_data
  diseno <- set_data %>% as_survey_design(weights = !!sym(weight_fep))
  
  PMD_var <- sym(paste0("PMD", "_", k))
  cen_var <- sym(paste0("cen_sum_g0", "_", k))
  
  # Calcular indicadores de IPM
  ipm_HA <- diseno %>%
    summarise(
      H = survey_mean(!!PMD_var, na.rm = TRUE,  vartype = c("se", "ci")),
      A = survey_ratio(!!cen_var, !!PMD_var, na.rm = TRUE,  vartype = c("se", "ci")),
      A_num = survey_total(!!cen_var, na.rm = TRUE,  vartype = c("se", "ci")),
      A_den = survey_total( !!PMD_var, na.rm = TRUE,  vartype = c("se", "ci")),
      M0 = survey_mean(!!cen_var, na.rm = TRUE,  vartype = c("se", "ci"))
    ) %>% data.frame()
  
  # Calcular contribuciones
  resul <- list()
  for (nbi_ii in names(vars_nbi)) {
    cen_var <- paste0("cen", "_", nbi_ii, "_", k)
    
    # Crear variables censuradas
    diseno_temp <- diseno %>%
      mutate(!!cen_var := ifelse(!!PMD_var == 1, !!sym(nbi_ii), 0))
    
    # Calcular tasas y contribuciones
    Hc_var <- diseno_temp %>%
      summarise(Hc = survey_mean(!!sym(cen_var), vartype = c("se", "ci") , na.rm = TRUE))
    cont_var<-    data.frame(
    nbi =   nbi_ii,
    estimado = vars_nbi[nbi_ii] * Hc_var$Hc / ipm_HA$M0,
    low = vars_nbi[nbi_ii] * Hc_var$Hc_low / ipm_HA$M0,
    upp = vars_nbi[nbi_ii] * Hc_var$Hc_upp / ipm_HA$M0
    )
    resul[[nbi_ii]] <- cont_var
  }
  
  return(list(ipm_HA = ipm_HA, contribuciones = bind_rows(resul)))
}


calculate_ipm_censo <- function(k, set_data, vars_nbi, weight_fep = "n") {
  k_pct <- k / 100
  select <- dplyr::select
  # Crear nuevas variables a nivel individual
  for (nbi_ii in names(vars_nbi)) {
    set_data <- set_data %>%
      mutate(!!paste0("g0_", nbi_ii) := round(vars_nbi[nbi_ii] * !!sym(nbi_ii), 5))
  }
  
  temp <- set_data %>%
    summarise_at(.vars = vars(matches("g0")),
                 .funs = list(max = max, min = min)) %>%
    data.frame()
  
  if (!all(round(rowSums(temp), 2) == 1)) {
    warning("The sum of the weights does not equal 1.")
  }
  
  set_data <- set_data %>%
    mutate(sum_g0 = rowSums(select(., starts_with("g0_")), na.rm = TRUE))
  
  # Crear la variable de pobre multidimensional (PMD)
  set_data <- set_data %>%
    mutate(
      !!paste0("PMD_", k) := ifelse(sum_g0 >= k_pct & !is.na(sum_g0), 1, 0),
      !!paste0("cen_sum_g0_", k) := ifelse(!!sym(paste0("PMD_", k)) == 1, sum_g0, 0)
    )
  

  PMD_var <- sym(paste0("PMD", "_", k))
  cen_var <- sym(paste0("cen_sum_g0", "_", k))
  
  # Calcular indicadores de IPM
  ipm_HA <- set_data %>%
    summarise(
      H = weighted.mean(!!PMD_var, !!sym(weight_fep), na.rm = TRUE),
    A = ifelse(
      sum(!!PMD_var, na.rm = TRUE) > 0,
      sum(!!cen_var * !!PMD_var* !!sym(weight_fep), na.rm = TRUE) / sum(!!PMD_var* !!sym(weight_fep), na.rm = TRUE),
      NA
    ),
      M0 = weighted.mean(!!cen_var, !!sym(weight_fep), na.rm = TRUE)
    ) %>% data.frame()
  
  # Calcular contribuciones
  resul <- numeric()
  for (nbi_ii in names(vars_nbi)) {
    cen_var <- paste0("cen", "_", nbi_ii, "_", k)
    
    # Crear variables censuradas
    set_data_temp <- set_data %>%
      mutate(!!cen_var := ifelse(!!PMD_var == 1, !!sym(nbi_ii), 0))
    
    # Calcular tasas y contribuciones
    Hc_var <- set_data_temp %>%
      summarise(Hc = weighted.mean(!!sym(cen_var), !!sym(weight_fep), na.rm = TRUE))
    
    cont_var <- vars_nbi[nbi_ii] * Hc_var$Hc / ipm_HA$M0
    resul[nbi_ii] <- cont_var
  }
  
  return(list(ipm_HA = ipm_HA, contribuciones = resul))
}


calculate_ipm_by <- function(group_var = NULL, k, set_data, vars_nbi, weight_fep = "fep") {
  if (is.null(group_var)) {
    salida <- calculate_ipm_sample(k = k, set_data = set_data, vars_nbi = vars_nbi, weight_fep = weight_fep)
  } else {
    group <- unique(set_data[[group_var]])
    plan(multisession, workers = detectCores() - 1)
    salida <- map(group, ~{
      filtered_set_data <- set_data %>% filter(!!sym(group_var) == .x)
      calculate_ipm_sample(k = k, set_data = filtered_set_data, vars_nbi = vars_nbi, weight_fep = weight_fep)
    }, .progress = TRUE) %>%
      set_names(group)
  }
  
  return(salida)
}


calculate_ipm_by_censo <- function(group_var = NULL, k, set_data, vars_nbi, weight_fep = "n") {
  if (is.null(group_var)) {
    salida <- calculate_ipm_censo(k = k, set_data = set_data, vars_nbi = vars_nbi, weight_fep = weight_fep)
  } else {
    group <- unique(set_data[[group_var]])
    plan(multisession, workers = detectCores() - 1)
    salida <- map(group, ~{
      filtered_set_data <- set_data %>% filter(!!sym(group_var) == .x)
      calculate_ipm_censo(k = k, set_data = filtered_set_data, vars_nbi = vars_nbi, 
                          weight_fep = weight_fep)
    }, .progress = TRUE) %>%
      set_names(group)
  }
  
  return(salida)
}
