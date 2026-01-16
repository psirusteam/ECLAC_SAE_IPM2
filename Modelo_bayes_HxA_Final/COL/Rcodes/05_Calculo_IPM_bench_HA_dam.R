################################################################################
## Project: Multidimensional Poverty Index (MPI) – HxA Approach
##
## General description:
## This script implements the benchmarking and decomposition stage of the MPI
## under a Small Area Estimation (SAE) framework. It combines:
##
##  - Posterior predictive simulations of deprivation indicators (dummy draws)
##  - Post-stratified census information
##  - Calibration of weights (g-weights) using the Deville–Särndal logit method
##  - Estimation of MPI components (H, A, HA) and their decomposition by domains
##
## The objective is to produce coherent, calibrated, and disaggregated MPI
## estimates with explicit control of numerical stability and statistical
## quality.
##
##
## Output management and posterior aggregation of results
##
## This section performs three key tasks:
##
## 1. Persist iteration-level MPI estimates and contribution decompositions
##    to disk in .rds format, ensuring reproducibility and traceability.
##
## 2. Reload stored objects to compute posterior summary statistics
##    (mean and standard deviation) across Monte Carlo iterations.
##
## 3. Export aggregated results to Excel files for dissemination and
##    downstream analysis by users and institutions.
##
##
## Institution:
## United Nations Economic Commission for Latin America and the Caribbean
## (ECLAC / CEPAL)
################################################################################


################################################################################
## Libraries
################################################################################

# Clean the environment to ensure full reproducibility
rm(list = ls())

# Data manipulation and functional programming
library(tidyverse)
library(magrittr)
library(furrr)

# Bayesian infrastructure and multilevel modeling
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)

# Survey sampling and calibration
library(survey)
library(srvyr)
library(sampling)

# Export utilities
library(openxlsx)

# Avoid conflicts with select()
select <- dplyr::select

# Project-specific helper functions
source("Modelo_bayes_HxA_Final/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA_Final/0funciones/agregado_dim_ipm.r")
source("Modelo_bayes_HxA_Final/0funciones/IPM_descomponer.R")


################################################################################
## Data input
################################################################################

# Post-stratified census dataset (output from previous pipeline stages)
censo_ipm <- readRDS(
  "Modelo_bayes_HxA_Final/COL/Data/Censo/censo_COL_recor.rds"
)

# Survey dataset used for direct (benchmark) MPI estimation
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Final/COL/Data/Encuestas/encuesta_nbi.rds") %>%
  filter(edad > 1, anoest < 6)

# Ensure deprivation indicators are numeric
encuesta_ipm <- encuesta_ipm %>%
  mutate_at(vars(matches("nbi")), as.numeric)


################################################################################
## Definition of MPI weights (official structure)
################################################################################

# Each MPI dimension receives an equal weight (1/12)
vars_nbi_hogar <-
  c(
    "nbi_hnolee_ee"        = 1/12,
    "nbi_hlogroeduc_ee"    = 1/12,
    "nbi_heducninios"      = 1/12,
    "nbi_hhacina"          = 1/12,
    "nbi_henergia"         = 1/12,
    "nbi_htic"             = 1/12,
    "nbi_hagua_ee"         = 1/12,
    "nbi_hsaneamiento_ee"  = 1/12,
    "nbi_hsalud_ee"        = 1/12,
    "nbi_hpartemp"         = 1/12,
    "nbi_hempe"            = 1/12,
    "nbi_hjub"             = 1/12
  )


################################################################################
## Direct (benchmark) MPI estimation from survey data
################################################################################

# Direct MPI estimation used as calibration benchmark
resul_sample <- calculate_ipm_sample(
  k = 40,
  set_data = encuesta_ipm,
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep"
)

# Reference totals used for benchmarking
N_sample <- sum(encuesta_ipm$fep)

total <- c(
  N_sample,
  N_sample * resul_sample$ipm_HA$H,
  resul_sample$ipm_HA$A_num
)


################################################################################
## Load posterior predictive dummy matrices
################################################################################

# Each matrix corresponds to one MPI dimension.
# Matrices are transposed so that rows represent posterior iterations.

epred_mat_dummy_hnolee        <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hnolee_ee.rds") %>% t()
epred_mat_dummy_hlogroeduc    <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hlogroeduc_ee.rds") %>% t()
epred_mat_dummy_heducninios   <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_heducninios.rds") %>% t()
epred_mat_dummy_hhacina       <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hhacina.rds") %>% t()
epred_mat_dummy_henergia      <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_henergia.rds") %>% t()
epred_mat_dummy_htic          <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_htic.rds") %>% t()
epred_mat_dummy_hagua         <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hagua_ee.rds") %>% t()
epred_mat_dummy_hsaneamiento  <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hsaneamiento_ee.rds") %>% t()
epred_mat_dummy_hsalud        <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hsalud_ee.rds") %>% t()
epred_mat_dummy_hpartemp      <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hpartemp.rds") %>% t()
epred_mat_dummy_hempe         <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hempe.rds") %>% t()
epred_mat_dummy_hjub          <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hjub.rds") %>% t()


################################################################################
## Initialize result containers
################################################################################

ipm_HA_national         <- list()
contributions_national  <- list()

ipm_HA_department       <- list()
contributions_department <- list()

ipm_HA_area             <- list()
contributions_area      <- list()

k <- 40


################################################################################
## Main loop over posterior iterations
################################################################################

for (iter in 1:1000) {
  
  ###########################################################################
  ## Construction of simulated census data (single iteration)
  ###########################################################################
  
  # Inject dummy realizations for each MPI dimension
  censo_ipm_temp <- censo_ipm %>%
    mutate(
      !!paste0("hnolee_", iter)        := epred_mat_dummy_hnolee[iter,],
      !!paste0("hlogroeduc_", iter)    := epred_mat_dummy_hlogroeduc[iter,],
      !!paste0("heducninios_", iter)   := epred_mat_dummy_heducninios[iter,],
      !!paste0("hhacina_", iter)       := epred_mat_dummy_hhacina[iter,],
      !!paste0("henergia_", iter)      := epred_mat_dummy_henergia[iter,],
      !!paste0("htic_", iter)          := epred_mat_dummy_htic[iter,],
      !!paste0("hagua_", iter)         := epred_mat_dummy_hagua[iter,],
      !!paste0("hsaneamiento_", iter)  := epred_mat_dummy_hsaneamiento[iter,],
      !!paste0("hsalud_", iter)        := epred_mat_dummy_hsalud[iter,],
      !!paste0("hpartemp_", iter)      := epred_mat_dummy_hpartemp[iter,],
      !!paste0("hempe_", iter)         := epred_mat_dummy_hempe[iter,],
      !!paste0("hjub_", iter)          := epred_mat_dummy_hjub[iter,]
    )
  
  ###########################################################################
  ## Iteration-specific MPI weights
  ###########################################################################
  
  vars_nbi_hogar <- c(
    hnolee        = 1/12,
    hlogroeduc    = 1/12,
    heducninios   = 1/12,
    hhacina       = 1/12,
    henergia      = 1/12,
    htic          = 1/12,
    hagua         = 1/12,
    hsaneamiento  = 1/12,
    hsalud        = 1/12,
    hpartemp      = 1/12,
    hempe         = 1/12,
    hjub          = 1/12
  )
  
  names(vars_nbi_hogar) <- paste0(names(vars_nbi_hogar), "_", iter)
  
  ###########################################################################
  ## Benchmarking via calibration (Deville–Särndal, logit)
  ###########################################################################
  
  censo_ipm_temp <- censo_ipm_temp %>%
    mutate(den = 1) %>%
    fastDummies::dummy_cols("den", remove_selected_columns = FALSE)
  
  k_pct <- k / 100
  
  # Construction of weighted deprivation components
  for (nbi_ii in names(vars_nbi_hogar)) {
    censo_ipm_temp <- censo_ipm_temp %>%
      mutate(!!paste0("g0_", nbi_ii) :=
               round(vars_nbi_hogar[nbi_ii] * !!sym(nbi_ii), 5))
  }
  
  censo_ipm_temp <- censo_ipm_temp %>%
    mutate(
      sum_g0 = rowSums(select(., starts_with("g0_")), na.rm = TRUE),
      PMD = ifelse(sum_g0 >= k_pct & !is.na(sum_g0), 1, 0),
      cen_sum_g0 = ifelse(PMD == 1, sum_g0, 0)
    )
  
  # Calibration of weights
  censo_ipm_temp$gk <- calib(
    Xs = censo_ipm_temp %>% select("den_1", "PMD", "cen_sum_g0"),
    d  = censo_ipm_temp$n,
    total = total,
    method = "logit",
    max_iter = 500
  )
  
  ###########################################################################
  ## Diagnostic checks for calibration factors (gk)
  ###########################################################################
  
  gk_min  <- min(censo_ipm_temp$gk, na.rm = TRUE)
  gk_mean <- mean(censo_ipm_temp$gk, na.rm = TRUE)
  gk_max  <- max(censo_ipm_temp$gk, na.rm = TRUE)
  
  cat(
    "Iter =", iter,
    "| min(gk) =", round(gk_min, 4),
    "| mean(gk) =", round(gk_mean, 4),
    "| max(gk) =", round(gk_max, 4), "\n"
  )
  
  # Warning: potential calibration tension
  if (gk_min < 0.3) {
    warning(
      paste0(
        "[SAE WARNING] iter=", iter,
        ": min(gk) < 0.3. Possible instability or domain misspecification."
      )
    )
  }
  
  # Critical error: calibration likely invalid
  if (gk_min < 0.1) {
    stop(
      paste0(
        "[SAE ERROR] iter=", iter,
        ": min(gk) < 0.1. Calibration weights are critically small."
      )
    )
  }
  
  ###########################################################################
  ## MPI estimation and decomposition
  ###########################################################################
  
  ipm_national <- calculate_ipm_by_censo(
    group_var = NULL,
    k = 40,
    set_data = censo_ipm_temp %>% mutate(fep2 = n * gk),
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep2"
  )
  
  ipm_department <- calculate_ipm_by_censo(
    group_var = "dam",
    k = 40,
    set_data = censo_ipm_temp %>% mutate(fep2 = n * gk),
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep2"
  )
  
  ipm_area <- calculate_ipm_by_censo(
    group_var = "area",
    k = 40,
    set_data = censo_ipm_temp %>% mutate(fep2 = n * gk),
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep2"
  )
  
  ###########################################################################
  ## Store iteration-specific results
  ###########################################################################
  
  ipm_HA_national[[iter]]          <- ipm_national$ipm_HA
  contributions_national[[iter]]   <- ipm_national$contributions
  
  ipm_HA_department[[iter]]        <- map_df(ipm_department, ~ .x$ipm_HA, .id = "dam")
  contributions_department[[iter]] <- map_dfr(ipm_department, ~ .x$contributions, .id = "dam")
  
  ipm_HA_area[[iter]]              <- map_df(ipm_area, ~ .x$ipm_HA, .id = "area")
  contributions_area[[iter]]       <- map_dfr(ipm_area, ~ .x$contributions, .id = "area")
  
  cat("End iteration", iter, "\n")
}


################################################################################
## Save iteration-level results to disk (.rds)
################################################################################

# National-level contributions by MPI dimension (per iteration)
saveRDS(
  contribuciones_nacional,
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.rds"
)

# National-level MPI components (H, A, HA) by iteration
saveRDS(
  ipm_HA_nacional,
  "Modelo_bayes_HxA_Final/COL/Output/ipm_HA_nacional.rds"
)

# Department-level contributions by MPI dimension (per iteration)
saveRDS(
  contribuciones_depto,
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.rds"
)

# Department-level MPI components by iteration
saveRDS(
  ipm_HA_depto,
  "Modelo_bayes_HxA_Final/COL/Output/ipm_HA_dam.rds"
)

# Area-level contributions by MPI dimension (per iteration)
saveRDS(
  contribuciones_area,
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_area.rds"
)

# Area-level MPI components by iteration
saveRDS(
  ipm_HA_area,
  "Modelo_bayes_HxA_Final/COL/Output/ipm_HA_area.rds"
)


################################################################################
## National-level posterior aggregation
################################################################################

# Reload national-level objects from disk
contribuciones <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.rds")

ipm_HA <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/ipm_HA_nacional.rds")

# Aggregate MPI components across iterations:
# posterior mean and posterior standard deviation
ipm_HA_agg <- ipm_HA %>%
  bind_rows() %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Aggregate dimensional contributions:
# - Remove iteration suffixes from variable names
# - Compute posterior mean and standard deviation
contribuciones_agg <- map_df(contribuciones, function(df) {
  names(df) <- gsub("_\\d+", "", names(df))
  df
}) %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Export national-level aggregated results to Excel
openxlsx::write.xlsx(
  list(
    ipm_HA = ipm_HA_agg,
    contribuciones = contribuciones_agg
  ),
  file = "Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.xlsx"
)

# Open the Excel file for inspection
openxlsx::openXL(
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.xlsx"
)


################################################################################
## Department-level posterior aggregation
################################################################################

# Reload department-level objects from disk
contribuciones <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.rds")

ipm_HA <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/ipm_HA_dam.rds")

# Aggregate MPI components by department
ipm_HA_agg <- ipm_HA %>%
  bind_rows() %>%
  group_by(dam) %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Aggregate dimensional contributions by department
contribuciones_agg <- map_df(contribuciones, function(df) {
  names(df) <- gsub("_\\d+", "", names(df))
  df
}) %>%
  group_by(dam) %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Export department-level aggregated results to Excel
openxlsx::write.xlsx(
  list(
    ipm_HA = ipm_HA_agg,
    contribuciones = contribuciones_agg
  ),
  file = "Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.xlsx"
)

# Open the Excel file for inspection
openxlsx::openXL(
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.xlsx"
)


################################################################################
## Area-level posterior aggregation
################################################################################

# Reload area-level objects from disk
contribuciones <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/contribuciones_area.rds")

ipm_HA <-
  readRDS("Modelo_bayes_HxA_Final/COL/Output/ipm_HA_area.rds")

# Aggregate MPI components by area
ipm_HA_agg <- ipm_HA %>%
  bind_rows() %>%
  group_by(area) %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Aggregate dimensional contributions by area
contribuciones_agg <- map_df(contribuciones, function(df) {
  names(df) <- gsub("_\\d+", "", names(df))
  df
}) %>%
  group_by(area) %>%
  summarise_all(.funs = list(media = mean, sd = sd))

# Export area-level aggregated results to Excel
openxlsx::write.xlsx(
  list(
    ipm_HA = ipm_HA_agg,
    contribuciones = contribuciones_agg
  ),
  file = "Modelo_bayes_HxA_Final/COL/Output/contribuciones_area.xlsx"
)

# Open the Excel file for inspection
openxlsx::openXL(
  "Modelo_bayes_HxA_Final/COL/Output/contribuciones_area.xlsx"
)
