################################################################################
## Project: Multidimensional Poverty Index (MPI / IPM) – HxA Framework
## Scope  : Household × Area (HxA) modeling for MPI components
##
## Purpose:
## This script estimates multilevel binomial models for each MPI deprivation
## indicator and for the overall MPI indicator, using household survey data
## aggregated by demographic and geographic domains.
##
## The models are used to generate small-area predictions and to assess
## consistency between observed (direct) and model-based estimates.
##
##
## Institution:
## United Nations Economic Commission for Latin America and the Caribbean
## (ECLAC / CEPAL)
################################################################################


################################################################################
## Libraries
################################################################################

# Clean R session to avoid contamination from previous objects
rm(list = ls())

# Core data manipulation and functional programming
library(tidyverse)
library(magrittr)
library(furrr)

# Bayesian and mixed-effects modeling
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)
library(bayesplot)

# Data input/output
library(haven)
library(patchwork)


################################################################################
## Load prepared survey data
################################################################################

# Read household-level dataset previously prepared for SAE
# (see data preparation script)
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Final/COL/Data/Encuestas/encuesta_nbi.rds") %>%
  
  # Restrict analysis to relevant population:
  # - Exclude youngest age group
  # - Exclude undefined or extreme education categories
  filter(edad > 1, anoest < 6)


################################################################################
## Construction of survey-based MPI indicator
################################################################################

# Compute the MPI score as the average of the 12 deprivation indicators
# and apply the standard poverty cutoff (k = 0.4)
encuesta_ipm <- encuesta_ipm %>%
  mutate(
    ipm_encuesta = (1 / 12) * (
      as.numeric(nbi_hnolee_ee) +
        as.numeric(nbi_hlogroeduc_ee) +
        as.numeric(nbi_heducninios) +
        as.numeric(nbi_hhacina) +
        as.numeric(nbi_henergia) +
        as.numeric(nbi_htic) +
        as.numeric(nbi_hagua_ee) +
        as.numeric(nbi_hsaneamiento_ee) +
        as.numeric(nbi_hsalud_ee) +
        as.numeric(nbi_hpartemp) +
        as.numeric(nbi_hempe) +
        as.numeric(nbi_hjub)
    ),
    
    # Binary MPI indicator
    ipm_encuesta = ifelse(ipm_encuesta >= 0.4, "1", "0")
  )


################################################################################
## Load area-level auxiliary predictors
################################################################################

# Area-level covariates used as fixed effects in the multilevel models
statelevel_predictors_df <-
  readRDS(
    "Modelo_bayes_HxA_Final/COL/Data/statelevel_predictors_df_dam2.rds"
  )

# Quick inspection of auxiliary data
summary(statelevel_predictors_df)


################################################################################
## Aggregation structure
################################################################################

# Variables defining demographic and geographic aggregation cells
byAgrega <- c(
  "dam", "dam2", "area", "sexo", "etnia", "anoest", "edad"
)


################################################################################
## MPI dimensions to be modeled
################################################################################

# List of deprivation indicators and the global MPI indicator
nbi_hogar <- c(
  "nbi_hnolee_ee",
  "nbi_hlogroeduc_ee",
  "nbi_heducninios",
  "nbi_hhacina",
  "nbi_henergia",
  "nbi_htic",
  "nbi_hagua_ee",
  "nbi_hsaneamiento_ee",
  "nbi_hsalud_ee",
  "nbi_hpartemp",
  "nbi_hempe",
  "nbi_hjub",
  "ipm_encuesta"
)


################################################################################
## Aggregate survey data by MPI dimension
################################################################################

# For each deprivation indicator:
# - Aggregate household data by demographic/geographic cells
# - Compute number of deprived (yno) and non-deprived (ysi)
# - Merge with area-level predictors
encuesta_df <- map(
  setNames(nbi_hogar, nbi_hogar),
  function(y) {
    
    encuesta_ipm$temp <- as.numeric(encuesta_ipm[[y]])
    
    encuesta_ipm %>%
      group_by_at(byAgrega) %>%
      summarise(
        n   = n(),
        yno = sum(temp),
        ysi = n - yno,
        .groups = "drop"
      ) %>%
      inner_join(
        statelevel_predictors_df,
        by = c("dam", "dam2")
      )
  }
)


################################################################################
## Multilevel model specification
################################################################################

# Fixed-effect covariates (excluding area identifiers)
names_cov <- statelevel_predictors_df %>%
  dplyr::select(-dam, -dam2) %>%
  names()

# Include individual-level covariates explicitly
names_cov <- c("sexo", "area", names_cov)

# Random intercept structure:
# - Administrative domain (dam)
# - Ethnicity
# - Age group
# - Education level
efec_aleat <-
  paste0(
    "(1|",
    c("dam", "etnia", "edad", "anoest"),
    ")",
    collapse = " + "
  )

# Final binomial multilevel model formula
formula_mod <-
  formula(
    paste(
      "cbind(yno, ysi) ~",
      efec_aleat,
      "+",
      paste0(names_cov, collapse = " + ")
    )
  )


################################################################################
## Parallel computation setup
################################################################################

# Enable parallel execution for model fitting
plan(multisession, workers = 2)


################################################################################
## Model estimation function
################################################################################

# Fit a multilevel binomial-logit model for a given MPI dimension
# and store the fitted model to disk
run_bayesian_model <- function(variable, data) {
  
  fit <- glmer(
    formula = formula_mod,
    family  = binomial(link = "logit"),
    data    = data[[variable]]
  )
  
  saveRDS(
    fit,
    file = paste0(
      "Modelo_bayes_HxA_Final/COL/Data/Modelo/fit_",
      variable,
      ".rds"
    )
  )
}


################################################################################
## Model estimation loop
################################################################################

# Fit and store one model per MPI dimension
for (variable in nbi_hogar) {
  cat(variable, "\n")
  run_bayesian_model(variable, encuesta_df)
}


################################################################################
## Prediction and benchmarking diagnostics
################################################################################

# Merge microdata with area-level predictors for prediction
new_encuenta <-
  inner_join(
    encuesta_ipm,
    statelevel_predictors_df,
    by = join_by(dam, dam2)
  )

# Base path for stored models
ruta_base_modelos <- "Modelo_bayes_HxA_Final/COL/Data/Modelo/"

# Container for comparison results
resultados <- data.frame(
  variable  = character(),
  pred_sum  = numeric(),
  obs_sum   = numeric(),
  pred_mean = numeric(),
  obs_mean  = numeric(),
  stringsAsFactors = FALSE
)


################################################################################
## Compare predicted vs observed totals and means
################################################################################

for (varii in nbi_hogar) {
  
  # Load fitted model
  modelo <- readRDS(
    file.path(ruta_base_modelos, paste0("fit_", varii, ".rds"))
  )
  
  # Predicted probabilities
  pred <- predict(modelo, type = "response", newdata = new_encuenta)
  
  # Observed outcomes
  obs <- as.numeric(new_encuenta[[varii]])
  
  # Survey expansion factors
  pesos <- as.numeric(encuesta_ipm$fep)
  
  # Store benchmarking statistics
  resultados <- rbind(
    resultados,
    data.frame(
      variable  = varii,
      pred_sum  = sum(pred * pesos),
      obs_sum   = sum(obs * pesos),
      pred_mean = weighted.mean(pred, pesos),
      obs_mean  = weighted.mean(obs, pesos)
    )
  )
}


################################################################################
## Output
################################################################################

# Display comparison between model-based and direct estimates
resultados
