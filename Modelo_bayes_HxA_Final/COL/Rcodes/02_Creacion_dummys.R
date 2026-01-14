################################################################################
## Project: Multidimensional Poverty Index (MPI / IPM) – HxA Framework
##
## Purpose:
## This script generates posterior predictive binary outcomes for each
## MPI deprivation indicator using fitted multilevel binomial models.
##
## The procedure applies the models to post-stratified census data in order
## to:
##  - Propagate model uncertainty
##  - Generate simulated deprivation microdata
##  - Enable aggregation and uncertainty estimation at small-area level
##
# Authors:
# - Andrés Gutiérrez Rojas (ECLAC)
# - Alejandra María Arias Salazar
# - Xavier Mancero (ECLAC)
# - Stalyn Guerrero (ECLAC)
#
#
## Institution:
## United Nations Economic Commission for Latin America and the Caribbean
## (ECLAC / CEPAL)
################################################################################


################################################################################
## Libraries
################################################################################

# Clean environment to ensure reproducibility
rm(list = ls())

# Core data manipulation and functional programming
library(tidyverse)
library(magrittr)
library(furrr)

# Bayesian and mixed-effects modeling infrastructure
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)

# Visualization utilities (used downstream)
library(patchwork)


################################################################################
## Function: Posterior predictive binary simulation
################################################################################

# This function:
# 1. Loads a previously fitted multilevel binomial model
# 2. Obtains predicted probabilities for post-stratification cells
# 3. Simulates binary outcomes via Bernoulli sampling
# 4. Stores the simulated draws as a matrix (N × nsim)
#
# Args:
# - variable     : Name of the MPI deprivation indicator
# - new_encuenta : Post-stratification data (census × covariates)
# - nsim         : Number of posterior predictive simulations
#
# Output:
# - A matrix of simulated binary outcomes saved as an .rds file

crear_epred_mat_dummy <- function(variable, new_encuenta, nsim = 10) {
  
  # Path to fitted model
  modelo_rds <- paste0(
    "Modelo_bayes_HxA_Final/COL/Data/Modelo/fit_",
    variable,
    ".rds"
  )
  
  # Output path for posterior predictive simulations
  ruta_guardado <- paste0(
    "Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_",
    variable,
    ".rds"
  )
  
  # Load fitted model
  modelo <- readRDS(file = modelo_rds)
  
  # Predict probabilities for post-stratification cells
  # allow.new.levels = TRUE is required for census-only combinations
  prob <- predict(
    modelo,
    type = "response",
    newdata = new_encuenta,
    allow.new.levels = TRUE
  )
  
  # Number of post-stratification cells
  N <- length(prob)
  
  # Initialize matrix for simulated binary outcomes
  epred_mat_dummy <- matrix(
    NA_integer_,
    nrow = N,
    ncol = nsim
  )
  
  # Generate Bernoulli simulations
  for (i in seq_len(nsim)) {
    epred_mat_dummy[, i] <- rbinom(
      n    = N,
      size = 1,
      prob = prob
    )
  }
  
  # Assign iteration labels
  colnames(epred_mat_dummy) <- paste0("iter_", seq_len(nsim))
  
  # Save posterior predictive matrix
  saveRDS(epred_mat_dummy, file = ruta_guardado)
  
  # Log output path
  cat(ruta_guardado, "\n")
}


################################################################################
## Load and prepare census post-stratification data
################################################################################

# Load census microdata aggregated by post-stratification cells
censo_ipm <- readRDS(
  "Modelo_bayes_HxA_Final/COL/Data/Censo/censo_COL.rds"
) %>%
  
  # Harmonize identifiers with survey/model notation
  rename(
    dam  = depto,
    dam2 = mpio
  ) %>%
  
  # Aggregate counts by demographic and geographic strata
  group_by(dam, dam2, area, sexo, edad, etnia, anoest) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )


################################################################################
## Load area-level auxiliary predictors
################################################################################

statelevel_predictors_df <-
  readRDS(
    "Modelo_bayes_HxA_Final/COL/Data/statelevel_predictors_df_dam2.rds"
  )


################################################################################
## Construct post-stratification frame
################################################################################

# Merge census cells with area-level covariates
poststrat_df <- left_join(
  censo_ipm,
  statelevel_predictors_df,
  by = c("dam", "dam2")
) %>%
  
  # Remove cells with missing key covariates
  filter(!is.na(edad4))


# Retain original census structure
censo_ipm2 <- poststrat_df %>%
  dplyr::select(names(censo_ipm))

# Store cleaned post-stratification census
saveRDS(
  object = censo_ipm2,
  file   = "Modelo_bayes_HxA_Final/COL/Data/Censo/censo_COL_recor.rds"
)


################################################################################
## MPI deprivation indicators to simulate
################################################################################

# List of household-level MPI deprivation indicators
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
  "nbi_hjub"
)


################################################################################
## Generate posterior predictive simulations
################################################################################

# For each MPI indicator:
# - Load fitted model
# - Predict probabilities on census cells
# - Simulate binary outcomes
# - Store simulation matrices

crear_epred_mat_dummy(variable = nbi_hogar[1],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[2],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[3],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[4],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[5],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[6],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[7],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[8],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[9],  new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[10], new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[11], new_encuenta = poststrat_df)
crear_epred_mat_dummy(variable = nbi_hogar[12], new_encuenta = poststrat_df)
