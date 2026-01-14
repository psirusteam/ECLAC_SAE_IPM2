################################################################################
## Project: Multidimensional Poverty Index (MPI / IPM) – HxA Approach
##
## Objective:
## This script implements the final stage of MPI estimation under a
## Small Area Estimation (SAE) framework. It combines:
##  - Posterior predictive simulations of deprivation indicators (dummy draws)
##  - Post-stratified census information
##  - Aggregation of MPI and its components (H, A, MPI)
##
## Outputs include national and subnational estimates, multiple demographic
## and geographic disaggregations, and associated uncertainty measures.
##
##
## Institution:
## United Nations Economic Commission for Latin America and the Caribbean
## (ECLAC / CEPAL)
################################################################################


################################################################################
## Libraries
################################################################################

# Clean environment to ensure reproducibility
rm(list = ls())

# Data manipulation and functional programming
library(tidyverse)
library(magrittr)
library(furrr)

# Bayesian infrastructure and multilevel models
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)

# Visualization and export utilities
library(patchwork)
library(openxlsx)

# Project-specific helper functions
source("Modelo_bayes_HxA_Final/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA_Final/0funciones/agregado_dim_ipm.r")
select <- dplyr::select

################################################################################
## Load post-stratified census data
################################################################################

# Census data aggregated by post-stratification cells
# (output from previous pipeline stages)
censo_ipm <- readRDS(
  "Modelo_bayes_HxA_Final/COL/Data/Censo/censo_COL_recor.rds"
)


################################################################################
## Load posterior predictive dummy matrices
################################################################################

# Each matrix contains Bernoulliulli (0/1) simulations by census cell
# for a specific MPI deprivation dimension

epred_mat_dummy_hnolee        <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hnolee_ee.rds")
epred_mat_dummy_hlogroeduc    <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hlogroeduc_ee.rds")
epred_mat_dummy_heducninios   <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_heducninios.rds")
epred_mat_dummy_hhacina       <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hhacina.rds")
epred_mat_dummy_henergia      <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_henergia.rds")
epred_mat_dummy_htic          <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_htic.rds")
epred_mat_dummy_hagua         <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hagua_ee.rds")
epred_mat_dummy_hsaneamiento  <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hsaneamiento_ee.rds")
epred_mat_dummy_hsalud        <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hsalud_ee.rds")
epred_mat_dummy_hpartemp      <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hpartemp.rds")
epred_mat_dummy_hempe         <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hempe.rds")
epred_mat_dummy_hjub          <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Modelo/epred_mat_dummy_nbi_hjub.rds")


################################################################################
## Construction of the MPI posterior chain
################################################################################

# Continuous MPI score: average of the 12 deprivation dimensions
chain_q <-
  (1 / 12) * (
    epred_mat_dummy_hnolee +
      epred_mat_dummy_hlogroeduc +
      epred_mat_dummy_heducninios +
      epred_mat_dummy_hhacina +
      epred_mat_dummy_henergia +
      epred_mat_dummy_htic +
      epred_mat_dummy_hagua +
      epred_mat_dummy_hsaneamiento +
      epred_mat_dummy_hsalud +
      epred_mat_dummy_hpartemp +
      epred_mat_dummy_hempe +
      epred_mat_dummy_hjub
  )

# Identification of multidimensional poverty (k = 0.4)
chain_Ind <- chain_q
chain_Ind[chain_Ind <= 0.4] <- 0
chain_Ind[chain_Ind >  0.4] <- 1

# Intensity conditional on being poor
chain_ci <- matrix(0, nrow = nrow(chain_q), ncol = ncol(chain_q))
chain_ci[chain_Ind == 1] <- chain_q[chain_Ind == 1]


################################################################################
## Aggregation using census expansion factors
################################################################################

# MPI numerator
numIPM <- chain_ci %>%
  as.data.frame() %>%
  mutate_all(~ . * censo_ipm$n) %>%
  as.matrix()

# Poor population counts
chain_N <- chain_Ind %>%
  as.data.frame() %>%
  mutate_all(~ . * censo_ipm$n) %>%
  as.matrix()

# Aggregated indicators by simulation
IPM_l <- colSums(numIPM) / sum(censo_ipm$n)
Nz_l  <- colSums(chain_N)
H_l   <- Nz_l / sum(censo_ipm$n)
A_l   <- colSums(numIPM) / Nz_l

# Simulation-level summary
datos_chain <- data.frame(IPM_l, H_l, A_l, HA_l = H_l * A_l)
rownames(datos_chain) <- paste0("l = ", seq_len(ncol(chain_q)))
datos_chain

# Posterior summaries
data.frame(
  H      = mean(H_l),
  H_sd   = sd(H_l),
  A      = mean(A_l),
  A_sd   = sd(A_l),
  IPM    = mean(IPM_l),
  IPM_sd = sd(IPM_l)
)


################################################################################
## Aggregated MPI estimates
################################################################################

# National level
estimacion_Nacional <-
  estime_IPM(
    poststrat = censo_ipm,
    chain_ci  = t(chain_ci),
    chain_ind = t(chain_Ind),
    byMap     = NULL
  ) %>% data.frame()

# DAM level
estimacion_dam <-
  estime_IPM(
    poststrat = censo_ipm,
    chain_ci  = t(chain_ci),
    chain_ind = t(chain_Ind),
    byMap     = "dam"
  ) %>% data.frame()

# Municipality (DAM2) level
estimacion_dam2 <-
  estime_IPM(
    poststrat = censo_ipm,
    chain_ci  = t(chain_ci),
    chain_ind = t(chain_Ind),
    byMap     = "dam2"
  ) %>% data.frame()


################################################################################
## MPI dimension-specific results
################################################################################

# List of dummy matrices by dimension
epred_mat_dim <- list(
  nbi_hnolee        = epred_mat_dummy_hnolee,
  nbi_hlogroeduc    = epred_mat_dummy_hlogroeduc,
  nbi_heducninios   = epred_mat_dummy_heducninios,
  nbi_hhacina       = epred_mat_dummy_hhacina,
  nbi_henergia      = epred_mat_dummy_henergia,
  nbi_htic          = epred_mat_dummy_htic,
  nbi_hagua         = epred_mat_dummy_hagua,
  nbi_hsaneamiento  = epred_mat_dummy_hsaneamiento,
  nbi_hsalud        = epred_mat_dummy_hsalud,
  nbi_hpartemp      = epred_mat_dummy_hpartemp,
  nbi_hempe         = epred_mat_dummy_hempe,
  nbi_hjub          = epred_mat_dummy_hjub
)


################################################################################
## Helper function: aggregation by MPI dimension
################################################################################

aux_agregado <- function(dat, byx = NULL, censo) {
  
  temp_estimate <- map_df(
    dat,
    function(dummy) {
      agregado_dim_ipm(
        poststrat = censo,
        epredmat  = t(dummy),
        byMap     = byx
      )
    },
    .id = "Indicator"
  )
  
  inner_join(
    spread(temp_estimate %>% select(-estimate_se),
           key = "Indicator",
           value = "estimate"),
    spread(temp_estimate %>% select(-estimate),
           key = "Indicator",
           value = "estimate_se") %>%
      rename_if(is.numeric, ~ paste0(.x, "_se"))
  )
}


################################################################################
## Simple and crossed disaggregations
################################################################################

by_agrega <- c("dam", "dam2", "area", "sexo", "edad", "etnia", "anoest")

estimado_ipm1 <- map(by_agrega, function(xby) {
  
  paso_ipm <- estime_IPM(
    poststrat = censo_ipm,
    chain_ci  = t(chain_ci),
    chain_ind = t(chain_Ind),
    byMap     = xby
  ) %>% data.frame()
  
  paso_dim <- aux_agregado(epred_mat_dim, xby, censo_ipm)
  
  inner_join(paso_dim, paso_ipm)
})

names(estimado_ipm1) <- by_agrega


by_agrega2 <- t(combn(by_agrega[-c(1:2)], 2)) %>% cbind("dam")

estimado_ipm2 <- map(seq_len(nrow(by_agrega2)), function(ii) {
  
  paso_ipm <- estime_IPM(
    poststrat = censo_ipm,
    chain_ci  = t(chain_ci),
    chain_ind = t(chain_Ind),
    byMap     = by_agrega2[ii, ]
  ) %>% data.frame()
  
  paso_dim <- aux_agregado(
    epred_mat_dim,
    by_agrega2[ii, ],
    censo_ipm
  )
  
  inner_join(paso_dim, paso_ipm)
})

names(estimado_ipm2) <- apply(by_agrega2, 1, paste0, collapse = "_")

estimado_ipm <- c(estimado_ipm1, estimado_ipm2)

saveRDS(
  estimado_ipm,
  file = "Modelo_bayes_HxA_Final/COL/Data/Modelo/estimado_ipm_HA_freq.rds"
)


################################################################################
## Export results to Excel
################################################################################

wb <- createWorkbook()
hojas <- names(estimado_ipm)

# Index sheet
addWorksheet(wb, "Index")

writeDataTable(
  wb,
  data.frame(Order = seq_along(hojas), Title = NA),
  sheet = "Index",
  tableStyle = "TableStyleLight9"
)

# Results by sheet
for (ii in seq_along(hojas)) {
  
  addWorksheet(wb, hojas[ii])
  writeData(wb, estimado_ipm[[hojas[ii]]], hojas[ii])
  
  writeFormula(
    wb,
    "Index",
    startRow = 1 + ii,
    startCol = 2,
    x = makeHyperlinkString(
      sheet = hojas[ii],
      row = 1,
      col = 2,
      text = hojas[ii]
    )
  )
}

saveWorkbook(
  wb,
  "Modelo_bayes_HxA_Final/COL/Output/estimacion_ipm.xlsx",
  overwrite = TRUE
)


################################################################################
## Dimension-specific estimates at municipal level
################################################################################

temp_estimate_mpio <- map_df(
  epred_mat_dim,
  function(dummy) {
    agregado_dim_ipm(
      poststrat = censo_ipm,
      epredmat  = dummy,
      byMap     = "dam2"
    )
  },
  .id = "Indicator"
)

saveRDS(
  temp_estimate_mpio,
  "Modelo_bayes_HxA_Final/COL/Data/Modelo/temp_estimate_mpio.rds"
)
