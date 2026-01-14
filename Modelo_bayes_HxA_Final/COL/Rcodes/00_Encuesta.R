#########################################################
# Project: Multidimensional Poverty Index (MPI / IPM)
# Title  : An unified approach for small area estimation
#          of the Multidimensional Poverty Index:
#          An application to the Latin American Region
#
# Purpose:
# This script performs data loading, harmonization, and
# preparation of household survey microdata required for
# Small Area Estimation (SAE) of the Multidimensional
# Poverty Index (MPI/IPM).
#
# The resulting dataset is used as input for hierarchical
# and model-based SAE approaches described in the
# associated publication.
#
#
# Institution:
# United Nations Economic Commission for Latin America
# and the Caribbean (ECLAC / CEPAL)
#
# Notes:
# - This script focuses exclusively on data preparation.
# - No estimation or modeling is performed here.
# - All steps are fully reproducible.
#########################################################


############################
### Cleaning R environment ###
############################

# Remove all existing objects to ensure a clean session
rm(list = ls())

# Force garbage collection to free memory
gc()


#################
### Libraries ###
#################

# Core data manipulation
library(dplyr)
library(data.table)
library(magrittr)
library(stringr)

# Survey and SAE-related packages
library(survey)
library(srvyr)
library(sae)
library(lme4)

# Data import/export
library(haven)
library(openxlsx)

# Formula handling and utilities
library(formula.tools)
library(fastDummies)
library(remotes)

# Explicitly bind dplyr::select to avoid conflicts
select <- dplyr::select


############################################################
### Memory management                                    ###
############################################################

# Increase memory limit to handle large survey microdata
# (relevant for Windows environments)
memory.limit(250000000)


################################################################################
### Loading datasets                                                          ###
################################################################################

# ---------------------------------------------------------------------------
# Household and individual survey microdata (GEIH 2018)
# ---------------------------------------------------------------------------

# Main household-individual survey file
encuesta_sta <- read_dta(
  "Modelo_bayes_HxA_Final/COL/Data/Encuestas/COL_2018N1.dta"
)

# Create a temporary ordering variable to preserve record order
encuesta_sta %<>%
  mutate(
    orden_temp = str_pad(
      string = 1:n(),
      width  = 7,
      pad    = "0"
    )
  )

# Complementary survey file with household identifiers
encuesta_comp <- read_dta(
  "Modelo_bayes_HxA_Final/COL/Data/Encuestas/col18n1.dta"
)

# MPI/IPM deprivation indicators
encuesta_ipm <- read_dta(
  "Modelo_bayes_HxA_Final/COL/Data/Encuestas/COL2018IPM.dta"
)

# Geographic auxiliary information (Primary Sampling Units)
upms <- readRDS(
  file = "Modelo_bayes_HxA_Final/COL/Data/Encuestas/upm_dpto_2018.rds"
)


# ---------------------------------------------------------------------------
# Merge survey data with geographic identifiers
# ---------------------------------------------------------------------------

encuesta_comp %<>%
  left_join(
    upms,
    by = c(
      "directorio" = "DIRECTORIO",
      "secuencia_p" = "SECUENCIA_P",
      "orden"
    )
  )

# Extract municipal identifier from segment code
encuesta_comp$mpio <- substr(encuesta_comp$segmento, 8, 12)


# ---------------------------------------------------------------------------
# Construct unified survey dataset
# ---------------------------------------------------------------------------

encuesta <-
  encuesta_comp %>%
  select(id_hogar, id_pers, upm, estrato, mpio) %>%
  inner_join(encuesta_sta) %>%
  select(
    id_hogar:mpio,
    `_fep`,
    area_ee,
    sexo,
    etnia_ee,
    anoest,
    edad
  ) %>%
  inner_join(
    encuesta_ipm %>%
      select(id_hogar, id_pers, matches("^nbi|^g0|^or"))
  )

# Store variable names for validation or auditing
temp <- names(encuesta)


################################################################################
### MPI: Definition of household deprivation indicators (GEIH)              ###
################################################################################

# List of household-level NBI deprivation indicators
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

# Convert deprivation indicators to character
# (required for post-stratification and modeling consistency)
encuesta <- encuesta %>%
  mutate_at(vars(nbi_hogar), as.character)


################################################################################
### Creation of post-stratification variables                                ###
################################################################################

# This step defines demographic and socioeconomic strata
# used in SAE models (e.g., age, education, ethnicity, area)

encuesta_nbi <- encuesta %>%
  transmute(
    
    # Administrative domains
    dam  = str_sub(mpio, 1, 2),
    dam2 = mpio,
    
    # Identifiers
    id_hogar,
    
    # Household deprivation indicators
    nbi_hnolee_ee,
    nbi_hlogroeduc_ee,
    nbi_heducninios,
    nbi_hhacina,
    nbi_henergia,
    nbi_htic,
    nbi_hagua_ee,
    nbi_hsaneamiento_ee,
    nbi_hsalud_ee,
    nbi_hpartemp,
    nbi_hempe,
    nbi_hjub,
    
    # Area (urban / rural)
    area = case_when(
      area_ee == 1 ~ "1",
      TRUE ~ "0"
    ),
    
    # Sex
    sexo = as.character(sexo),
    
    # Ethnicity
    etnia = case_when(
      etnia_ee == 1 ~ "1",  # Indigenous
      etnia_ee == 2 ~ "2",  # Afro-descendant
      TRUE ~ "3"            # Other
    ),
    
    # Educational attainment (harmonized categories)
    anoest = case_when(
      edad < 5 | is.na(anoest) ~ "98",  # Not applicable
      anoest == 99             ~ "99",  # NS/NR
      anoest == 0              ~ "1",   # No education
      anoest %in% 1:6          ~ "2",   # Primary
      anoest %in% 7:12         ~ "3",   # Secondary
      anoest > 12              ~ "4",   # Tertiary
      TRUE                     ~ "Error"
    ),
    
    # Age groups
    edad = case_when(
      edad <= 15 ~ "1",
      edad < 30  ~ "2",
      edad < 45  ~ "3",
      edad < 65  ~ "4",
      edad >= 65 ~ "5"
    ),
    
    # Expansion factor
    fep = `_fep`
  )


################################################################################
### Output                                                                   ###
################################################################################

# Save the processed dataset for SAE modeling
saveRDS(
  encuesta_nbi,
  file = "Modelo_bayes_HxA_Final/COL/Data/Encuestas/encuesta_nbi.rds"
)
