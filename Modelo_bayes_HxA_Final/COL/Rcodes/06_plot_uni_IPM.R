################################################################################
## Project: Multidimensional Poverty Index (MPI) – HxA Approach
##
## Purpose of this script section:
## This final block performs validation, comparison, and visualization of MPI
## estimates obtained from:
##
##  - Direct survey-based estimation (sample)
##  - Model-based SAE estimation calibrated to census totals (model / census)
##
## The objective is to:
##  1. Compare national, departmental (DAM), and municipal results
##  2. Visualize MPI components (H, A, M0) and dimensional contributions
##  3. Quantify absolute and relative differences between sample and model
##  4. Produce publication-ready figures and tables for reporting
##
################################################################################


################################################################################
## Libraries and environment
################################################################################

# Clean environment to avoid object contamination
rm(list = ls())

# Core packages for data manipulation, modeling, and visualization
library(tidyverse)
library(magrittr)
library(furrr)
library(parallel)

# Bayesian and multilevel infrastructure
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)

# Survey and SAE utilities
library(survey)
library(srvyr)

# Export utilities
library(openxlsx)

# Avoid conflicts with select()
select <- dplyr::select

# Project-specific helper functions
source("Modelo_bayes_HxA_Final/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA_Final/0funciones/agregado_dim_ipm.r")
source("Modelo_bayes_HxA_Final/0funciones/IPM_descomponer.R")


################################################################################
## Data input: census and survey
################################################################################

# Post-stratified census dataset
censo_ipm <- readRDS("Modelo_bayes_HxA_Final/COL/Data/Censo/censo_COL_recor.rds")

# Survey dataset for direct estimation
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Final/COL/Data/Encuestas/encuesta_nbi.rds") %>%
  filter(edad > 1, anoest < 6) %>%
  mutate_at(vars(matches("nbi")), as.numeric)


################################################################################
## Definition of MPI weights (equal weights across dimensions)
################################################################################

vars_nbi_hogar <-
  c(
    "nbi_hnolee_ee"       = 1/12,
    "nbi_hlogroeduc_ee"   = 1/12,
    "nbi_heducninios"     = 1/12,
    "nbi_hhacina"         = 1/12,
    "nbi_henergia"        = 1/12,
    "nbi_htic"            = 1/12,
    "nbi_hagua_ee"        = 1/12,
    "nbi_hsaneamiento_ee" = 1/12,
    "nbi_hsalud_ee"       = 1/12,
    "nbi_hpartemp"        = 1/12,
    "nbi_hempe"           = 1/12,
    "nbi_hjub"            = 1/12
  )


################################################################################
## Direct (survey-based) MPI estimation
################################################################################

# National-level direct estimates
resul_nacional <- calculate_ipm_sample(
  k = 40,
  set_data = encuesta_ipm,
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep"
)

# Department-level direct estimates
resul_dam <- calculate_ipm_by(
  group_var = "dam",
  k = 40,
  set_data = encuesta_ipm,
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep"
)

# Area-level direct estimates
resul_area <- calculate_ipm_by(
  group_var = "area",
  k = 40,
  set_data = encuesta_ipm,
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep"
)


################################################################################
## NATIONAL LEVEL: comparison between sample and model-based estimates
################################################################################

# Load model-based (census-calibrated) national results
ipm_HA_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.xlsx")

contribuciones_censo <-
  openxlsx::read.xlsx(
    "Modelo_bayes_HxA_Final/COL/Output/contribuciones_nacional.xlsx",
    sheet = "contribuciones"
  )

# Prepare survey-based MPI components for comparison
tab_dir <- resul_nacional$ipm_HA %>%
  select(H, A, M0, matches("_low|_upp")) %>%
  pivot_longer(everything(), names_to = "elemento", values_to = "value") %>%
  filter(value < 1) %>%
  separate(elemento, into = c("elemento", "tipo"), sep = "_") %>%
  mutate(tipo = ifelse(is.na(tipo), "estimate", tipo)) %>%
  pivot_wider(names_from = tipo, values_from = value) %>%
  mutate(tipo = "Sample")

# Prepare model-based MPI components
tab_censo <- ipm_HA_censo %>%
  select(matches("media")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "elemento",
    values_to = "estimate"
  ) %>%
  separate(elemento, into = c("elemento", "tipo"), sep = "_") %>%
  mutate(tipo = "Model")

# Visualization: MPI components (H, A, M0)
p1 <- ggplot(tab_censo, aes(x = elemento, y = estimate, color = tipo)) +
  geom_point(size = 3) +
  geom_jitter(
    data = tab_dir,
    aes(x = elemento, y = estimate, color = tipo),
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "National MPI components: Sample vs Model",
    x = "Component",
    y = "Estimate",
    color = ""
  )


################################################################################
## NATIONAL LEVEL: dimensional contributions
################################################################################

# Survey-based contributions
tab_dir <- resul_nacional$contribuciones %>%
  data.frame() %>%
  separate(nbi, into = c("type", "nbi"), sep = "_") %>%
  mutate(tipo = "Sample") %>%
  rename(estimate = estimado)

# Model-based contributions
tab_censo <- contribuciones_censo %>%
  select(matches("media")) %>%
  pivot_longer(cols = everything(), names_to = "nbi", values_to = "estimate") %>%
  separate(nbi, into = c("nbi", "tipo"), sep = "_") %>%
  mutate(tipo = "Model")

tab_plot <- bind_rows(tab_dir, tab_censo)

# Visualization: dimensional contributions
p2 <- ggplot(tab_plot, aes(x = nbi, y = estimate, color = tipo)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "National MPI dimensional contributions",
    x = "Dimension",
    y = "Contribution",
    color = ""
  )

# Save national-level plots
ggsave(
  p1,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/Nacional_HA.jpeg",
  width = 18,
  height = 14
)

ggsave(
  p2,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/Nacional_indicadores.png",
  width = 18,
  height = 14
)


################################################################################
## DEPARTMENT LEVEL (DAM): comparison and visualization
################################################################################

# Load model-based department-level results
ipm_HA_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.xlsx")

contribuciones_censo <-
  openxlsx::read.xlsx(
    "Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.xlsx",
    sheet = "contribuciones"
  )

# Prepare survey-based MPI components by department
tab_dir <- resul_dam %>%
  map_df(~ .x$ipm_HA, .id = "dam") %>%
  select(dam, H, A, M0, matches("_low|_upp")) %>%
  pivot_longer(
    cols = c(H, A, M0, matches("_low|_upp")),
    names_to = "elemento",
    values_to = "value"
  ) %>%
  filter(value < 1) %>%
  separate(elemento, into = c("elemento", "tipo"), sep = "_") %>%
  mutate(tipo = ifelse(is.na(tipo), "estimate", tipo)) %>%
  pivot_wider(names_from = tipo, values_from = value) %>%
  mutate(tipo = "Sample")

# Prepare model-based MPI components by department
tab_censo <- ipm_HA_censo %>%
  select(dam, matches("media")) %>%
  pivot_longer(
    cols = c("H_media", "A_media", "M0_media"),
    names_to = "elemento",
    values_to = "estimate"
  ) %>%
  separate(elemento, into = c("elemento", "tipo"), sep = "_") %>%
  mutate(tipo = "Model")

# Visualization: MPI components by department
p_dam <- ggplot(tab_censo, aes(x = dam, y = estimate, color = tipo)) +
  geom_point(size = 3) +
  geom_jitter(
    data = tab_dir,
    aes(x = dam, y = estimate, color = tipo),
    width = 0.2
  ) +
  theme_bw(15) +
  facet_grid(elemento ~ ., scales = "free_y") +
  labs(
    title = "Department-level MPI components",
    x = "Major administrative division (DAM)",
    y = "Estimate",
    color = ""
  )

# Save department-level MPI plot
ggsave(
  p_dam,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/ipm_dam.jpeg",
  width = 20,
  height = 14
)


################################################################################
## DEPARTMENT-LEVEL (DAM) DIMENSIONAL CONTRIBUTIONS
##
## Purpose:
## This section compares MPI dimensional contributions at the department (DAM)
## level between:
##   - Direct survey-based estimates ("sample")
##   - Model-based SAE estimates calibrated to census totals ("censo"/"model")
##
## Outputs include:
##   - Dimension-specific scatter plots by DAM
##   - Stacked contribution profiles (composition)
##   - Absolute and relative difference tables between sample and model
################################################################################


################################################################################
## Prepare survey-based dimensional contributions by DAM
################################################################################

# Extract dimensional contributions from direct estimates for each DAM
# Each element of resul_dam contains a list with a 'contribuciones' data frame
tab_dir <- resul_dam %>%
  map_dfr(~ .x$contribuciones, .id = "dam") %>%
  # Split original variable name into prefix and indicator name
  separate(col = "nbi", into = c("tipo", "nbi"), sep = "_") %>%
  # Label as survey-based estimates
  mutate(tipo = "sample") %>%
  # Standardize column name for downstream processing
  rename(estimacion = estimado)


################################################################################
## Prepare model-based (census-calibrated) dimensional contributions by DAM
################################################################################

# Reshape model-based contributions from wide to long format
tab_censo <- contribuciones_censo %>%
  select(dam, matches("media")) %>%
  pivot_longer(
    cols = matches("media"),
    names_to = "nbi",
    values_to = "estimacion"
  ) %>%
  # Separate indicator name from summary statistic suffix
  separate(col = "nbi", into = c("nbi", "tipo"), sep = "_") %>%
  # Label as model-based estimates
  mutate(tipo = "censo")


################################################################################
## Combine sample and model contributions into a single comparison table
################################################################################

tab_plot <- bind_rows(tab_dir, tab_censo)


################################################################################
## Dimension-level scatter plots by DAM (grouped by thematic blocks)
##
## Each plot compares sample vs model estimates across DAMs
## Faceting is used to improve readability and scale comparability
################################################################################

# Education-related indicators
p21 <- ggplot(
  tab_plot %>% filter(nbi %in% c("hnolee", "hlogroeduc", "heducninios")),
  aes(x = dam, y = estimacion, color = tipo)
) +
  geom_point(size = 3) +
  theme_bw(base_size = 15) +
  facet_grid(nbi ~ ., scales = "free_y") +
  labs(
    title = "Contribution",
    x = "DAM",
    y = "Estimation",
    color = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Housing and infrastructure indicators
p22 <- ggplot(
  tab_plot %>% filter(nbi %in% c("hhacina", "henergia", "htic")),
  aes(x = dam, y = estimacion, color = tipo)
) +
  geom_point(size = 3) +
  theme_bw(base_size = 15) +
  facet_grid(nbi ~ ., scales = "free_y") +
  labs(
    title = "Contribution",
    x = "DAM",
    y = "Estimation",
    color = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Water, sanitation, and health indicators
p23 <- ggplot(
  tab_plot %>% filter(nbi %in% c("hagua", "hsaneamiento", "hsalud")),
  aes(x = dam, y = estimacion, color = tipo)
) +
  geom_point(size = 3) +
  theme_bw(base_size = 15) +
  facet_grid(nbi ~ ., scales = "free_y") +
  labs(
    title = "Contribution",
    x = "DAM",
    y = "Estimation",
    color = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Labor market and social protection indicators
p24 <- ggplot(
  tab_plot %>% filter(nbi %in% c("hpartemp", "hempe", "hjub")),
  aes(x = dam, y = estimacion, color = tipo)
) +
  geom_point(size = 3) +
  theme_bw(base_size = 15) +
  facet_grid(nbi ~ ., scales = "free_y") +
  labs(
    title = "Contribution",
    x = "DAM",
    y = "Estimation",
    color = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
## Save dimension-level comparison plots
################################################################################

ggsave(p21,
       filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion1_dam.jpeg",
       width = 20, height = 14)

ggsave(p22,
       filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion2_dam.jpeg",
       width = 20, height = 14)

ggsave(p23,
       filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion3_dam.jpeg",
       width = 20, height = 14)

ggsave(p24,
       filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion4_dam.jpeg",
       width = 20, height = 14)


################################################################################
## Harmonize labels for presentation and reporting
################################################################################

# Human-readable labels for MPI indicators
var_names <- c(
  "hnolee"      = "Illiteracy",
  "hlogroeduc"  = "Educational attainment",
  "heducninios" = "Non-attendance or lag",
  "hhacina"     = "Overcrowding",
  "henergia"    = "Energy",
  "htic"        = "Internet access",
  "hagua"       = "Water",
  "hsaneamiento"= "Sanitation",
  "hsalud"      = "Health insurance",
  "hpartemp"    = "Labor market participation",
  "hempe"       = "Quality of employment",
  "hjub"        = "Pensions"
)

# Labels for estimation source
tipo_names <- c(
  "sample" = "Sample",
  "censo"  = "Model"
)

# Apply recoding
tab_plot <- tab_plot %>%
  mutate(
    nbi  = recode(nbi, !!!var_names),
    tipo = recode(tipo, !!!tipo_names)
  )


################################################################################
## Stacked contribution profiles by DAM (composition comparison)
##
## Purpose:
## To compare the *structure* of multidimensional poverty between
## sample-based and model-based estimates, independently of levels
################################################################################

p3 <- ggplot(
  tab_plot,
  aes(x = dam, y = estimacion, fill = nbi)
) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw(15) +
  facet_grid(tipo ~ .) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    fill = "",
    x = "Major administrative division (DAM)",
    y = ""
  )

ggsave(
  p3,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion_dam.jpeg",
  width = 18, height = 12
)


################################################################################
## Absolute and relative difference tables (sample vs model)
##
## These tables quantify discrepancies between estimation approaches
## and serve as formal validation diagnostics
################################################################################

# Load DAM names
dat_set <- read_sf("Modelo_bayes_HxA_Final/COL/Shape/COL_dam2.shp") %>%
  data.frame() %>%
  distinct(dam, dam_name) %>%
  mutate(dam_name = ifelse(dam == 11, "BOGOTÁ, D.C.", dam_name))

# Reshape comparison table
temp <- tab_plot %>%
  select(dam, nbi, tipo, estimacion) %>%
  pivot_wider(
    names_from = tipo,
    values_from = estimacion,
    values_fill = list(estimacion = 0)
  ) %>%
  inner_join(dat_set)

# Model-only predictions (for reference)
tab_2 <- temp %>%
  filter(Sample == 0) %>%
  select(-Sample, -dam) %>%
  pivot_wider(
    names_from = dam_name,
    values_from = Model,
    values_fill = list(Model = 0)
  )

# Absolute differences
tab_1 <- temp %>%
  filter(Sample > 0) %>%
  transmute(
    dam_name,
    nbi,
    diff_abs = abs(Sample - Model)
  ) %>%
  pivot_wider(
    names_from = nbi,
    values_from = diff_abs
  )

# Relative differences
tab_11 <- temp %>%
  filter(Sample > 0) %>%
  transmute(
    dam_name,
    nbi,
    diff_rel = abs(Sample - Model) / Sample
  ) %>%
  pivot_wider(
    names_from = nbi,
    values_from = diff_rel
  )


################################################################################
## Export validation tables to Excel
################################################################################

openxlsx::write.xlsx(
  list(
    tab_dif_abs      = tab_1,
    tab_dif_relativa = tab_11,
    tab_pred_dam     = tab_2
  ),
  file = "Modelo_bayes_HxA_Final/COL/Doc/01_tablas/00_tablas_dam.xlsx"
)

openxlsx::openXL(
  file = "Modelo_bayes_HxA_Final/COL/Doc/01_tablas/00_tablas_dam.xlsx"
)

################################################################################
## MUNICIPAL-LEVEL (DAM2 / MPIO) ANALYSIS – SELECTED DEPARTMENT
##
## Purpose:
## This section analyzes MPI dimensional contributions at the municipal level
## (DAM2) for a selected department (id_dam).
##
## The objective is to:
##  - Compare model-based MPI contributions across municipalities
##  - Include the department total as a reference benchmark (MAD)
##  - Assess internal heterogeneity and structural composition of poverty
##
## NOTE:
## At the municipal level, only model-based (SAE) estimates are used,
## since direct survey estimates are not reliable at this scale.
################################################################################


################################################################################
## Define department of interest and prepare spatial reference
################################################################################

# Selected department code
id_dam <- "81"

# Extract municipalities belonging to the selected department
# and define plotting order and grouping labels
dat_set <- read_sf("Modelo_bayes_HxA_Final/COL/Shape/COL_dam2.shp") %>%
  data.frame() %>%
  filter(dam == id_dam) %>%
  select(dam, dam2, dam2_name) %>%
  mutate(
    orden = 1:n(),
    mpios = "Minor administrative divisions"
  )


################################################################################
## Load model-based MPI contributions
################################################################################

# Municipal-level model-based contributions
contribuciones_censo <-
  openxlsx::read.xlsx(
    "Modelo_bayes_HxA_Final/COL/Output/contribuciones_mpios.xlsx",
    sheet = "contribuciones"
  )

# Department-level model-based contributions (used as benchmark)
contribuciones_censo_dam <-
  openxlsx::read.xlsx(
    "Modelo_bayes_HxA_Final/COL/Output/contribuciones_dam.xlsx",
    sheet = "contribuciones"
  ) %>%
  filter(dam == id_dam)


################################################################################
## Combine DAM and DAM2 contributions into a unified structure
################################################################################

# Append department total to municipal table
# This allows direct comparison between each municipality and the DAM aggregate
contribuciones_censo2 <- bind_rows(
  contribuciones_censo_dam,
  contribuciones_censo
) %>%
  mutate(
    dam2 = ifelse(is.na(dam2), dam, dam2),
    dam  = str_sub(dam2, 1, 2)
  ) %>%
  filter(dam == id_dam)


################################################################################
## Reshape contributions to long format for plotting
################################################################################

tab_censo <- contribuciones_censo2 %>%
  select(dam2, matches("media")) %>%
  pivot_longer(
    cols = matches("media"),
    names_to = "nbi",
    values_to = "estimacion"
  ) %>%
  separate(
    col = "nbi",
    into = c("nbi", "tipo"),
    sep = "_"
  ) %>%
  # Explicitly label as model-based estimates
  mutate(tipo = "Model")

tab_plot <- tab_censo


################################################################################
## Recode MPI indicators to human-readable labels
################################################################################

var_names <- c(
  "hnolee"      = "Illiteracy",
  "hlogroeduc"  = "Educational attainment",
  "heducninios" = "Non-attendance or lag",
  "hhacina"     = "Overcrowding",
  "henergia"    = "Energy",
  "htic"        = "Internet access",
  "hagua"       = "Water",
  "hsaneamiento"= "Sanitation",
  "hsalud"      = "Health insurance",
  "hpartemp"    = "Labor market participation",
  "hempe"       = "Quality of employment",
  "hjub"        = "Pensions"
)

tab_plot <- tab_plot %>%
  mutate(nbi = recode(nbi, !!!var_names))


################################################################################
## Add department aggregate (MAD) as a separate comparison group
################################################################################

tab_plot <- tab_plot %>%
  inner_join(
    dat_set %>%
      bind_rows(
        data.frame(
          dam       = id_dam,
          dam2      = id_dam,
          dam2_name = "ARAUCA",
          orden     = max(dat_set$orden) + 5,
          mpios     = "MAD"
        )
      )
  )


################################################################################
## Define factor ordering for consistent plotting
################################################################################

levels_originales <- c(
  "ARAUCA", "ARAUQUITA", "CRAVO NORTE", "FORTUL",
  "PUERTO RONDÓN", "SARAVENA", "TAME"
)

levels_originales2 <- c("MAD", "Minor administrative divisions")

tab_plot <- tab_plot %>%
  mutate(
    dam2_name = forcats::fct_reorder(dam2_name, orden),
    dam2_name = factor(dam2_name, levels = levels_originales),
    mpios     = forcats::fct_reorder(mpios, orden),
    mpios     = factor(mpios, levels = levels_originales2)
  )


################################################################################
## Plot: MPI dimensional composition by municipality (full indicators)
################################################################################

p3 <- ggplot(
  tab_plot,
  aes(x = dam2_name, y = estimacion, fill = nbi)
) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  theme_bw(15) +
  facet_grid(tipo ~ mpios, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "", x = "", y = "")

ggsave(
  plot = p3,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion_dam_ARAUCA.jpeg",
  width = 18,
  height = 12
)


################################################################################
## Aggregate indicators by MPI dimensions
##
## Purpose:
## To assess dimensional composition (Education, Housing, Health, Employment)
## instead of individual indicators
################################################################################

var_names <- c(
  "hnolee"      = "Education",
  "hlogroeduc"  = "Education",
  "heducninios" = "Education",
  "hhacina"     = "Housing",
  "henergia"    = "Health",
  "htic"        = "Housing",
  "hagua"       = "Housing",
  "hsaneamiento"= "Health",
  "hsalud"      = "Health",
  "hpartemp"    = "Employment and social security",
  "hempe"       = "Employment and social security",
  "hjub"        = "Employment and social security"
)

tab_plot2 <- tab_censo %>%
  mutate(nbi = recode(nbi, !!!var_names)) %>%
  group_by(dam2, nbi, tipo) %>%
  summarise(estimacion = sum(estimacion), .groups = "drop")


################################################################################
## Merge spatial and ordering metadata (again, including MAD)
################################################################################

tab_plot2 <- tab_plot2 %>%
  inner_join(
    dat_set %>%
      bind_rows(
        data.frame(
          dam       = id_dam,
          dam2      = id_dam,
          dam2_name = "ARAUCA",
          orden     = max(dat_set$orden) + 5,
          mpios     = "MAD"
        )
      )
  ) %>%
  mutate(
    dam2_name = forcats::fct_reorder(dam2_name, orden),
    dam2_name = factor(dam2_name, levels = levels_originales),
    mpios     = forcats::fct_reorder(mpios, orden),
    mpios     = factor(mpios, levels = levels_originales2)
  )


################################################################################
## Plot: MPI dimensional composition by municipality (aggregated dimensions)
################################################################################

p3 <- ggplot(
  tab_plot2,
  aes(x = dam2_name, y = estimacion, fill = nbi)
) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw(15) +
  facet_grid(tipo ~ mpios, scales = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "", x = "", y = "")

ggsave(
  plot = p3,
  filename = "Modelo_bayes_HxA_Final/COL/Output/plot_contribucion/contribucion_dam_ARAUCA_dim.jpeg",
  width = 18,
  height = 12
)

