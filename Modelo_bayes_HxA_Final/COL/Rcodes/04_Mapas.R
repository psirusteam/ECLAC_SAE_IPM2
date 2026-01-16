################################################################################
## Project: Multidimensional Poverty Index (MPI / IPM) – HxA Framework
##
## Purpose:
## This script generates cartographic outputs for the Multidimensional Poverty
## Index (MPI) and its components using Small Area Estimation (SAE) results.
##
## Specifically, it produces:
##  - National municipal-level maps of H, A, and MPI
##  - Department-specific (DAM) maps
##  - Municipal maps by individual MPI dimensions
##
## All maps are exported in high-resolution formats (JPEG and PDF) for
## publication and dissemination.
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

# Data manipulation
library(tidyverse)
library(magrittr)

# Bayesian modeling infrastructure (required by upstream objects)
library(rstan)
library(rstantools)
library(rstanarm)
library(lme4)
library(posterior)

# Spatial data handling and mapping
library(sf)
library(tmap)

# Export utilities
library(openxlsx)
library(patchwork)


################################################################################
## Load SAE estimation results
################################################################################

# Aggregated MPI estimates (H, A, MPI) for multiple domains
estimado_ipm <- readRDS(
  file = "Modelo_bayes_HxA_Final/COL/Data/Modelo/estimado_ipm_HA_freq.rds"
)

# Municipal-level estimates by MPI dimension
temp_estimate_mpio <- readRDS(
  "Modelo_bayes_HxA_Final/COL/data/Modelo/temp_estimate_mpio.rds"
)


################################################################################
## Load spatial data
################################################################################

# Municipal shapefile used for SAE mapping
ShapeSAE <- read_sf(
  "Modelo_bayes_HxA_Final/COL/Shape/COL_dam2.shp"
)


################################################################################
## Define classification breaks for maps
################################################################################

# Headcount ratio (H)
brks_H <- round(
  quantile(estimado_ipm$dam2$H,
           probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
  2
)

# Multidimensional Poverty Index (MPI)
brks_ipm <- round(
  quantile(estimado_ipm$dam2$IPM,
           probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
  2
)

# Intensity of poverty (A)
brks_A <- round(
  quantile(estimado_ipm$dam2$A,
           probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
  2
)


################################################################################
## National municipal-level maps (H, A, MPI)
################################################################################

# Base map with MPI estimates joined to spatial data
maps3 <- tm_shape(
  ShapeSAE %>%
    left_join(
      estimado_ipm$dam2 %>% rename(PMI = IPM),
      by = "dam2"
    )
)

# Headcount ratio (H)
Mapa_H <- maps3 +
  tm_polygons(
    fill = "H",
    fill.scale = tm_scale(
      breaks   = brks_H,
      values   = "brewer.yl_or_rd",
      value.na = "white"
    ),
    fill.legend = tm_legend(title = "H")
  )

# Intensity of poverty (A)
Mapa_A <- maps3 +
  tm_polygons(
    fill = "A",
    fill.scale = tm_scale(
      breaks   = brks_A,
      values   = "brewer.yl_or_rd",
      value.na = "white"
    ),
    fill.legend = tm_legend(title = "A")
  )

# Multidimensional Poverty Index (MPI)
Mapa_ipm <- maps3 +
  tm_polygons(
    fill = "PMI",
    fill.scale = tm_scale(
      breaks   = brks_ipm,
      values   = "brewer.yl_or_rd",
      value.na = "white"
    ),
    fill.legend = tm_legend(title = "MPI")
  )

# Arrange maps in a single panel
Mapas <- tmap_arrange(Mapa_H, Mapa_A, Mapa_ipm, ncol = 3)

# Export national maps
tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Final/COL/Output/COL_IPM.jpeg",
  width  = 5920,
  height = 3080,
  asp    = 0
)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Final/COL/Output/COL_IPM.pdf",
  width  = 5920,
  height = 3080,
  asp    = 0
)


################################################################################
## Department-level maps (example: DAM = 81)
################################################################################

id_dam <- "81"

# Tabular inspection of municipal results within the department
ShapeSAE %>%
  filter(dam == id_dam) %>%
  select(dam2, dam2_name) %>%
  left_join(
    estimado_ipm$dam2 %>% rename(PMI = IPM),
    by = "dam2"
  ) %>%
  select(dam2, dam2_name, H, A, PMI)

# Spatial subset
maps3 <- tm_shape(
  ShapeSAE %>%
    filter(dam == id_dam) %>%
    left_join(
      estimado_ipm$dam2 %>% rename(PMI = IPM),
      by = "dam2"
    )
)

# Arrange department-level maps vertically
Mapas <- tmap_arrange(
  maps3 + tm_polygons("H",   fill.scale = tm_scale(breaks = brks_H,   values = "brewer.yl_or_rd")),
  maps3 + tm_polygons("A",   fill.scale = tm_scale(breaks = brks_A,   values = "brewer.yl_or_rd")),
  maps3 + tm_polygons("PMI", fill.scale = tm_scale(breaks = brks_ipm, values = "brewer.yl_or_rd")),
  ncol = 1
)

# Export department-level maps
tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Final/COL/Output/COL_IPM_arauca2.jpeg",
  width  = 4920,
  height = 2080,
  asp    = 0
)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Final/COL/Output/COL_IPM_arauca2.pdf",
  width  = 4920,
  height = 2080,
  asp    = 0
)


################################################################################
## Municipal maps by MPI dimension
################################################################################

# Classification breaks shared across dimensions
brks_dim <- round(
  quantile(
    temp_estimate_mpio$estimate,
    probs = seq(0, 1, by = 0.1)
  ),
  2
)

# Human-readable labels for MPI dimensions
var_names <- c(
  "nbi_hnolee"       = "Illiteracy",
  "nbi_hlogroeduc"   = "Educational attainment",
  "nbi_heducninios"  = "School attendance / lag",
  "nbi_hhacina"      = "Overcrowding",
  "nbi_henergia"     = "Energy",
  "nbi_htic"         = "Internet access",
  "nbi_hagua"        = "Water",
  "nbi_hsaneamiento" = "Sanitation",
  "nbi_hsalud"       = "Health insurance",
  "nbi_hpartemp"     = "Labor market participation",
  "nbi_hempe"        = "Employment quality",
  "nbi_hjub"         = "Pensions"
)

# Recode indicator labels
temp_estimate_mpio <- temp_estimate_mpio %>%
  mutate(Indicador = recode(Indicador, !!!var_names))

# National municipal maps by dimension
maps2 <- tm_shape(
  ShapeSAE %>%
    inner_join(temp_estimate_mpio, by = "dam2")
)

Mapa_ing2 <- maps2 +
  tm_polygons(
    fill = "estimate",
    fill.scale = tm_scale(
      breaks   = brks_dim,
      values   = "brewer.yl_or_rd",
      value.na = "white"
    ),
    fill.legend = tm_legend(title = "")
  ) +
  tm_facets(
    by = "Indicador",
    ncol = 4,
    titles.var = list(size = 5)
  )

# Export dimension maps (national)
tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA_Final/COL/Output/COL_dims_ipm.jpeg",
  width  = 6920,
  height = 4080,
  asp    = 0
)

tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA_Final/COL/Output/COL_dims_ipm.pdf",
  width  = 6920,
  height = 4080,
  asp    = 0
)


################################################################################
## Department-level maps by MPI dimension
################################################################################

maps2 <- tm_shape(
  ShapeSAE %>%
    filter(dam == id_dam) %>%
    inner_join(temp_estimate_mpio, by = "dam2")
)

Mapa_ing3 <- maps2 +
  tm_polygons(
    fill = "estimate",
    fill.scale = tm_scale(
      breaks   = brks_dim,
      values   = "brewer.yl_or_rd",
      value.na = "white"
    )
  ) +
  tm_facets(
    by = "Indicador",
    ncol = 4,
    titles.var = list(size = 5)
  )

# Export department-level dimension maps
tmap_save(
  Mapa_ing3,
  "Modelo_bayes_HxA_Final/COL/Output/COL_dims_ipm_arauca.jpeg",
  width  = 5920,
  height = 3080,
  asp    = 0
)

tmap_save(
  Mapa_ing3,
  "Modelo_bayes_HxA_Final/COL/Output/COL_dims_ipm_arauca.pdf",
  width  = 6920,
  height = 4080,
  asp    = 0
)
