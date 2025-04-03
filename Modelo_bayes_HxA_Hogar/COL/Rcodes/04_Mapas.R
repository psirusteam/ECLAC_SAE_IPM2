################################################################################
## Proyecto del IPM = HxA 
## Andrés Gutiérrez y Stalyn Guerrero 
################################################################################

################################################################################
## Librerias 
################################################################################
rm(list = ls())
library(tidyverse)
library(rstantools)
library(rstan)
library(posterior)
library(patchwork)
library(lme4)
library(rstanarm)
library(magrittr)
library(tmap)
library(openxlsx)
library(sf)

estimado_ipm <- readRDS(file = "Modelo_bayes_HxA_Hogar/COL/Data/Modelo/estimado_ipm1.rds")
temp_estimate_mpio <- readRDS("Modelo_bayes_HxA_Hogar/COL/data/Modelo/temp_estimate_mpio.rds")

ShapeSAE <- read_sf("Modelo_bayes_HxA_Hogar/COL/Shape/COL_dam2.shp") 

brks_H <- round(quantile(estimado_ipm$dam2$H,probs = c(0,0.2,0.4,0.6,0.8,1)),2)
brks_ipm <- round(quantile(estimado_ipm$dam2$IPM,probs = c(0,0.2,0.4,0.6,0.8,1)),2)
brks_A <- round(quantile(estimado_ipm$dam2$A,probs = c(0,0.2,0.4,0.6,0.8,1)),2)

maps3 <- tm_shape(ShapeSAE %>%
                    left_join(estimado_ipm$dam2,  by = "dam2"))

thema_map <- tm_layout(legend.only = FALSE,
                       legend.height = -0.5,
                       legend.width = -0.4,
                       asp = 1.5,
                       legend.text.size = 5,
                       legend.title.size = 4)

Mapa_H <-
  maps3 + tm_polygons(
    "H",
    breaks = brks_H,
    title = "H",
    palette = "brewer.yl_or_rd",
    colorNA = "white"
  ) + thema_map

Mapa_A <-
  maps3 + tm_polygons(
    "A",
    breaks = brks_A,
    title = "A",
    palette = "brewer.yl_or_rd",
    colorNA = "white"
  ) + thema_map
Mapa_ipm <-
  maps3 + tm_polygons(
    "IPM",
    breaks = brks_ipm,
    title = "IPM",
    palette = "brewer.yl_or_rd",
    colorNA = "white"
  ) + thema_map


Mapas <- tmap_arrange(Mapa_H, Mapa_A, Mapa_ipm)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Hogar/COL/Output/COL_IPM.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA_Hogar/COL/Output/COL_IPM.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

########## mapa municipal por dimensiones #############
brks_dim <- round(quantile(
  temp_estimate_mpio$estimate,
  probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
), 2)

var_names <- c(
  "nbi_hnolee" = "Illiteracy",
  "nbi_hlogroeduc" = "Educational attainment",
  "nbi_heducninios" = "Non_attendance or lag",
  "nbi_hhacina" = "Overcrowding",
  "nbi_henergia" = "Energy",
  "nbi_htic" = "Internet access",
  "nbi_hagua" = "Water",
  "nbi_hsaneamiento" = "Sanitation",
  "nbi_hsalud" = "Health insurance",
  "nbi_hpartemp" = "Labor market participation",
  "nbi_hempe" = "Quality of employment",
  "nbi_hjub" = "Pensions"
)


temp_estimate_mpio <- temp_estimate_mpio %>%
  mutate(Indicador = recode(Indicador, !!!var_names)) 

unique(temp_estimate_mpio$Indicador)

maps2 <- tm_shape(ShapeSAE %>%
                    inner_join(temp_estimate_mpio,  by = "dam2"))

Mapa_ing2 <- 
  maps2 + 
  tm_polygons(
    fill = "estimate",
    fill.scale = tm_scale(
      breaks = brks_dim,
      values = "brewer.yl_or_rd",  # Antes "palette"
      value.na = "white"           # Antes "colorNA"
    ),
    fill.legend = tm_legend(title = "")  # Antes "title"
  ) + 
  tm_facets(
    by = "Indicador", 
    ncol = 4,
   titles.var = list(
      size = 2  # Ajusta este valor (1 = tamaño base)
    )
  ) 

tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA_Hogar/COL/Output/COL_dims_ipm.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA_Hogar/COL/Output/COL_dims_ipm.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)


