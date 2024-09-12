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
library(survey)
library(srvyr)

encuesta <- readRDS(file = "Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds")
diseno <- encuesta %>% as_survey_design(data = encuesta, weights = fep ) 

estimado_ipm <- diseno %>% mutate_at(vars(matches("nbi")), as.numeric) %>%
  group_by(dam2) %>% summarise_at(vars(matches("nbi")), ~survey_mean(.x,  na.rm = TRUE))

estimado_ipm2 <- estimado_ipm %>% dplyr::select(-matches("_se")) %>% 
  pivot_longer(cols = matches("nbi"),values_to = "valor",names_to = "indicador")

ShapeSAE <- read_sf("Modelo_bayes_HxA/COL/Shape/COL_dam2.shp") 


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
    palette = "YlOrRd",
    colorNA = "white"
  ) + thema_map

Mapa_A <-
  maps3 + tm_polygons(
    "A",
    breaks = brks_A,
    title = "A",
    palette = "YlOrRd",
    colorNA = "white"
  ) + thema_map
Mapa_ipm <-
  maps3 + tm_polygons(
    "IPM",
    breaks = brks_ipm,
    title = "IPM",
    palette = "YlOrRd",
    colorNA = "white"
  ) + thema_map


Mapas <- tmap_arrange(Mapa_H, Mapa_A, Mapa_ipm)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA/COL/Output/COL_IPM.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapas,
  "Modelo_bayes_HxA/COL/Output/COL_IPM.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

########## mapa municipal por dimensiones #############
brks_dim <- round(quantile(
  temp_estimate_mpio$estimate,
  probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
), 2)
maps2 <- tm_shape(ShapeSAE %>%
                    inner_join(temp_estimate_mpio,  by = "dam2"))

Mapa_ing2 <-
  maps2 + tm_polygons(
    "estimate",
    breaks = brks_dim,
    title = "",
    palette = "YlOrRd",
    colorNA = "white"
  ) +
  tm_facets(by = "Indicador", ncol = 5)

tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA/COL/Output/COL_dims_ipm.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing2,
  "Modelo_bayes_HxA/COL/Output/COL_dims_ipm.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)


