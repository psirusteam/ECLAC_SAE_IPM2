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
library(furrr)
library(parallel)
library(openxlsx)
library(survey)
library(srvyr)
library(sampling)
source("Modelo_bayes_HxA_Hogar/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA_Hogar/0funciones/agregado_dim_ipm.r")
source("Modelo_bayes_HxA_Hogar/0funciones/IPM_descomponer.R")
################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Censo/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")




################################################################################
## Lectura de variables dummy  
################################################################################
epred_mat_dummy_hnolee <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hnolee_ee.rds")
epred_mat_dummy_hlogroeduc <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hlogroeduc_ee.rds")
epred_mat_dummy_heducninios <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_heducninios.rds")
epred_mat_dummy_hhacina <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hhacina.rds")
epred_mat_dummy_henergia <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_henergia.rds")
epred_mat_dummy_htic <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_htic.rds")
epred_mat_dummy_hagua <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hagua_ee.rds")
epred_mat_dummy_hsaneamiento <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hsaneamiento_ee.rds")
epred_mat_dummy_hsalud <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hsalud_ee.rds")
epred_mat_dummy_hpartemp <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hpartemp.rds")
epred_mat_dummy_hempe <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hempe.rds")
epred_mat_dummy_hjub <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_nbi_hjub.rds")

k = 40

vars_nbi_hogar <-
  c("hnolee" = 1/12 ,
    "hlogroeduc"  = 1/12 ,
    "heducninios"    = 1/12 ,
    "hhacina"          = 1/12 ,
    "henergia" = 1/12 ,
    "htic" = 1/12 ,
    "hagua"          = 1/12 ,
    "hsaneamiento"  = 1/12 ,
    "hsalud"        = 1/12 ,
    "hpartemp"        = 1/12 ,
    "hempe" = 1/12 ,
    "hjub"  = 1/12 )

iter <- 1 
ipm_dam <- list()
ipm_nacional <- list()
for(iter in 1:1000){
  censo_ipm_temp <- censo_ipm %>% mutate(
    !!paste0("hnolee_", iter) :=   epred_mat_dummy_hnolee[iter,], 
    !!paste0("hlogroeduc_", iter) :=   epred_mat_dummy_hlogroeduc[iter,],
    !!paste0("heducninios_", iter) :=   epred_mat_dummy_heducninios[iter,],
    !!paste0("hhacina_", iter) :=   epred_mat_dummy_hhacina[iter,],
    !!paste0("henergia_", iter) :=   epred_mat_dummy_henergia[iter,],
    !!paste0("htic_", iter) :=   epred_mat_dummy_htic[iter,],
    !!paste0("hagua_", iter) :=   epred_mat_dummy_hagua[iter,],
    !!paste0("hsaneamiento_", iter) :=   epred_mat_dummy_hsaneamiento[iter,],
    !!paste0("hsalud_", iter) :=   epred_mat_dummy_hsalud[iter,],
    !!paste0("hpartemp_", iter) :=   epred_mat_dummy_hpartemp[iter,],
    !!paste0("hempe_", iter) :=   epred_mat_dummy_hempe[iter,],
    !!paste0("hjub_", iter) :=   epred_mat_dummy_hjub[iter,]  
  )
  vars_nbi_hogar_temp <- vars_nbi_hogar
  names(vars_nbi_hogar_temp) <- paste0(names(vars_nbi_hogar_temp), "_",iter)  
  
ipm_nacional[[iter]] <-calculate_ipm_by_censo(
  group_var = NULL,
  k = 40,
  set_data = censo_ipm_temp %>% mutate(fep2 = n),
  vars_nbi = vars_nbi_hogar_temp,
  weight_fep = "fep2"
)

ipm_dam[[iter]] <-calculate_ipm_by_censo(
  group_var = "dam",
  k = 40,
  set_data = censo_ipm_temp %>% mutate(fep2 = n),
  vars_nbi = vars_nbi_hogar_temp,
  weight_fep = "fep2"
)

cat(iter, "\n")
}


contribuciones_agg <- map(ipm_nacional, function(df) {
  names(df$contribuciones) <- gsub("_\\d+", "", names(df$contribuciones))
  return(df)
}) %>%  map_df( ~ .x$contribuciones) %>% 
  summarise_all(.funs = list("media" = mean, sd = sd))



estimado_nacional <- list(
  ipm_HA = ipm_nacional %>% map_df( ~ .x$ipm_HA) %>% 
  summarise_all(.funs = list("media" = mean, sd = sd)),
  
  contribuciones = contribuciones_agg
)




contribuciones_agg_dam <- map_df(ipm_dam, function(df) {
   temp <- map_df(df, ~ .x$contribuciones, .id = "dam")
  names(temp) <- gsub("_\\d+", "", names(temp))
  return(temp)
}) %>%  
  group_by(dam) %>%  summarise_all(.funs = list("media" = mean, sd = sd))


estimado_dam <- list(
  ipm_HA = map_df(ipm_dam, function(sublist)
    map_df(sublist, ~ .x$ipm_HA, .id = "dam")) %>% 
    group_by(dam) %>%  summarise_all(.funs = list("media" = mean, sd = sd))
  ,
  
  contribuciones = contribuciones_agg_dam
)


# Guardar resultados agregados en un archivo Excel y abrir el archivo

openxlsx::write.xlsx(estimado_nacional,
                     file = "Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_nacional_sin_bench.xlsx")

openxlsx::openXL("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_nacional_sin_bench.xlsx")

# Guardar resultados agregados en un archivo Excel y abrir el archivo

openxlsx::write.xlsx(estimado_dam,
file = "Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_dam_sin_bench.xlsx")

openxlsx::openXL("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_dam_sin_bench.xlsx")
