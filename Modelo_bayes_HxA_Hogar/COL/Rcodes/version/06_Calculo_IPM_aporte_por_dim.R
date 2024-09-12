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


encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Encuestas/encuesta_nbi.rds") %>% 
  filter(edad >1, anoest <6)

encuesta_ipm <- encuesta_ipm %>%
  mutate_at(vars(matches("nbi")), as.numeric)
  
vars_nbi_hogar <-
  
  c("nbi_hnolee_ee" = 1/12 ,
    "nbi_hlogroeduc_ee"  = 1/12 ,
    "nbi_heducninios"    = 1/12 ,
    "nbi_hhacina"          = 1/12 ,
    "nbi_henergia" = 1/12 ,
    "nbi_htic" = 1/12 ,
    "nbi_hagua_ee"          = 1/12 ,
    "nbi_hsaneamiento_ee"  = 1/12 ,
    "nbi_hsalud_ee"        = 1/12 ,
    "nbi_hpartemp"        = 1/12 ,
    "nbi_hempe" = 1/12 ,
    "nbi_hjub"  = 1/12 )

resul_sample <- calculate_ipm_sample(k = 40, set_data = encuesta_ipm, vars_nbi= vars_nbi_hogar, weight_fep = "fep")
  
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

ipm_HA <- list() 
contribuciones <- list() 
iter = 1
for(iter in 1:50){
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

names(vars_nbi_hogar) <- paste0(names(vars_nbi_hogar), "_",iter)

resul <- calculate_ipm_censo(
  k = 40,
  set_data = censo_ipm_temp,
  vars_nbi = vars_nbi_hogar,
  weight_fep = "n"
)
names(resul$contribuciones) <- gsub(
  x = names(resul$contribuciones),
  pattern = paste0("_", iter),
  replacement = ""
)

ipm_HA[[iter]] <- resul$ipm_HA
contribuciones[[iter]] <- resul$contribuciones %>% data.frame(t(.)) %>% select(-1) %>% unique()
}

saveRDS(ipm_HA, "Modelo_bayes_HxA_Hogar/COL/Data/Estimacion_ipm/ipm_HA.rds")
saveRDS(contribuciones, "Modelo_bayes_HxA_Hogar/COL/Data/Estimacion_ipm/contribuciones.rds")

saveRDS(resul_sample, "Modelo_bayes_HxA_Hogar/COL/Data/Estimacion_ipm/resul_sample.rds")


ipm_nacional <- ipm_HA %>% bind_rows() %>% summarise_all(.fun = list(media = mean, sd = sd))
contribuciones_nacional <- contribuciones %>% bind_rows() %>% summarise_all(.fun = list(media = mean, sd = sd))


################################################################################

ipm_HA_agg <- list() 
contribuciones_agg <- list() 
iter <- 1
by_agrega = "sexo"
for(iter in 1:100){
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
  
  names(vars_nbi_hogar) <- paste0(names(vars_nbi_hogar), "_",iter)
  
  resul <- calculate_ipm_by_censo(
    group_var = by_agrega,
    k = 40,
    set_data = censo_ipm_temp,
    vars_nbi = vars_nbi_hogar,
    weight_fep = "n"
  )

  ipm_HA_agg[[iter]] <- map_df(resul,~.x$ipm_HA, .id = by_agrega)
  contribuciones_agg[[iter]] <- map_dfr(resul,~.x$contribuciones, .id = by_agrega)
  
  names(  contribuciones_agg[[iter]]) <- gsub(
    x = names(  contribuciones_agg[[iter]]),
    pattern = paste0("_", iter),
    replacement = ""
  )
  
}

ipm_sexo <- ipm_HA_agg %>% bind_rows() %>% group_by_at(by_agrega) %>% summarise_all(.fun = list(media = mean, sd = sd))
contribuciones_sexo <- contribuciones_agg %>% bind_rows() %>% group_by_at(by_agrega) %>% summarise_all(.fun = list(media = mean, sd = sd))

#####################################################################################################

ipm_HA_agg2 <- list() 
contribuciones_agg2 <- list() 
by_agrega = "dam"
for(iter in 1:100){
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
  
  names(vars_nbi_hogar) <- paste0(names(vars_nbi_hogar), "_",iter)
  
  resul <- calculate_ipm_by_censo(
    group_var = by_agrega,
    k = 40,
    set_data = censo_ipm_temp,
    vars_nbi = vars_nbi_hogar,
    weight_fep = "n"
  )
  
  ipm_HA_agg[[iter]] <- map_df(resul,~.x$ipm_HA, .id = by_agrega)
  contribuciones_agg[[iter]] <- map_dfr(resul,~.x$contribuciones, .id = by_agrega)
  
  names(  contribuciones_agg[[iter]]) <- gsub(
    x = names(  contribuciones_agg[[iter]]),
    pattern = paste0("_", iter),
    replacement = ""
  )
  
}

ipm_sexo <- ipm_HA_agg %>% bind_rows() %>% group_by_at(by_agrega) %>% summarise_all(.fun = list(media = mean, sd = sd))
contribuciones_sexo <- contribuciones_agg %>% bind_rows() %>% group_by_at(by_agrega) %>% summarise_all(.fun = list(media = mean, sd = sd))


