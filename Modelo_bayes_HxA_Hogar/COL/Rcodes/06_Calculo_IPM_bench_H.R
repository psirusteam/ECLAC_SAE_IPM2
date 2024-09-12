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
N_sample = sum(encuesta_ipm$fep)  
total <- c(N_sample,N_sample*resul_sample$ipm_HA$H)
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

ipm_HA_nacional <- list() 
contribuciones_nacional <- list() 
ipm_HA_depto <- list() 
contribuciones_depto <- list() 
ipm_HA_area <- list() 
contribuciones_area <- list() 
iter = 1
names_cov = "den"
k = 40
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

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
censo_ipm_temp <- censo_ipm_temp  %>% mutate("den" = 1) %>% 
  fastDummies::dummy_cols(select_columns = "den",
                          remove_selected_columns = FALSE)
k_pct <- k / 100

# Crear nuevas variables a nivel individual
for (nbi_ii in names(vars_nbi_hogar)) {
  censo_ipm_temp <- censo_ipm_temp %>%
    mutate(!!paste0("g0_", nbi_ii) := round(vars_nbi_hogar[nbi_ii] * !!sym(nbi_ii), 5))
}

censo_ipm_temp <- censo_ipm_temp  %>%
  mutate(sum_g0 = rowSums(select(., starts_with("g0_")), na.rm = TRUE))

# Crear la variable de pobre multidimensional (PMD)
censo_ipm_temp <- censo_ipm_temp %>%
  mutate(
    !!paste0("PMD") := ifelse(sum_g0 >= k_pct & !is.na(sum_g0), 1, 0)
  )


# censo_ipm_temp %<>% 
#   mutate_at(vars(matches(names_cov)) ,~.*censo_ipm_temp$PMD)


### total

censo_ipm_temp$gk <- calib(Xs = censo_ipm_temp %>% select("den_1", "PMD"), 
                         d = censo_ipm_temp$n,
                         total = total,
                         method="logit") 

checkcalibration(Xs = censo_ipm_temp %>% select("den_1", "PMD"), 
                 d = censo_ipm_temp$n,
                 total = total,
                 g = censo_ipm_temp$gk)



hist(censo_ipm_temp$gk)
min(censo_ipm_temp$gk)
sum(censo_ipm_temp$gk == min(censo_ipm_temp$gk))


map(names_cov ,~ censo_ipm_temp  %>%
      summarise(
        Nhat = sum(n),
        t_num = sum(PMD* n*gk)
      ) %>% mutate(H = t_num /Nhat))
total
resul_sample$ipm_HA$H

ipm_nacional <-calculate_ipm_by_censo(
  group_var = NULL,
  k = 40,
  set_data = censo_ipm_temp %>% mutate(fep2 = n*gk),
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep2"
)

ipm_mpio <-calculate_ipm_by_censo(
  group_var = "dam",
  k = 40,
  set_data = censo_ipm_temp %>% mutate(fep2 = n*gk),
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep2"
)

ipm_area <-calculate_ipm_by_censo(
  group_var = "area",
  k = 40,
  set_data = censo_ipm_temp %>% mutate(fep2 = n*gk),
  vars_nbi = vars_nbi_hogar,
  weight_fep = "fep2"
)

ipm_HA[[iter]] <- map_df(paso, ~.x$ipm_HA, .id = "dam2")
contribuciones[[iter]] <-  map_dfr(paso, ~.x$contribuciones,.id = "dam2")
print(table(rowSums(map_dfr(paso, ~.x$contribuciones))))


}
paso
