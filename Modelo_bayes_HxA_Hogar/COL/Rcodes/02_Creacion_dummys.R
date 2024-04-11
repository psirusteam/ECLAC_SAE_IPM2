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

crear_epred_mat_dummy <- function(variable, newdata) {
  # Paso 1: Cargar el modelo
  modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", variable, ".rds")
  ruta_guardado <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/epred_mat_dummy_", variable, ".rds")
  fit <- readRDS(file = modelo_rds)
  
  # Paso 2: Generar epred_mat_agua
  epred_mat <- posterior_epred(
    fit,
    newdata = newdata,
    type = "response",
    allow.new.levels = TRUE
  )
  
  # Paso 3: Generar epred_mat_agua_dummy
  epred_mat_dummy <- rbinom(n = nrow(epred_mat) * ncol(epred_mat), 1, epred_mat)
  epred_mat_dummy <- matrix(epred_mat_dummy, nrow = nrow(epred_mat), 
                            ncol = ncol(epred_mat))
  
  # Guardar epred_mat_agua_dummy como un archivo .rds
  saveRDS(epred_mat_dummy, file = ruta_guardado)
  cat(ruta_guardado, "\n")

}


################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA_hogar/COL/Data/Censo/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") 

poststrat_df <- left_join(censo_ipm, statelevel_predictors_df,
                          by = c("dam", "dam2"))

################################################################################
## Lectura de los modelos 
################################################################################
nbi_hogar <- c(
  "nbi_hnolee_ee" , #ok
  "nbi_hlogroeduc_ee" ,#ok
  "nbi_heducninios" , #ok
  "nbi_hhacina" , #ok
  "nbi_henergia" ,
  "nbi_htic" ,
  "nbi_hagua_ee" , #ok
  "nbi_hsaneamiento_ee" , #ok
  "nbi_hsalud_ee" , #ok
  "nbi_hpartemp" , #ok
  "nbi_hempe" , #ok
  "nbi_hjub" #ok
)  

crear_epred_mat_dummy(nbi_hogar[1],poststrat_df) # OK
crear_epred_mat_dummy(nbi_hogar[2],poststrat_df) # OK
crear_epred_mat_dummy(nbi_hogar[3],poststrat_df) # OK
crear_epred_mat_dummy(nbi_hogar[4],poststrat_df) # OK
crear_epred_mat_dummy(nbi_hogar[5],poststrat_df) # Ok
crear_epred_mat_dummy(nbi_hogar[6],poststrat_df) # Ok
crear_epred_mat_dummy(nbi_hogar[7],poststrat_df) # Ok
crear_epred_mat_dummy(nbi_hogar[8],poststrat_df) # ok
crear_epred_mat_dummy(nbi_hogar[9],poststrat_df) # ok
crear_epred_mat_dummy(nbi_hogar[10],poststrat_df) # ok 
crear_epred_mat_dummy(nbi_hogar[11],poststrat_df) # ok
crear_epred_mat_dummy(nbi_hogar[12],poststrat_df) # ok


