################################################################################
## Proyecto del IPM = HxA 
## Andrés Gutiérrez y Stalyn Guerrero 
################################################################################

################################################################################
## Librerias 
################################################################################
rm(list = ls())
library(tidyverse)
library(patchwork)
library(magrittr)
library(furrr)
library(haven)
library(weights)
library(rstanarm)
library(rstantools)

crear_epred_mat_dummy <- function(variable, newdata) {
  # Paso 1: Cargar el modelo
  modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", variable, ".rds")
  fit <- readRDS(file = modelo_rds)
  
  # Paso 2: Generar epred_mat_agua
  epred_mat <- posterior_epred(
    fit,
    newdata = newdata,
    type = "response",
    allow.new.levels = TRUE,
    draws = 100
  )
  
  # Paso 3: Generar epred_mat_agua_dummy
  epred_mat_dummy <- rbinom(n = nrow(epred_mat) * ncol(epred_mat), 1, epred_mat)
  epred_mat_dummy <- matrix(epred_mat_dummy, nrow = nrow(epred_mat), 
                            ncol = ncol(epred_mat))
  
  # Guardar epred_mat_agua_dummy como un archivo .rds
  cat(modelo_rds, "\n")
  return(epred_mat_dummy)
}


################################################################################
# Lectura de base de datos
################################################################################

censo_ipm <- readRDS("Modelo_bayes_HxA_hogar/COL/Data/Censo/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

encuesta_ipm <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Encuestas/encuesta_nbi.rds")

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") 

encuesta_ipm <- inner_join(encuesta_ipm, statelevel_predictors_df)
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

cor_muestra <- cor(encuesta_ipm[,nbi_hogar] %>% mutate_all(as.numeric))

################################################################################
## Lectura de los modelos 
################################################################################
epred_mat_hnolee_ee <- crear_epred_mat_dummy(nbi_hogar[1],encuesta_ipm)  
epred_mat_hlogroeduc_ee <- crear_epred_mat_dummy(nbi_hogar[2],encuesta_ipm) 
epred_mat_heducninios <- crear_epred_mat_dummy(nbi_hogar[3],encuesta_ipm)  
epred_mat_hhacina <- crear_epred_mat_dummy(nbi_hogar[4],encuesta_ipm)  
epred_mat_henergia <- crear_epred_mat_dummy(nbi_hogar[5],encuesta_ipm) 
epred_mat_htic <- crear_epred_mat_dummy(nbi_hogar[6],encuesta_ipm) 
epred_mat_hagua_ee <- crear_epred_mat_dummy(nbi_hogar[7],encuesta_ipm)  
epred_mat_hsaneamiento_ee <- crear_epred_mat_dummy(nbi_hogar[8],encuesta_ipm)  
epred_mat_hsalud_ee <- crear_epred_mat_dummy(nbi_hogar[9],encuesta_ipm)  
epred_mat_hpartemp <- crear_epred_mat_dummy(nbi_hogar[10],encuesta_ipm)  
epred_mat_hempe <- crear_epred_mat_dummy(nbi_hogar[11],encuesta_ipm)  
epred_mat_hjub <- crear_epred_mat_dummy(nbi_hogar[12],encuesta_ipm) 


dim(cor_muestra)
sum_cor <- matrix(0, 12,12)

for( ii in 1:100) {
  sum_cor <- sum_cor +
    data.frame(
      hnolee_ee = epred_mat_hnolee_ee[ii, ] ,
      hlogroeduc_ee = epred_mat_hlogroeduc_ee[ii, ] ,
      heducninios = epred_mat_heducninios[ii, ] ,
      hhacina = epred_mat_hhacina[ii, ] ,
      henergia = epred_mat_henergia[ii, ] ,
      htic = epred_mat_htic[ii, ] ,
      hagua_ee = epred_mat_hagua_ee[ii, ] ,
      hsaneamiento_ee = epred_mat_hsaneamiento_ee[ii, ] ,
      hsalud_ee = epred_mat_hsalud_ee[ii, ] ,
      hpartemp = epred_mat_hpartemp[ii, ] ,
      hempe = epred_mat_hempe[ii, ] ,
      hjub = epred_mat_hjub[ii, ]
    ) %>% cor()
}

rm(
  list = c("epred_mat_hnolee_ee" ,  
    "epred_mat_hlogroeduc_ee" ,  
    "epred_mat_heducninios" ,  
    "epred_mat_hhacina" ,  
    "epred_mat_henergia" ,  
    "epred_mat_htic" ,  
    "epred_mat_hagua_ee" ,  
    "epred_mat_hsaneamiento_ee" ,  
    "epred_mat_hsalud_ee" ,  
    "epred_mat_hpartemp" ,  
    "epred_mat_hempe" ,  
    "epred_mat_hjub"   )
)


list(
## Muestra 
cor_muestra = cor_muestra,
## Modelo Bayes 
cor_SAE = sum_cor/100) %>% 
openxlsx::write.xlsx("Modelo_bayes_HxA_Hogar/COL/Data/correlations.xlsx") 

## Error realtivo 
100*((cor_muestra - sum_cor/1000)/cor_muestra)

########################################################################
## censo 
## 

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

sum_cor2 <- matrix(0, 12,12)
for(ii in 1:100) {
temp <-
    data.frame(
      hnolee_ee = epred_mat_dummy_hnolee[ii, ] ,
      hlogroeduc_ee = epred_mat_dummy_hlogroeduc[ii, ] ,
      heducninios = epred_mat_dummy_heducninios[ii, ] ,
      hhacina = epred_mat_dummy_hhacina[ii, ] ,
      henergia = epred_mat_dummy_henergia[ii, ] ,
      htic = epred_mat_dummy_htic[ii, ] ,
      hagua_ee = epred_mat_dummy_hagua[ii, ] ,
      hsaneamiento_ee = epred_mat_dummy_hsaneamiento[ii, ] ,
      hsalud_ee = epred_mat_dummy_hsalud[ii, ] ,
      hpartemp = epred_mat_dummy_hpartemp[ii, ] ,
      hempe = epred_mat_dummy_hempe[ii, ] ,
      hjub = epred_mat_dummy_hjub[ii, ]
    ) 
sum_cor2 <- sum_cor2 +
  wtd.cors(temp, weight = censo_ipm$n)
cat(ii, "\n")
}

# 4:14 

list(
  ## Muestra 
  cor_muestra = cor_muestra,
  ## Modelo Bayes 
  cor_SAE = sum_cor/100, 
  ## Modelo Bayes censo
  cor_SAE_cesno = sum_cor2/100
  ) %>% 
  openxlsx::write.xlsx("Modelo_bayes_HxA_Hogar/COL/Data/correlations.xlsx") 

