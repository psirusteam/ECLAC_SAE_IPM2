#########################################################
# Proyecto IPM                                          #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(magrittr)
library(formula.tools)
library(remotes)
#library(StatisticalModels)
library(fastDummies)
library(haven)
library(magrittr)
library(stringr)
library(openxlsx)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
# # Leer encuesta
encuesta_sta <- read_dta("V:/DAT/BADEHOG/BADEHOG_N/Estandarizadas/COL_2018N1.dta")
encuesta_sta %<>% mutate(orden_temp = str_pad(
  string = 1:n(),
  width = 7,
  pad = "0"
))

encuesta_comp <- read_dta("V:/DAT/BADEHOG/BADEHOG_N/Completas/col18n1/col18n1.dta")

encuesta_ipm <- read_dta("Modelo_bayes_HxA/COL/Data/COL2018IPM.dta")

upms <- readRDS(file = "Modelo_bayes_HxA/COL/Data/upm_dpto_2018.rds")

encuesta_comp %<>% left_join(upms,
                        by = c("directorio" = "DIRECTORIO",
                               "secuencia_p" = "SECUENCIA_P",
                               "orden"))

encuesta_comp$mpio <- substr(encuesta_comp$segmento,8,12)

encuesta <-
  encuesta_comp %>% select(id_hogar, id_pers, upm, estrato, mpio) %>%
  inner_join(encuesta_sta) %>%
  select(id_hogar:mpio, `_fep`, area_ee, sexo, etnia_ee, anoest, edad) %>%
  inner_join(encuesta_ipm %>% select(id_hogar, id_pers, matches("^nbi|^g0|^or")))


################################################################################
### GEIH: Creando las dimensiones de interés ###
################################################################################

table(encuesta_ipm$or_nbi_matviv_ee,encuesta_ipm$nbi_matviv_ee, useNA = "a")
table(encuesta_ipm$or_nbi_hacina,encuesta_ipm$nbi_hacina_ee, useNA = "a")
table(encuesta_ipm$or_nbi_agua_ee,encuesta_ipm$nbi_agua_ee, useNA = "a")
table(encuesta_ipm$or_nbi_saneamiento_ee,encuesta_ipm$nbi_saneamiento_ee, useNA = "a")
table(encuesta_ipm$or_nbi_elect_ee,encuesta_ipm$nbi_elect_ee, useNA = "a")
table(encuesta_ipm$or_nbi_combus_ee,encuesta_ipm$nbi_combus_ee, useNA = "a")
table(encuesta_ipm$or_ssalud_ee,encuesta_ipm$ssalud_ee, useNA = "a")
table(encuesta_ipm$or_tic_acc_internet_ee,encuesta_ipm$tic_acc_internet_ee, useNA = "a")


### GEIH: Creating the post-stratification variables: Age and Schooling ###

encuesta_nbi <- encuesta %>%
  transmute(
    dam = str_sub(string = mpio,start = 1,end = 2 ),
    dam2 = mpio,
    nbi_matviv_ee = as.character(nbi_matviv_ee),
    nbi_hacina_ee = as.character(nbi_hacina_ee),        
    nbi_tic = as.character(nbi_tic),
    nbi_agua_ee = as.character(nbi_agua_ee),
    nbi_saneamiento_ee = as.character(nbi_saneamiento_ee),
    nbi_energia = as.character(nbi_energia),       
    nbi_salud_ee = as.character(nbi_salud_ee),
    nbi_empe = as.character(nbi_empe),          
    nbi_educ = as.character(nbi_educ),
    area = case_when(area_ee == 1 ~ "1",
                     TRUE ~ "0"),
    sexo = as.character(sexo),
    
    etnia = case_when(
      etnia_ee == 1 ~ "1", # Indígena
      etnia_ee == 2 ~ "2", # Afro
      TRUE ~ "3"), # Otro
    
    anoest = case_when(
      edad < 5 | is.na(anoest)   ~ "98"  , #No aplica
      anoest == 99 ~ "99", #NS/NR
      anoest == 0  ~ "1", # Sin educacion
      anoest %in% c(1:6) ~ "2",       # 1 - 6
      anoest %in% c(7:12) ~ "3",      # 7 - 12
      anoest > 12 ~ "4",      # mas de 12
      TRUE ~ "Error"  ),
    
    
    edad = case_when(
      edad <= 15 ~ "1",
      edad < 30 ~ "2",
      edad < 45 ~ "3",
      edad < 65 ~ "4",
      edad >= 65 ~ "5"),
 
    fep = `_fep`
  ) 

saveRDS(encuesta_nbi, file = "Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds")




