#########################################################
# Proyecto IPM                   #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

# Loading required libraries ----------------------------------------------

library(patchwork)
library(trafo)
library(normtest)
library(nortest)
library(lme4)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(purrr)
library(furrr)
source("Frecuentista_mpio/0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
fit_ipm <- readRDS( file = "Frecuentista_mpio/COL/Data/fit_freq_ipm.rds")

encuesta_ipm <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")
censo_ipm <- readRDS("Frecuentista_mpio/COL/Data/censo_ipm2.rds") 
tasa_desocupados <- readRDS("Frecuentista_mpio/COL/Data/tasa_desocupacion.rds")

# Agregando encuesta ------------------------------------------------------
statelevel_predictors_df <- tasa_desocupados
byAgrega <- c("mpio", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )

poststrat_df <- inner_join(censo_ipm %>% mutate_at(all_of(byAgrega), as.character),
                           statelevel_predictors_df)


poststrat_df$prob_ipm <- predict(
  fit_ipm,
  newdata = poststrat_df,
  type = "response",
  allow.new.levels = TRUE
)

poststrat_MC <- poststrat_df %>% data.frame() %>% 
  dplyr::select(mpio, n,matches("prob") ) %>% 
  tibble()


MC_ipm <- function(){
poststrat_MC %>% mutate(
  ipm_MC = map2_dbl(n,prob_ipm, function(ni, prob_e){
    y_empleo =  rbinom(ni, 1, prob = prob_e) 
    mean(y_empleo)
  })) %>% group_by(mpio) %>%
    summarise(ipm_MC = sum((n*ipm_MC))/sum(n))    
}

plan(multisession, workers = 4)
ipm_MC <- furrr::future_imap(1:100,~MC_ipm(),.progress = TRUE)
#ipm_MC <- replicate(100,MC_ipm())

ipm_empleo <- map_df(1:ncol(ipm_MC), function(x) data.frame(t(ipm_MC)[x,])) %>%
group_by(mpio) %>% summarise(ipm_estimado_MC = mean(ipm_MC))

ipm_empleo <- ipm_MC %>% bind_rows() %>% 
  group_by(mpio) %>% 
  summarise(ipm_estimado_MC = mean(ipm_MC))


saveRDS(ipm_empleo, 
    file = "Frecuentista_mpio/COL/Data/ipm.rds")

