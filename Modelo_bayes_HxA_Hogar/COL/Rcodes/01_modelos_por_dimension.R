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
library(haven)
library(rstantools)
library(bayesplot)

################################################################################
# Lectura de base de datos
################################################################################
rm(list = ls())
encuesta_ipm <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Encuestas/encuesta_nbi.rds")

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA_Hogar/COL/Data/statelevel_predictors_df_dam2.rds") 

byAgrega <- c("dam", "dam2", "area", "sexo", "etnia", 
              "anoest", "edad" )

## Organizando las base de datos por dimension del ipm

nbi_hogar <- c(
  "nbi_hnolee_ee" ,
  "nbi_hlogroeduc_ee" ,
  "nbi_heducninios" ,
  "nbi_hhacina" ,
  "nbi_henergia" ,
  "nbi_htic" ,
  "nbi_hagua_ee" ,
  "nbi_hsaneamiento_ee" ,
  "nbi_hsalud_ee" ,
  "nbi_hpartemp" ,
  "nbi_hempe" ,
  "nbi_hjub"
)  

encuesta_df <- map(setNames(nbi_hogar,nbi_hogar),
                   function(y){
                     encuesta_ipm$temp <- as.numeric(encuesta_ipm[[y]])
                     encuesta_ipm %>% 
                       group_by_at((byAgrega)) %>%
                       summarise(n = n(),
                                 yno = sum(temp),
                                 ysi = n - yno, .groups = "drop") %>% 
                       inner_join(statelevel_predictors_df,
                                  by = c("dam","dam2"))
                   })

### Definiendo el modelo multinivel.

# Para cada dimensión que compone el IPM se ajusta el siguiente modelo
# mostrado en el script. 

names_cov <-  statelevel_predictors_df %>%
  dplyr::select(-dam,-dam2) %>%
  names()
names_cov <- c("sexo","area",names_cov[16:22])
efec_aleat <-
  paste0("(1|",
         c("dam", "etnia"),
         ")",
         collapse = "+")

formula_mod <-
  formula(paste(
    " cbind(yno, ysi) ~",
    efec_aleat,
    "+",
    paste0(names_cov,
           collapse = " + ")
  ))
plan(multisession, workers = 4)


run_bayesian_model <- function(variable, data) {
  fit <- stan_glmer(
    formula = formula_mod,
    family = binomial(link = "logit"),
    data = data[[variable]],
    cores = 4,
    chains = 4,
    iter = 500,
    open_progress = TRUE
  )
  
  saveRDS(fit, file = paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", 
                             variable, ".rds"))
}





for (variable in nbi_hogar[c(2:6)]) {
  run_bayesian_model(variable, encuesta_df)
}

for (variable in nbi_hogar[-c(1:6,10)]) {
  run_bayesian_model(variable, encuesta_df)
}


run_bayesian_model(nbi_hogar[1], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[1], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[2], encuesta_df) #ok 
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[2], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)


run_bayesian_model(nbi_hogar[3], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[3], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)


run_bayesian_model(nbi_hogar[4], encuesta_df) # Corriendo 
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[4], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)


run_bayesian_model(nbi_hogar[5], encuesta_df) # Corriendo 
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[5], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[6], encuesta_df) # Corriendo 
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[6], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[7], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[7], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[8], encuesta_df) # Corriendo
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[8], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[9], encuesta_df)  #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[9], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[10], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[10], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[11], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[11], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

run_bayesian_model(nbi_hogar[12], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[12], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)



