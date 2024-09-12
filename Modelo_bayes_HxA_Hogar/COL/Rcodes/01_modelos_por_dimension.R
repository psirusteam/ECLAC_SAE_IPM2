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

encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Encuestas/encuesta_nbi.rds") %>% 
  filter(edad >1, anoest <6)

encuesta_ipm <- encuesta_ipm %>%
  mutate(
    ipm_encuesta = (1 / 12) * (
      as.numeric(nbi_hnolee_ee) +
        as.numeric(nbi_hlogroeduc_ee) +
        as.numeric(nbi_heducninios) +
        as.numeric(nbi_hhacina) +
        as.numeric(nbi_henergia) +
        as.numeric(nbi_htic) +
        as.numeric(nbi_hagua_ee) +
        as.numeric(nbi_hsaneamiento_ee) +
        as.numeric(nbi_hsalud_ee) +
        as.numeric(nbi_hpartemp) +
        as.numeric(nbi_hempe) +
        as.numeric(nbi_hjub)
    ),
    ipm_encuesta = ifelse(ipm_encuesta >= 0.4, "1", "0")
  )

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA_Hogar/COL/Data/statelevel_predictors_df_dam2.rds") 

summary(statelevel_predictors_df)

byAgrega <- c("dam", "dam2", "area", "sexo", "etnia",
              "anoest", "edad")

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
  "nbi_hjub",
  "ipm_encuesta"
)  

n_distinct(encuesta_ipm$anoest)

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
names_cov <- c("sexo","area",names_cov)

efec_aleat <-
  paste0("(1|",
         c("dam", "etnia", "edad", "anoest"),
         ")",
         collapse = " + ")

formula_mod <-
  formula(paste(
    " cbind(yno, ysi) ~",
    efec_aleat,
    "+",
    paste0(names_cov,
           collapse = " + ")
  ))
plan(multisession, workers = 2)


run_bayesian_model <- function(variable, data) {
  fit <- stan_glmer(
    formula = formula_mod,
    family = binomial(link = "logit"),
    data = data[[variable]],
    cores = 2,
    chains = 4,
    iter = 500,
    open_progress = TRUE
  )
  
  saveRDS(fit, file = paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", 
                             variable, ".rds"))
}





for (variable in nbi_hogar[c(3:6)]) {
  cat(variable,"\n")
  run_bayesian_model(variable, encuesta_df)
}

for (variable in nbi_hogar[-c(1:7)]) {
  cat(variable,"\n")
  run_bayesian_model(variable, encuesta_df)
}

# "nbi_hnolee_ee"
# "nbi_hlogroeduc_ee" 
# "nbi_heducninios"   PC Andres esc1
# "nbi_hhacina"       PC Andres virtu1  
# "nbi_henergia"
# "nbi_htic"
# "nbi_hagua_ee"        ok 
# "nbi_hsaneamiento_ee" ok  
# "nbi_hsalud_ee"       ok
# "nbi_hpartemp"       PC Andres virtu2
# "nbi_hempe"
# "nbi_hjub" 
# "ipm_encuesta"

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

run_bayesian_model(nbi_hogar[13], encuesta_df) #ok
modelo_rds <- paste0("Modelo_bayes_HxA_Hogar/COL/Data/Modelo/fit_", nbi_hogar[13], ".rds")
s <- summary(readRDS(modelo_rds)) %>% as.data.frame()
mcmc_rhat(s$Rhat)

