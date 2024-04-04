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

################################################################################
# Lectura de base de datos
################################################################################
rm(list = ls())
encuesta_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds")

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") 

byAgrega <- c("dam", "dam2", "area", "sexo", "etnia", 
              "anoest", "edad" )

## Organizando las base de datos por dimension del ipm

names_ipm <- grep(pattern = "nbi", names(encuesta_ipm),value = TRUE)

encuesta_df <- map(setNames(names_ipm,names_ipm),
                   function(y){
                     encuesta_ipm$temp <- as.numeric(encuesta_ipm[[y]])
                     encuesta_ipm %>% 
                       group_by_at(all_of(byAgrega)) %>%
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
names_cov <- c("sexo","area",names_cov[16:19])
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

fit_matviv <- stan_glmer(formula = formula_mod ,
    family = binomial(link = "logit"),
    data = encuesta_df$nbi_matviv_ee,
    cores = 4,
    chains = 4,
    iter = 500
  )

saveRDS(fit_matviv, file = "Modelo_bayes_HxA/COL/Data/fit_matviv.rds")

fit_hacina <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_hacina_ee,
  cores = 4,
  chains = 4,
  iter = 500
)


saveRDS(fit_hacina, file = "Modelo_bayes_HxA/COL/Data/fit_hacina.rds")

fit_tic <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_tic,
  cores = 4,
  chains = 4,
  iter = 500
)

saveRDS(fit_tic, file = "Modelo_bayes_HxA/COL/Data/fit_tic.rds")


fit_agua <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_agua_ee,
  cores = 4,
  chains = 4,
  iter = 500
)


saveRDS(fit_agua, file = "Modelo_bayes_HxA/COL/Data/fit_agua.rds")

fit_saneamiento <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_saneamiento_ee,
  cores = 4,
  chains = 4,
  iter = 500
)


saveRDS(fit_saneamiento, file = "Modelo_bayes_HxA/COL/Data/fit_saneamiento.rds")

fit_energia <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_energia,
  cores = 4,
  chains = 4,
  iter = 500
)

saveRDS(fit_energia, file = "Modelo_bayes_HxA/COL/fit_energia.rds")

fit_salud <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_salud_ee,
  cores = 4,
  chains = 4,
  iter = 500
)

saveRDS(fit_salud, file = "Modelo_bayes_HxA/COL/Data/fit_salud.rds")

fit_empleo <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_empe,
  cores = 4,
  chains = 4,
  iter = 500
)


saveRDS(fit_empleo, file = "Modelo_bayes_HxA/COL/Data/fit_empleo.rds")

fit_educacion <- stan_glmer(
  formula = formula_mod ,
  family = binomial(link = "logit"),
  data = encuesta_df$nbi_educ,
  cores = 4,
  chains = 4,
  iter = 500
)


saveRDS(fit_educacion, file = "Modelo_bayes_HxA/COL/Data/fit_educacion.rds")



