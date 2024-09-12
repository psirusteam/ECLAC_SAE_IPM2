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
library(nortest)
library(lme4)
library(tidyverse)
library(rstan)
library(rstanarm)
library(magrittr)

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds")
encuesta_ipm <- encuesta_ipm %>% mutate_at(vars(matches("nbi")), as.numeric) 

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds")

byAgrega <- c("dam", "dam2", "area", "sexo", "etnia",
              "anoest", "edad")

encuesta_ipm %<>% mutate(
  ipm_encuesta =  (1 / 16) * (nbi_matviv_ee +
                                nbi_hacina_ee +
                                nbi_energia +
                                nbi_tic) +
    # Salud
    (1 / 12) * (nbi_agua_ee +
                  nbi_saneamiento_ee +
                  nbi_salud_ee) +
    # Educación
    (1 / 4) * nbi_educ  +
    (1 / 4) * nbi_empe,
  ipm_encuesta = ifelse(ipm_encuesta >= 0.4, 1, 0)
)

encuesta_df <- encuesta_ipm %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(
    n = n(),
    yno = sum(ipm_encuesta),
    ysi = n - yno,
    .groups = "drop"
  ) %>%
  inner_join(statelevel_predictors_df,
             by = c("dam", "dam2"))


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



fit_ipm <- stan_glmer(formula = formula_mod ,
                         family = binomial(link = "logit"),
                         data = encuesta_df,
                         cores = 4,
                         chains = 4,
                         iter = 500
)

saveRDS(fit_ipm, file = "Modelo_bayes_HxA/COL/Data/fit_ipm.rds")

# Agregando encuesta ------------------------------------------------------

