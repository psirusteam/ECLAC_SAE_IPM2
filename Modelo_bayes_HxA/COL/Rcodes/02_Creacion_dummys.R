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

################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
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

fit_agua <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_agua.rds")
fit_educacion <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_educacion.rds")
fit_empleo <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_empleo.rds")
fit_energia <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_energia.rds")
fit_hacinamiento <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_hacina.rds")
fit_tic <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_tic.rds")
fit_material <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_matviv.rds")
fit_saneamiento <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_saneamiento.rds")
fit_salud <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_salud.rds")

################################################################################
## Predicción del modelo
################################################################################

# Privación de acceso al agua potable. 

epred_mat_agua <- posterior_epred(
  fit_agua,
  newdata = poststrat_df,
  type = "response",
  allow.new.levels = TRUE
)


# Privación de la educación.

epred_mat_educacion <-
  posterior_epred(
    fit_educacion,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación del empleo y la protección social.


epred_mat_empleo <-
  posterior_epred(
    fit_empleo,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )


# Acceso al servicio energía eléctrica.

epred_mat_energia <-
  posterior_epred(
    fit_energia,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )

# Hacinamiento en el hogar.

epred_mat_hacinamiento <-
  posterior_epred(
    fit_hacinamiento,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )


# Acceso al servicio de Internet.

epred_mat_tic <-
  posterior_epred(
    fit_tic,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación en material de construcción de la vivienda


epred_mat_material <-
  posterior_epred(
    fit_material,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación en saneamiento.

epred_mat_saneamiento <-
  posterior_epred(
    fit_saneamiento,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )

# Privación en salud
epred_mat_salud <-
  posterior_epred(
    fit_salud,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )




saveRDS(epred_mat_agua, "Modelo_bayes_HxA/COL/Data/epred_mat_agua.rds")
saveRDS(epred_mat_educacion, "Modelo_bayes_HxA/COL/Data//epred_mat_educacion.rds")
saveRDS(epred_mat_empleo, "Modelo_bayes_HxA/COL/Data//epred_mat_empleo.rds")
saveRDS(epred_mat_energia, "Modelo_bayes_HxA/COL/Data//epred_mat_energia.rds")
saveRDS(epred_mat_hacinamiento, "Modelo_bayes_HxA/COL/Data//epred_mat_hacinamiento.rds")
saveRDS(epred_mat_tic, "Modelo_bayes_HxA/COL/Data/epred_mat_tic.rds")
saveRDS(epred_mat_material, "Modelo_bayes_HxA/COL/Data/epred_mat_material.rds")
saveRDS(epred_mat_saneamiento, "Modelo_bayes_HxA/COL/Data/epred_mat_saneamiento.rds")
saveRDS(epred_mat_salud, "Modelo_bayes_HxA/COL/Data/epred_mat_salud.rds")


## Creando variables dummys: Si carencia (1) y  No carencia (0) 

# Privación de acceso al agua potable. 

epred_mat_agua_dummy <-
  rbinom(n = nrow(epred_mat_agua) * ncol(epred_mat_agua) , 1,
         epred_mat_agua)

epred_mat_agua_dummy <- matrix(
  epred_mat_agua_dummy,
  nrow = nrow(epred_mat_agua),
  ncol = ncol(epred_mat_agua)
)

# Privación de la educación.

epred_mat_educacion_dummy <-
  rbinom(n = nrow(epred_mat_educacion) * ncol(epred_mat_educacion) ,
         1,
         epred_mat_educacion)

epred_mat_educacion_dummy <- matrix(
  epred_mat_educacion_dummy,
  nrow = nrow(epred_mat_educacion),
  ncol = ncol(epred_mat_educacion)
)

# Acceso al servicio energía eléctrica 

epred_mat_energia_dummy <-
  rbinom(n = nrow(epred_mat_energia) * ncol(epred_mat_energia) ,
         1,
         epred_mat_energia)

epred_mat_energia_dummy <- matrix(
  epred_mat_energia_dummy,
  nrow = nrow(epred_mat_energia),
  ncol = ncol(epred_mat_energia)
)


# Hacinamiento en el hogar.
epred_mat_hacinamiento_dummy <-
  rbinom(
    n = nrow(epred_mat_hacinamiento) * ncol(epred_mat_hacinamiento) ,
    1,
    epred_mat_hacinamiento
  )

epred_mat_hacinamiento_dummy <-
  matrix(
    epred_mat_hacinamiento_dummy,
    nrow = nrow(epred_mat_hacinamiento),
    ncol = ncol(epred_mat_hacinamiento)
  )

# Acceso al servicio de Internet.
epred_mat_tic_dummy <-
  rbinom(n = nrow(epred_mat_tic) * ncol(epred_mat_tic) ,
         1,
         epred_mat_tic)

epred_mat_tic_dummy <- matrix(
  epred_mat_tic_dummy,
  nrow = nrow(epred_mat_tic),
  ncol = ncol(epred_mat_tic)
)

# Privación en material de construcción de la vivienda 

epred_mat_material_dummy <-
  rbinom(n = nrow(epred_mat_material) * ncol(epred_mat_material) ,
         1,
         epred_mat_material)

epred_mat_material_dummy <- matrix(
  epred_mat_material_dummy,
  nrow = nrow(epred_mat_material),
  ncol = ncol(epred_mat_material)
)

# Privación en saneamiento. 

epred_mat_saneamiento_dummy <-
  rbinom(n = nrow(epred_mat_saneamiento) * ncol(epred_mat_saneamiento) ,
         1,
         epred_mat_saneamiento)

epred_mat_saneamiento_dummy <- matrix(
  epred_mat_saneamiento_dummy,
  nrow = nrow(epred_mat_saneamiento),
  ncol = ncol(epred_mat_saneamiento)
)

# Privación del empleo y la protección social. 

epred_mat_empleo_dummy <-
  rbinom(n = nrow(epred_mat_empleo) * ncol(epred_mat_empleo) ,
         1,
         epred_mat_empleo)

epred_mat_empleo_dummy <- matrix(
  epred_mat_empleo_dummy,
  nrow = nrow(epred_mat_empleo),
  ncol = ncol(epred_mat_empleo)
)


# Privación salud. 

epred_mat_salud_dummy <-
  rbinom(n = nrow(epred_mat_salud) * ncol(epred_mat_salud) ,
         1,
         epred_mat_salud)

epred_mat_salud_dummy <- matrix(
  epred_mat_salud_dummy,
  nrow = nrow(epred_mat_salud),
  ncol = ncol(epred_mat_salud)
)



saveRDS(epred_mat_agua_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_agua_dummy.rds")
saveRDS(epred_mat_educacion_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_educacion_dummy.rds")
saveRDS(epred_mat_empleo_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_empleo_dummy.rds")
saveRDS(epred_mat_energia_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_energia_dummy.rds")
saveRDS(epred_mat_hacinamiento_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_hacinamiento_dummy.rds")
saveRDS(epred_mat_tic_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_tic_dummy.rds")
saveRDS(epred_mat_material_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_material_dummy.rds")
saveRDS(epred_mat_saneamiento_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_saneamiento_dummy.rds")
saveRDS(epred_mat_salud_dummy, "Modelo_bayes_HxA/COL/Data/epred_mat_salud_dummy.rds")

