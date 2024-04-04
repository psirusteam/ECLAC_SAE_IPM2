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

################################################################################
# Lectura de base de datos
################################################################################
rm(list = ls())
encuesta_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds")

names_ipm <- grep(pattern = "nbi", names(encuesta_ipm),value = TRUE)

cor_muestra <- cor(encuesta_ipm[,names_ipm] %>% mutate_all(as.numeric))

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
  ype = "response",
  allow.new.levels = TRUE
)


# Privación de la educación.

epred_mat_educacion <-
  posterior_epred(
    fit_educacion,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación del empleo y la protección social.


epred_mat_empleo <-
  posterior_epred(
    fit_empleo,
    type = "response",
    allow.new.levels = TRUE
  )


# Acceso al servicio energía eléctrica.

epred_mat_energia <-
  posterior_epred(
    fit_energia,
    type = "response",
    allow.new.levels = TRUE
  )

# Hacinamiento en el hogar.

epred_mat_hacinamiento <-
  posterior_epred(
    fit_hacinamiento,
    type = "response",
    allow.new.levels = TRUE
  )


# Acceso al servicio de Internet.

epred_mat_tic <-
  posterior_epred(
    fit_tic,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación en material de construcción de la vivienda


epred_mat_material <-
  posterior_epred(
    fit_material,
    type = "response",
    allow.new.levels = TRUE
  )


# Privación en saneamiento.

epred_mat_saneamiento <-
  posterior_epred(
    fit_saneamiento,
    type = "response",
    allow.new.levels = TRUE
  )

# Privación en salud
epred_mat_salud <-
  posterior_epred(
    fit_salud,
    type = "response",
    allow.new.levels = TRUE
  )


#########################################################################
## Creando variables dummys: Si carencia (1) y  No carencia (0) 
#########################################################################

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

dim(epred_mat_salud_dummy)
sum_cor <- matrix(0, 9,9)
for(ii in 1:1000){
  sum_cor <- sum_cor + 
    data.frame(
    nbi_matviv_ee = epred_mat_material_dummy[ii,],
    nbi_hacina_ee = epred_mat_hacinamiento_dummy[ii,],
    nbi_tic               = epred_mat_tic_dummy[ii,],
    nbi_agua_ee           = epred_mat_agua_dummy[ii,],
    nbi_saneamiento_ee    = epred_mat_saneamiento_dummy[ii,],
    nbi_energia           = epred_mat_energia_dummy[ii,],
    nbi_salud_ee          = epred_mat_salud_dummy[ii,],
    nbi_empe              = epred_mat_empleo_dummy[ii,],
    nbi_educ              = epred_mat_educacion_dummy[ii,]
  ) %>% cor()
}

list(
## Muestra 
cor_muestra = cor_muestra,
## Modelo Bayes 
cor_SAE = sum_cor/1000) %>% 
openxlsx::write.xlsx("Modelo_bayes_HxA/COL/Data/correlations.xlsx") 

## Error realtivo 
100*((cor_muestra - sum_cor/1000)/cor_muestra)

