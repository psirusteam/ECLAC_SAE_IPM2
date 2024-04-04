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
library(openxlsx)
source("Modelo_bayes_HxA/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA/0funciones/agregado_dim_ipm.r")
################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

################################################################################
## Lectura de variables dummy  
################################################################################

epred_mat_agua_dummy <-  readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_agua_dummy.rds")
epred_mat_educacion_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_educacion_dummy.rds")
epred_mat_empleo_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_empleo_dummy.rds")
epred_mat_energia_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_energia_dummy.rds")
epred_mat_hacinamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_hacinamiento_dummy.rds")
epred_mat_tic_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_tic_dummy.rds")
epred_mat_material_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_material_dummy.rds")
epred_mat_saneamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_saneamiento_dummy.rds")
epred_mat_salud_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_salud_dummy.rds")

chain_q  <-
  # Vivienda y servicios
  (1 / 16) * (
    epred_mat_material_dummy +
      epred_mat_hacinamiento_dummy +
      epred_mat_energia_dummy +
      epred_mat_tic_dummy
  ) +
  # Salud
  (1 / 12) * (epred_mat_agua_dummy +
                epred_mat_saneamiento_dummy +
                epred_mat_salud_dummy) +
  # Educación
  (1 / 4) * epred_mat_educacion_dummy  +
  (1 / 4) * epred_mat_empleo_dummy

 saveRDS(chain_q, "Modelo_bayes_HxA/COL/Data/chain_q.rds")

 chain_Ind <- chain_q
 chain_Ind[chain_Ind <= 0.4] <- 0
 chain_Ind[chain_Ind != 0] <- 1
 
 chain_ci <- matrix(0,nrow = nrow(chain_q), ncol = ncol(chain_q))
 chain_ci[chain_Ind == 1] <- chain_q[chain_Ind == 1]
 
 numIPM <- t(chain_ci) %>%
   as.data.frame() %>%
   mutate_all(~ . * censo_ipm$n) %>%
   as.matrix()
 
 chain_N <- t(chain_Ind) %>%
   as.data.frame() %>%
   mutate_all(~ . * censo_ipm$n) %>%
   as.matrix()
 
 
 IPM_l <- colSums(numIPM)/sum(censo_ipm$n)
 Nz_l <- colSums(chain_N)
 H_l <- Nz_l/sum(censo_ipm$n)
 A_l <- colSums(numIPM)/Nz_l
 
 datos_chain <- data.frame(IPM_l,H_l,A_l,HA_l = H_l*A_l) %>% 
   slice(1:10)
 rownames(datos_chain) <- paste0("l = ", 1:10)
 datos_chain
 
 data.frame(H = mean(H_l),
            H_sd = sd(H_l),
            A = mean(A_l),
            A_sd = sd(A_l),
            IPM = mean(IPM_l),
            IPM_sd = sd(IPM_l))
 
 
 estime_IPM(
   poststrat = censo_ipm,
   chain_ci = chain_ci,
   chain_ind = chain_ind,
   byMap = NULL
 ) %>% data.frame()
 
 estime_IPM(
   poststrat = censo_ipm,
   chain_ci = chain_ci,
   chain_ind = chain_ind,
   byMap = "dam"
 ) %>% data.frame()
 
 estime_IPM(
   poststrat = censo_ipm,
   chain_ci = chain_ci,
   chain_ind = chain_ind,
   byMap = "dam2"
 ) %>% data.frame()
 
 ###################################################
 ## Resultado por dimensiones ######################
 ###################################################
 epred_mat_dim <- list(
   Material = epred_mat_material_dummy,
   Hacinamienot =    epred_mat_hacinamiento_dummy ,
   Agua =  epred_mat_agua_dummy, 
   Saneamiento =  epred_mat_saneamiento_dummy, 
   Energia = epred_mat_energia_dummy ,
   Internet = epred_mat_tic_dummy,
   Educacion = epred_mat_educacion_dummy , 
   Empleo =  epred_mat_empleo_dummy,
   Salud =  epred_mat_salud_dummy)
 
 aux_agregado <- function(dat, byx = NULL, censo) {
   temp_estimate <- map_df(dat,
                           function(dummy) {
                             agregado_dim_ipm(poststrat = censo,
                                              epredmat = dummy,
                                              byMap = byx)
                           }, .id = "Indicador")
   
   inner_join(
     spread(
       temp_estimate %>% select(-estimate_se),
       key = "Indicador",
       value = "estimate"
     ),
     spread(
       temp_estimate %>% select(-estimate),
       key = "Indicador",
       value = "estimate_se"
     ) %>%
       rename_if(
         is.numeric,
         .funs = function(x)
           paste0(x, "_se")
       )
   )
   
 }
 
 #############################################################
 
 by_agrega  <- c("dam", "dam2",  "area",
                 "sexo",  "edad",  "etnia",
                 "anoest")
 
 estimado_ipm1 <-map(by_agrega, function(xby){
   
   paso_ipm <-  estime_IPM(
     poststrat = censo_ipm,
     chain_ci = chain_ci,
     chain_ind = chain_ind,
     byMap = xby
   ) %>% data.frame() %>% data.frame()
   
   paso_dim <- aux_agregado(epred_mat_dim, xby, censo_ipm) %>% data.frame()
   
   inner_join(paso_dim,paso_ipm)
   
 })
 
 names(estimado_ipm1) <- by_agrega
 
 
 by_agrega2 <- t(combn(by_agrega[-c(1:2)], 2)) %>% cbind("dam")
 
 estimado_ipm2 <- map(1:nrow(by_agrega2), function(ii) {
   paso_ipm <- estime_IPM(poststrat = censo_ipm,
                          chain_ci = chain_ci,
                          chain_ind = chain_ind,
                          byMap = by_agrega2[ii,]) %>% data.frame()
   
   paso_dim <-
     aux_agregado(epred_mat_dim, by_agrega2[ii, ], censo_ipm) %>% data.frame()
   
   inner_join(paso_dim, paso_ipm)
 })
 
 names(estimado_ipm2) <- apply(by_agrega2, 1, paste0, collapse = "_")
 
 estimado_ipm <- c(estimado_ipm1, estimado_ipm2) 
 saveRDS(estimado_ipm, file = "Modelo_bayes_HxA/COL/Data/estimado_ipm_HA.rds")
 
 ## creando libro de excel. 
 wb <- createWorkbook()
 hojas <- names(estimado_ipm)
 ## Creando la hoja del índice. 
 addWorksheet(wb, "Indice")
 ## Creando la tablas de índice 
 datos <- data.frame(Orden = 1:length(hojas),
                     Titulo = NA)
 ## agregando el indice al libro de excel
 writeDataTable(
   wb = wb,
   x = datos,
   sheet = "Indice",
   rowNames = FALSE,
   tableStyle = "TableStyleLight9"
 )
 
 ## Agregando los resultados al libro de excel hoja por hoja
 for(ii in 1:length(hojas)) {
   addWorksheet(wb, hojas[ii])
   writeData(
     wb = wb,
     x = estimado_ipm[[hojas[ii]]],
     sheet = hojas[ii],
     rowNames = FALSE
   )
   ## agregando el nombre de la hoja al índice
   writeFormula(
     wb,
     "Indice",
     startRow = 1 + ii,
     startCol = 2,
     x = makeHyperlinkString(
       sheet = hojas[ii],
       row = 1,
       col = 2,
       text = hojas[ii]
     )
   )
   
 }
 
 saveWorkbook(wb, file = "Modelo_bayes_HxA/COL/Output/estimacion_ipm.xlsx",
              overwrite = TRUE)
 openxlsx::openXL("Modelo_bayes_HxA/COL/Output/estimacion_ipm.xlsx")
 
 ############ Estimaciones por dimension del IPM #####################
 
 temp_epred_mat <- list(
   Material = epred_mat_material_dummy,
   Hacinamienot =    epred_mat_hacinamiento_dummy ,
   Agua =  epred_mat_agua_dummy, 
   Saneamiento =  epred_mat_saneamiento_dummy, 
   Energia = epred_mat_energia_dummy ,
   Internet = epred_mat_tic_dummy,
   Educacion = epred_mat_educacion_dummy , 
   Empleo =  epred_mat_empleo_dummy,
   Salud = epred_mat_salud_dummy)
 
 
 
 temp_estimate_mpio <- map_df(temp_epred_mat,
                              function(dummy) {
                                agregado_dim_ipm(poststrat = censo_ipm,
                                                 epredmat = dummy,
                                                 byMap = "dam2") 
                              }, .id = "Indicador")
 
 saveRDS(temp_estimate_mpio, "Modelo_bayes_HxA/COL/data/temp_estimate_mpio.rds")
 
 
 