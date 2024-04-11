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
censo_ipm <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Censo/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

################################################################################
## Lectura de variables dummy  
################################################################################

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



chain_q  <-
(1/12)*(epred_mat_dummy_hnolee +
epred_mat_dummy_hlogroeduc +
epred_mat_dummy_heducninios +
epred_mat_dummy_hhacina +
epred_mat_dummy_henergia +
epred_mat_dummy_htic +
epred_mat_dummy_hagua +
epred_mat_dummy_hsaneamiento +
epred_mat_dummy_hsalud +
epred_mat_dummy_hpartemp +
epred_mat_dummy_hempe +
epred_mat_dummy_hjub )
 
saveRDS(chain_q, "Modelo_bayes_HxA_Hogar/COL/Data/Modelo/chain_q.rds")
rm(list = c("epred_mat_dummy_hnolee",
"epred_mat_dummy_hlogroeduc", 
"epred_mat_dummy_heducninios", 
"epred_mat_dummy_hhacina", 
"epred_mat_dummy_henergia", 
"epred_mat_dummy_htic", 
"epred_mat_dummy_hagua", 
"epred_mat_dummy_hsaneamiento", 
"epred_mat_dummy_hsalud",
"epred_mat_dummy_hpartemp", 
"epred_mat_dummy_hempe", 
"epred_mat_dummy_hjub"))

 chain_Ind <- chain_q
 chain_Ind[chain_Ind <= 0.4] <- 0
 chain_Ind[chain_Ind != 0] <- 1
 
 chain_ci <- matrix(0, nrow = nrow(chain_q), ncol = ncol(chain_q))
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
 
estimacion_Nacional <-
 estime_IPM(
   poststrat = censo_ipm,
   chain_ci = chain_ci,
   chain_ind = chain_ind,
   byMap = NULL
 ) %>% data.frame()

estimacion_dam <-
 estime_IPM(
   poststrat = censo_ipm,
   chain_ci = chain_ci,
   chain_ind = chain_ind,
   byMap = "dam"
 ) %>% data.frame()
 
estimacion_dam2 <-
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
    nbi_hnolee = epred_mat_dummy_hnolee,
    nbi_hlogroeduc = epred_mat_dummy_hlogroeduc,
    nbi_heducninios = epred_mat_dummy_heducninios,
    nbi_hhacina = epred_mat_dummy_hhacina,
    nbi_henergia = epred_mat_dummy_henergia,
    nbi_htic = epred_mat_dummy_htic,
    nbi_hagua = epred_mat_dummy_hagua,
    nbi_hsaneamiento = epred_mat_dummy_hsaneamiento,
    nbi_hsalud = epred_mat_dummy_hsalud,
    nbi_hpartemp =  epred_mat_dummy_hpartemp,
    nbi_hempe = epred_mat_dummy_hempe,
    nbi_hjub = epred_mat_dummy_hjub)
 
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
 
 estimado_ipm1 <- map(by_agrega, function(xby){
   
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
 saveRDS(estimado_ipm, file = "Modelo_bayes_HxA/COL/Data/Modelo/estimado_ipm_HA.rds")
 
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
    nbi_hnolee = epred_mat_dummy_hnolee,
    nbi_hlogroeduc = epred_mat_dummy_hlogroeduc,
    nbi_heducninios = epred_mat_dummy_heducninios,
    nbi_hhacina = epred_mat_dummy_hhacina,
    nbi_henergia = epred_mat_dummy_henergia,
    nbi_htic = epred_mat_dummy_htic,
    nbi_hagua = epred_mat_dummy_hagua,
    nbi_hsaneamiento = epred_mat_dummy_hsaneamiento,
    nbi_hsalud = epred_mat_dummy_hsalud,
    nbi_hpartemp =  epred_mat_dummy_hpartemp,
    nbi_hempe = epred_mat_dummy_hempe,
    nbi_hjub = epred_mat_dummy_hjub)
 
 
 
 temp_estimate_mpio <- map_df(temp_epred_mat,
                              function(dummy) {
                                agregado_dim_ipm(poststrat = censo_ipm,
                                                 epredmat = dummy,
                                                 byMap = "dam2") 
                              }, .id = "Indicador")
 
saveRDS(temp_estimate_mpio, "Modelo_bayes_HxA_Hogar/COL/data/Modelo/temp_estimate_mpio.rds")
 
 
 