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
library(sampling)
library(survey)
library(srvyr)
source("Modelo_bayes_HxA/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA/0funciones/agregado_dim_ipm.r")
################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")
dim(censo_ipm)

################################################################################
## Lectura de variables dummy  
################################################################################
chain_q <- readRDS("Modelo_bayes_HxA/COL/Data/chain_q.rds")

chain_Ind <- chain_q
chain_Ind[chain_Ind <= 0.4] <- 0
chain_Ind[chain_Ind != 0] <- 1

chain_ci <- matrix(0,nrow = nrow(chain_q), ncol = ncol(chain_q))
chain_ci[chain_Ind == 1] <- chain_q[chain_Ind == 1]

numIPM <- t(chain_ci) %>%
  as.data.frame() %>%
  mutate_all(~ . * censo_ipm$n) %>%
  as.matrix()

IPM_l <- (numIPM)/(censo_ipm$n)
censo_ipm$ipm <- rowMeans(IPM_l)

################################################################################
## Lectura de la encuesta
################################################################################
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds") 
encuesta_ipm <- encuesta_ipm %>% mutate_at(vars(matches("nbi")), as.numeric) %>% 
  filter(edad >1)

names(encuesta_ipm)
encuesta_ipm %<>% transmute(
  dam, dam2, area, etnia,anoest,sexo, edad, fep,
  ipm_encuesta =  (1 / 16) * (
  nbi_matviv_ee +
    nbi_hacina_ee +
    nbi_energia +
    nbi_tic
) +
  # Salud
  (1 / 12) * (nbi_agua_ee +
                nbi_saneamiento_ee +
                nbi_salud_ee) +
  # Educación
  (1 / 4) * nbi_educ  +
  (1 / 4) * nbi_empe, 
ipm_encuesta = ifelse(ipm_encuesta >= 0.4, 1, 0))  
  

diseno <- as_survey_design_(
  .data = encuesta_ipm,
  weights = ~ fep,
)



###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
names_cov <- names(censo_ipm)
names_cov <- names_cov[names_cov %in% names(encuesta_ipm)]

num_cat_censo <- apply(censo_ipm[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

num_cat_sample <- apply(encuesta_ipm[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

names_cov <- names_cov[num_cat_censo==num_cat_sample]
names_cov <- c( "area"  , "edad"  , "sexo")

censo_ipm2 <- censo_ipm %>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)

Xdummy <- censo_ipm2 %>% select(matches("\\d$")) %>% 
  transmute_at(vars(matches("_\\d")) ,
               list(num = function(.x) .x*censo_ipm$ipm,
                    den = function(.x).x))

colSums(Xdummy)

estimaciones <-
  map(names_cov ,~ censo_ipm %>% group_by_at(all_of(.x)) %>%
        summarise(
          den = sum(n),
          num = sum(ipm *n),
          tasa = num/den
        ))

### total

resultados <- map_dfr(names_cov, function(byi) {
  encuesta_ipm %>% mutate_at(vars(byi), as.character) %>% 
    group_by(nivel = .data[[byi]]) %>%
    summarise(
      num = sum(ipm_encuesta * fep, na.rm = TRUE),
      den = sum(fep, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(covariate = byi) 
  
}) 

resultados %<>% pivot_longer(
  cols = c("num", "den"),
  names_to = "termino",
  values_to = "valor"
) %>%
  transmute(covariate = paste0(covariate, "_",nivel, "_", termino), valor)

total <- setNames(resultados$valor,resultados$covariate)


gk <- calib(Xs = Xdummy[,names(total)], 
            d = censo_ipm$n,
            total = total,
            method="linear") 

checkcalibration(Xs = Xdummy[,names(total)], 
                 d = censo_ipm$n,
                 total = total,
                 g = gk)
x11()

summary(gk)
jpeg(file = "Modelo_bayes_HxA/COL/Output/Plot_Bench_ipm_8modelos.jpeg",width = 1200,
     height = 800, units = "px")
hist(gk)
dev.off()

censo_ipm$gk <- gk
saveRDS(censo_ipm,
        "Modelo_bayes_HxA/COL/Data/ipm_bench_censo.RDS")




