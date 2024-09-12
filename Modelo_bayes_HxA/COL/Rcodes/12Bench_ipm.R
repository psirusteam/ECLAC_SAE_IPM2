#########################################################
# Proyecto IPM                   #
# Mapas de ipm_final CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez   & Hanwen ZHANG   #
#########################################################
###--- Cleaning R environment ---###

rm(list = ls())
gc()
# Loading required libraries ----------------------------------------------
#################
### Libraries ###
#################
library(tidyverse)
library(sampling)
library(survey)
library(srvyr)
library(magrittr)
select <- dplyr::select

censo_ipm <- readRDS(file = "Frecuentista_mpio_actualizado/COL/Data/censo_ipm_MC.rds") %>% 
  na.omit()

encuesta_ipm <-
  readRDS("Frecuentista_mpio_actualizado/COL/Data/encuesta_ids.rds") %>% 
  mutate(
    ipm = 0.1 * (
      ipm_Material +
        ipm_Saneamiento +
        ipm_Energia +
        ipm_Internet +
        ipm_Agua +
        ipm_Hacinamiento
    ) +
      0.2 * (ipm_educacion +   ipm_Empleo_Aseguramiento),
    ipm = ifelse(ipm >= 0.4, 1, 0)
  )

diseno <- as_survey_design_(
  .data = encuesta_ipm,
  ids = ~ upm,
  weights = ~ fep,
  strata = ~ estrato,
  nest = TRUE
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
names_cov <- c("area", "sexo","edad")

censo_ipm2 <- censo_ipm %>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)

Xdummy <- censo_ipm2 %>% select(matches("\\d$")) %>% 
  transmute_at(vars(matches("_\\d")) ,
            list(num = function(.x) .x*censo_ipm$ipm_final,
                 den = function(.x).x))

colSums(Xdummy)

estimaciones <-
  map(names_cov ,~ censo_ipm %>% group_by_at(all_of(.x)) %>%
        summarise(
         den = sum(n),
         num = sum(ipm_final *n),
         tasa = num/den
        ))

### total

resultados <- map_dfr(names_cov, function(byi) {
  encuesta_ipm %>% mutate_at(vars(byi), as.character) %>% 
    group_by(nivel = .data[[byi]]) %>%
    summarise(
      num = sum(ipm * fep, na.rm = TRUE),
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
summary(gk)
jpeg(file = "Frecuentista_mpio_actualizado/COL/Output/Plot_Bench_ipm.jpeg",width = 1200,
     height = 800, units = "px")
hist(gk)
dev.off()

censo_ipm$gk <- gk
saveRDS(censo_ipm,
        "Frecuentista_mpio_actualizado/COL/Data/ipm_bench_censo.RDS")



