#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())

# Librerías

library(tidyverse)
library(patchwork)
library(survey)
library(srvyr)

source("Modelo_bayes_HxA/0funciones/funciones_mrp.R")


# Loading data ------------------------------------------------------------
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA/COL/Data/encuesta_nbi.rds") 
encuesta_ipm <- encuesta_ipm %>% mutate_at(vars(matches("nbi")), as.numeric) %>% 
  filter(edad >1)

names(encuesta_ipm)
encuesta_ipm %<>% transmute(
  dam, dam2, area, etnia,anoest, edad, fep,sexo,
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



poststrat_df <- readRDS( "Modelo_bayes_HxA/COL/Data/ipm_bench_censo.RDS") 
  

# Revisión de NAs ---------------------------------------------------------

# View(poststrat_df)
sum(complete.cases(poststrat_df)) == nrow(poststrat_df)

# Calculo de la pobreza Cepal.  -------------------------------------------

poststrat_df %<>% mutate(yk_lmer = ipm    , 
                         yk_bench = ipm    *gk)


diseno <- as_survey_design_(
  .data = encuesta_ipm,
  weights = ~ fep,
) %>% mutate(yk_dir = ipm_encuesta)


## validación nacional.
options(survey.lonely.psu = "adjust")
diseno %>% summarise(Nacional_dir = survey_mean(ipm_encuesta))
poststrat_df %>% summarise(
  Nacional_lmer = sum(n * yk_lmer) / sum(n),
  Nacional_bench = sum(n * yk_bench) / sum(n*gk)
)

###########################################
###########################################
### Validaciones por subgrupo completo  ###
###########################################
###########################################
bynames <-c("sexo","area","edad","dam", "etnia", "anoest")
 

plot_subgrupo <- map(
  .x = setNames(bynames, bynames),
  ~ plot_validacion(
    sample_diseno = diseno,
    poststrat = poststrat_df,
    by1 = .x
  )
)

x11()

plot_uni <- plot_subgrupo$dam$gg_plot  /
  (
    plot_subgrupo$sexo$gg_plot + plot_subgrupo$anoest$gg_plot +
      plot_subgrupo$edad$gg_plot + plot_subgrupo$etnia$gg_plot +
      plot_subgrupo$area$gg_plot + plot_subgrupo$discapacitado$gg_plot
  )
plot_uni
ggsave(
  plot = plot_uni,
  filename = "Modelo_bayes_HxA/COL/Output/plot_uni_8modelos.jpeg",
  width = 18,
  height = 14,
  units = "in"
)



