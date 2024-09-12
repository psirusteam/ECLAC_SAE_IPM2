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

source("Frecuentista_mpio_actualizado/0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
encuesta_mrp <-  readRDS("Frecuentista_mpio_actualizado/COL/Data/encuesta_ids.rds") %>% 
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
poststrat_df <- readRDS( "Frecuentista_mpio_actualizado/COL/Data/ipm_bench_censo_modelo.RDS") %>% 
  mutate(sexo = as.character(sexo), 
         depto = str_sub(mpio, 1,2))


# Revisión de NAs ---------------------------------------------------------

# View(poststrat_df)
sum(complete.cases(poststrat_df)) == nrow(poststrat_df)

# Calculo de la pobreza Cepal.  -------------------------------------------

poststrat_df %<>% mutate(yk_lmer = prob_ipm, 
                         yk_bench = prob_ipm*gk)

diseno <- encuesta_mrp %>% mutate(yk_dir = ipm) %>% 
  as_survey_design_(
    ids = ~ upm,
    weights = ~ fep,
    strata = ~ estrato,
    nest = TRUE
  )


## validación nacional.
options(survey.lonely.psu = "adjust")
diseno %>% summarise(Nacional_dir = survey_mean(ipm))
poststrat_df %>% summarise(
  Nacional_lmer = sum(n * yk_lmer) / sum(n/gk),# OJO actualizar depende del metodo
  Nacional_bench = sum(n * yk_bench) / sum(n)
)

###########################################
###########################################
### Validaciones por subgrupo completo  ###
###########################################
###########################################
bynames <-c("sexo","area","edad","depto")
 

plot_subgrupo <- map(
  .x = setNames(bynames, bynames),
  ~ plot_validacion(
    sample_diseno = diseno,
    poststrat = poststrat_df,
    by1 = .x
  )
)

x11()

plot_uni <- plot_subgrupo$depto$gg_plot  /
  (
    plot_subgrupo$sexo$gg_plot + plot_subgrupo$anoest$gg_plot +
      plot_subgrupo$edad$gg_plot + plot_subgrupo$etnia$gg_plot +
      plot_subgrupo$area$gg_plot + plot_subgrupo$discapacitado$gg_plot
  )

ggsave(plot = plot_uni, 
       filename = "Frecuentista_mpio_actualizado/COL/Output/plot_uni2_modelo.jpeg", width = 12, height = 8, units = "in")

#################################################################################
## Comparando modelo ipm modelo de unidad vs ipm modelo para educación y empleo
#################################################################################
ipm_mpio_modelo <- readRDS("Frecuentista_mpio_actualizado/COL/Data/ipm.rds")
ipm_mpio_sin_modelo <- readRDS("Frecuentista_mpio_actualizado/COL/Data/ipm_MC.rds")
ipm_mpio_directo <- readRDS("Frecuentista_mpio_actualizado/COL/Data/Educacion_Empleo_IPM_dir.rds")

ipm_mpio<-
inner_join(
  ipm_mpio_modelo,
  ipm_mpio_sin_modelo, by = "mpio", 
  suffix = c("_modelo_ipm", "_modelo_ipm_ind")
  ) %>% 
  inner_join(ipm_mpio_directo, by = "mpio")

x11()
g1   <-ggplot(ipm_mpio, aes(x = ipm_estimado_MC_modelo_ipm, y = ipm_estimado_MC_modelo_ipm_ind)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",) +
  labs(x = "IPM adjusted by unit model", y = "IPM estimated by dimension", 
       title = "Municipalities of Colombia") +
  theme_bw(20)+ 
  theme(plot.title = element_text(hjust = 0.5)) 


g2   <-ggplot(ipm_mpio, aes(x = IPM , y = ipm_estimado_MC_modelo_ipm_ind)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",) +
  labs(x = "Direct", y = "IPM estimated by dimension", 
       title = "Municipalities of Colombia") +
  theme_bw(20)+ 
  theme(plot.title = element_text(hjust = 0.5)) 

g3   <-ggplot(ipm_mpio, aes(y = ipm_estimado_MC_modelo_ipm, x = IPM)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red",) +
  labs(y = "IPM adjusted by unit model", x = "Direct", 
       title = "Municipalities of Colombia") +
  theme_bw(20)+ 
  theme(plot.title = element_text(hjust = 0.5)) 

x11()
g4 <- g3|g2|g1

# Save the plot as a JPEG file
ggsave(plot = g4, filename = "Frecuentista_mpio_actualizado/COL/Output/Plot_vs_ipms.jpeg", width = 12, height = 8, units = "in")
