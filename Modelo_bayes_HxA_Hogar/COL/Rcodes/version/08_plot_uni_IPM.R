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
library(parallel)
library(openxlsx)
library(survey)
library(srvyr)
source("Modelo_bayes_HxA_Hogar/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA_Hogar/0funciones/agregado_dim_ipm.r")
source("Modelo_bayes_HxA_Hogar/0funciones/IPM_descomponer.R")
################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Censo/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")


encuesta_ipm <-
  readRDS("Modelo_bayes_HxA_Hogar/COL/Data/Encuestas/encuesta_nbi.rds") %>% 
  filter(edad >1, anoest <6)

encuesta_ipm <- encuesta_ipm %>%
  mutate_at(vars(matches("nbi")), as.numeric)
  
vars_nbi_hogar <-
  c("nbi_hnolee_ee" = 1/12 ,
    "nbi_hlogroeduc_ee"  = 1/12 ,
    "nbi_heducninios"    = 1/12 ,
    "nbi_hhacina"          = 1/12 ,
    "nbi_henergia" = 1/12 ,
    "nbi_htic" = 1/12 ,
    "nbi_hagua_ee"          = 1/12 ,
    "nbi_hsaneamiento_ee"  = 1/12 ,
    "nbi_hsalud_ee"        = 1/12 ,
    "nbi_hpartemp"        = 1/12 ,
    "nbi_hempe" = 1/12 ,
    "nbi_hjub"  = 1/12 )

resul_nacional <-
  calculate_ipm_sample(
    k = 40,
    set_data = encuesta_ipm,
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep"
  )

resul_dam <-
  calculate_ipm_by(
    group_var = "dam",
    k = 40,
    set_data = encuesta_ipm,
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep"
  )

resul_area <-
  calculate_ipm_by(
    group_var = "area",
    k = 40,
    set_data = encuesta_ipm,
    vars_nbi = vars_nbi_hogar,
    weight_fep = "fep"
  )

################################################################################
## Nacional 
################################################################################
ipm_HA_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_nacional.xlsx")
contribuciones_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_nacional.xlsx",
                      sheet = "contribuciones")

tab_dir <- resul_nacional$ipm_HA %>% select(H,A,M0, matches("_low|_upp")) %>% 
  pivot_longer(cols = everything(), names_to = "elemento", values_to = "value") %>% 
  filter(value <1) %>% 
  separate(col = "elemento", into = c("elemento", "tipo"),sep = "_" ) %>% 
  mutate(tipo = ifelse(is.na(tipo), "estimacion", tipo)) %>% 
  pivot_wider(names_from = tipo, values_from = value) %>% 
  mutate(tipo = "sample")


tab_censo <- ipm_HA_censo %>%  select(matches("media")) %>% 
  pivot_longer(cols = everything(), names_to = "elemento", values_to = "estimacion") %>% 
  separate(col = "elemento", into = c("elemento", "tipo"),sep = "_" ) %>% 
  mutate(tipo = "censo")

# Crear el gráfico
p1 <- ggplot(tab_censo, aes(x = elemento, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Estimaciones con Intervalo de Confianza para M0, H y A",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  ) +
  geom_jitter(data = tab_dir,
              aes(x = elemento, y = estimacion, color = tipo),
              size = 3,
              position = position_jitter(width = 0.2, height = 0)) +  
  geom_errorbar(data = tab_dir,aes(ymin = low, ymax = upp), width = 0.2) +
  theme_minimal()  # Usar un tema minimalista




tab_dir <- resul_nacional$contribuciones %>% data.frame(estimacion = .) %>% 
  tibble::rownames_to_column(var = "nbi") %>% 
  separate(col = "nbi", into = c("tipo", "nbi"),sep = "_" ) %>% 
  mutate(tipo = "sample")

tab_censo <- contribuciones_censo %>%  select(matches("media")) %>% 
  pivot_longer(cols = everything(), names_to = "nbi", values_to = "estimacion") %>% 
  separate(col = "nbi", into = c("nbi", "tipo"),sep = "_" ) %>% 
  mutate(tipo = "censo")

tab_plot <- bind_rows(tab_dir,tab_censo)

p2 <- ggplot(tab_plot, aes(x = nbi, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribución",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  ) +  theme_minimal() 
p3 <- p2/p1

ggsave(plot = p3,
  filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/nacional.png",
       width = 18,height = 14
        )
################################################################################
## Dam 
################################################################################
ipm_HA_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_dam.xlsx")
contribuciones_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_dam.xlsx",
                      sheet = "contribuciones")

tab_dir <- resul_dam %>% map_df( ~ .x$ipm_HA, .id = "dam") %>% 
  select(dam, H, A, M0, matches("_low|_upp")) %>%
  pivot_longer(cols = c( "H", "A", "M0", matches("_low|_upp")),
               names_to = "elemento",
               values_to = "value") %>%  
  filter(value <1) %>% 
  separate(col = "elemento", into = c("elemento", "tipo"),sep = "_" ) %>% 
  mutate(tipo = ifelse(is.na(tipo), "estimacion", tipo)) %>% 
  pivot_wider(names_from = tipo, values_from = value) %>% 
  mutate(tipo = "sample")


tab_censo <- ipm_HA_censo %>%  select(dam,matches("media")) %>% 
  pivot_longer(cols = c( "H_media"  , "A_media"  ,  "M0_media"), names_to = "elemento", values_to = "estimacion") %>% 
  separate(col = "elemento", into = c("elemento", "tipo"),sep = "_" ) %>% 
  mutate(tipo = "censo")

# Crear el gráfico
p1 <- ggplot(tab_censo, aes(x = dam, y = estimacion,color =  tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Estimaciones con Intervalo de Confianza para M0, H y A",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  ) +
  geom_jitter(data = tab_dir,
              aes(x = dam, y = estimacion,color =  tipo),
              size = 3,
              position = position_jitter(width = 0.2, height = 0)) +  
  geom_errorbar(data = tab_dir,aes(ymin = low, ymax = upp), width = 0.2) +
  theme_bw(base_size = 15) + facet_grid(elemento~.,scales = "free_y")


ggsave(plot = p1,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/ipm_dam.png",
       width = 20,height = 14
)

tab_dir <- resul_dam %>% map_dfr( ~ .x$contribuciones, .id = "dam")  %>% 
  pivot_longer(cols = c( matches("nbi")),
               names_to = "nbi",
               values_to = "estimacion") %>% 
   separate(col = "nbi", into = c("tipo", "nbi"),sep = "_" ) %>% 
  mutate(tipo = "sample")

tab_censo <- contribuciones_censo %>%  select(dam, matches("media")) %>%
  pivot_longer(
    cols = matches("media"),
             names_to = "nbi", values_to = "estimacion") %>%
      separate(
        col = "nbi",
        into = c("nbi", "tipo"),
        sep = "_"
      ) %>%
      mutate(tipo = "censo")
    
tab_plot <- bind_rows(tab_dir,tab_censo)

p21 <- ggplot(tab_plot %>% filter(nbi %in% c("hnolee" ,  "hlogroeduc" ,  "heducninios")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribución",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")

p22 <- ggplot(tab_plot %>% filter(nbi %in% c("hhacina"   ,   "henergia"   ,  "htic")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribución",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")

p23 <- ggplot(tab_plot %>% filter(nbi %in% c("hagua"  ,      "hsaneamiento", "hsalud")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribución",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")

p24 <- ggplot(tab_plot %>% filter(nbi %in% c("hpartemp"  ,   "hempe"   ,     "hjub")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribución",
    x = "Elemento",
    y = "Estimación",
    color = "Tipo"
  ) +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")

ggsave(plot = p21,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion1_dam.png",
       width = 20,height = 14
)

ggsave(plot = p22,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion2_dam.png",
       width = 20,height = 14
)

ggsave(plot = p23,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion3_dam.png",
       width = 20,height = 14
)

ggsave(plot = p24,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion4_dam.png",
       width = 20,height = 14
)


p3 <- ggplot(tab_plot %>% mutate(dam2 = paste0(dam,"_", tipo) ), 
       aes(x = dam, y = estimacion, fill = nbi)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw(15) +
  facet_grid(tipo~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave(plot = p3,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion_dam.png",
       width = 20,height = 14
)
