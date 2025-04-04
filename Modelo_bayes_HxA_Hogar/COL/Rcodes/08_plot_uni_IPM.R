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
  pivot_longer(cols = everything(),
               names_to = "elemento",
               values_to = "estimacion") %>%
  separate(col = "elemento",
           into = c("elemento", "tipo"),
           sep = "_") %>%
  mutate(tipo = "censo")

# Crear el gráfico
p1 <- ggplot(tab_censo, aes(x = elemento, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Add points for the estimates
  labs(
    title = "Results of the estimation of the M0, H, and A",
    x = "Element",
    y = "Estimate",
    color = ""
  ) +
  geom_jitter(data = tab_dir,
              aes(x = elemento, y = estimacion, color = tipo),
              size = 3,
              position = position_jitter(width = 0.2, height = 0)) +  
  # geom_errorbar(data = tab_dir, aes(ymin = low, ymax = upp), width = 0.2) +
  theme_minimal() +  # Use a minimalist theme
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )



tab_dir <- resul_nacional$contribuciones %>% data.frame() %>% 
  # tibble::rownames_to_column(var = "nbi") %>% 
  separate(col = "nbi", into = c("tipo", "nbi"),sep = "_" ) %>% 
  mutate(tipo = "sample") %>% rename(estimacion = estimado)

tab_censo <- contribuciones_censo %>%  select(matches("media")) %>% 
  pivot_longer(cols = everything(), names_to = "nbi", values_to = "estimacion") %>% 
  separate(col = "nbi", into = c("nbi", "tipo"),sep = "_" ) %>% 
  mutate(tipo = "censo")

tab_plot <- bind_rows(tab_dir,tab_censo)

p2 <- ggplot(tab_plot, aes(x = nbi, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribution",
    x = "Element",
    y = "Estimate",
    color = ""
  ) +  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
p3 <- p2/p1

ggsave(plot = p1,
  filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/Nacional_HA.jpeg",
       width = 18,height = 14
        )
ggsave(plot = p2,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/Nacional_indicadores.png",
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
    title = "Results of the estimation of the M0, H, and A",
    x = "DAM",
    y = "Estimate",
    color = ""
  ) +
  geom_jitter(data = tab_dir,
              aes(x = dam, y = estimacion,color =  tipo),
              size = 3,
              position = position_jitter(width = 0.2, height = 0)) +  
  # geom_errorbar(data = tab_dir,aes(ymin = low, ymax = upp), width = 0.2) +
  theme_bw(base_size = 15) + facet_grid(elemento~.,scales = "free_y")


ggsave(plot = p1,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/ipm_dam.jpeg",
       width = 20,height = 14
)

tab_dir <- resul_dam %>% map_dfr( ~ .x$contribuciones, .id = "dam")  %>% 
   separate(col = "nbi", into = c("tipo", "nbi"),sep = "_" ) %>% 
  mutate(tipo = "sample") %>% rename(estimacion = estimado    )

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
    title = "Contribution",
    x = "DAM",
    y = "Estimation",
    color = ""
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y") +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

p22 <- ggplot(tab_plot %>% filter(nbi %in% c("hhacina"   ,   "henergia"   ,  "htic")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribution",
    x = "Elemento",
    y = "Estimation",
    color = ""
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")+
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

p23 <- ggplot(tab_plot %>% filter(nbi %in% c("hagua"  ,      "hsaneamiento", "hsalud")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribution",
    x = "Elemento",
    y = "Estimation",
    color = ""
  )  +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y")+
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

p24 <- ggplot(tab_plot %>% filter(nbi %in% c("hpartemp"  ,   "hempe"   ,     "hjub")), aes(x = dam, y = estimacion, color = tipo)) +
  geom_point(size = 3) +  # Añadir puntos para las estimaciones
  labs(
    title = "Contribution",
    x = "Elemento",
    y = "Estimation",
    color = ""
  ) +  theme_bw(base_size = 15) + facet_grid(nbi~.,scales = "free_y") +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

ggsave(plot = p21,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion1_dam.jpeg",
       width = 20,height = 14
)

ggsave(plot = p22,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion2_dam.jpeg",
       width = 20,height = 14
)

ggsave(plot = p23,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion3_dam.jpeg",
       width = 20,height = 14
)

ggsave(plot = p24,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion4_dam.jpeg",
       width = 20,height = 14
)


var_names <- c(
  "hnolee" = "Illiteracy",
  "hlogroeduc" = "Educational attainment",
  "heducninios" = "Non-attendance or lag",
  "hhacina" = "Overcrowding",
  "henergia" = "Energy",
  "htic" = "Internet access",
  "hagua" = "Water",
  "hsaneamiento" = "Sanitation",
  "hsalud" = "Health insurance",
  "hpartemp" = "Labor market participation",
  "hempe" = "Quality of employment",
  "hjub" = "Pensions"
)

tipo_names <- c("sample" = "Sample", 
                "censo" = "Model")

unique(tab_plot$tipo)
tab_plot <- tab_plot %>%
  mutate(nbi = recode(nbi, !!!var_names),
         tipo = recode(tipo, !!!tipo_names)) 




p3 <- ggplot(tab_plot %>% mutate(dam2 = paste0(dam,"_", tipo) ), 
       aes(x = dam, y = estimacion, fill = nbi)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw(15) +
  facet_grid(tipo~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "", x = "Major administrative division", y = "")

ggsave(plot = p3,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion_dam.jpeg",
       width = 18,height = 12
)


dat_set <- read_sf("Modelo_bayes_HxA_Hogar/COL/Shape/COL_dam2.shp") %>% 
  data.frame() %>% distinct(dam,dam_name) %>% 
  mutate(dam_name = ifelse(dam == 11, "BOGOTÁ, D.C.", dam_name))

temp <- tab_plot %>% select(dam : estimacion) %>%
  pivot_wider(names_from  = "tipo", 
              values_from = "estimacion", 
              values_fill = list(estimacion = 0)) %>% 
  data.frame() %>% 
  inner_join(dat_set)

tab_2 <- temp %>% filter(Sample == 0) %>% select(-Sample, -dam) %>% 
  pivot_wider(names_from  = "dam_name", 
              values_from = "Model", 
              values_fill = list(Model = 0))

tab_1 <- temp %>% filter(Sample > 0) %>% 
  transmute(
    dam_name, nbi,
    diff_abs  = abs(Sample - Model) ) %>% 
  pivot_wider(names_from  = "nbi", 
              values_from = "diff_abs",
              values_fill = list(Model = 0))

tab_11 <- temp %>% filter(Sample > 0) %>% 
  transmute(
    dam_name, nbi,
    diff_abs  = abs(Sample - Model)/Sample ) %>% 
  pivot_wider(names_from  = "nbi", 
              values_from = "diff_abs",
              values_fill = list(Model = 0))

openxlsx::write.xlsx(
  list(tab_dif_abs = tab_1, 
       tab_dif_relativa = tab_11,
       tab_pred_dam = tab_2), 
  file = "Modelo_bayes_HxA_Hogar/COL/Doc/01_tablas/00_tablas_dam.xlsx"
)
openxlsx::openXL(file = "Modelo_bayes_HxA_Hogar/COL/Doc/01_tablas/00_tablas_dam.xlsx")



################################################################################
## Mpios 
################################################################################
id_dam <- "81"
dat_set <- read_sf("Modelo_bayes_HxA_Hogar/COL/Shape/COL_dam2.shp") %>%
  data.frame() %>% filter(dam == id_dam) %>% 
  select(dam,dam2, dam2_name) %>% 
  mutate(orden = 1:n(), 
         mpios = "Minor adminsitrative divisions")

contribuciones_censo <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_mpios.xlsx",
                      sheet = "contribuciones")

contribuciones_censo_dam <-
  openxlsx::read.xlsx("Modelo_bayes_HxA_Hogar/COL/Output/contribuciones_dam.xlsx",
                      sheet = "contribuciones") %>% 
  filter(dam == id_dam)

contribuciones_censo2 <- bind_rows(contribuciones_censo_dam, contribuciones_censo) %>%
  mutate(dam2 = ifelse(is.na(dam2), dam, dam2)) %>%
  mutate(dam = str_sub(dam2, 1, 2)) %>%
  filter(dam == id_dam)


tab_censo <- contribuciones_censo2 %>%  
  select(dam2, matches("media")) %>%  
  pivot_longer(
    cols = matches("media"),
    names_to = "nbi", values_to = "estimacion") %>%
  separate(
    col = "nbi",
    into = c("nbi", "tipo"),
    sep = "_"
  ) %>%
  mutate(tipo = "Model")

tab_plot <- tab_censo

var_names <- c(
  "hnolee" = "Illiteracy",
  "hlogroeduc" = "Educational attainment",
  "heducninios" = "Non-attendance or lag",
  "hhacina" = "Overcrowding",
  "henergia" = "Energy",
  "htic" = "Internet access",
  "hagua" = "Water",
  "hsaneamiento" = "Sanitation",
  "hsalud" = "Health insurance",
  "hpartemp" = "Labor market participation",
  "hempe" = "Quality of employment",
  "hjub" = "Pensions"
)

unique(tab_plot$tipo)
unique(tab_plot$dam2)
tab_plot <- tab_plot %>%
  mutate(nbi = recode(nbi, !!!var_names)) 

tab_plot <-  tab_plot %>% inner_join(dat_set %>%
                                       bind_rows(
                                         data.frame(
                                           dam = id_dam,
                                           dam2 = id_dam,
                                           dam2_name = "ARAUCA",
                                           orden = max(dat_set$orden) +  5, 
                                           mpios = "MAD"
                                         )
                                       ))

levels_originales <- c("ARAUCA"   ,    "ARAUQUITA"  ,  "CRAVO NORTE" , "FORTUL" ,     
                      "PUERTO RONDÓN", "SARAVENA" ,    "TAME"   )

levels_originales2 <- c("MAD", "Minor adminsitrative divisions")

tab_plot <-  tab_plot %>% 
  mutate(dam2_name = forcats::fct_reorder(dam2_name, orden),
         dam2_name = factor(dam2_name, levels = c(levels_originales)),
         mpios = forcats::fct_reorder(mpios, orden),
         mpios = factor(mpios, levels = c(levels_originales2))
         )
unique(tab_plot$dam2_name)                                

p3 <- ggplot(tab_plot, 
             aes(x = dam2_name, y = estimacion, fill = nbi)) +
  geom_bar(stat = "identity", position = "fill",  width = 0.7) +
  # scale_x_discrete(drop = FALSE)  +
  theme_bw(15) +
  facet_grid(tipo ~ mpios, scales = "free_x", space = "free_x") +  # Ajusta espacio
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "", x = "", y = "")
p3
ggsave(plot = p3,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion_dam_ARAUCA.jpeg",
       width = 18,height = 12
)


tab_plot <- tab_censo



var_names <- c(
  "hnolee" = "Education",
  "hlogroeduc" = "Education",
  "heducninios" = "Education",
  "hhacina" = "Housing",
  "henergia" = "Health",
  "htic" = "Housing",
  "hagua" = "Housing",
  "hsaneamiento" = "Health",
  "hsalud" = "Health",
  "hpartemp" = "Employment and social securuty",
  "hempe" = "Employment and social securuty",
  "hjub" = "Employment and social securuty"
)


tab_plot <- tab_plot %>%
  mutate(nbi = recode(nbi, !!!var_names)) 

tab_plot2 <- tab_plot %>% group_by(dam2, nbi,  tipo ) %>% 
  summarise(estimacion = sum(estimacion))


tab_plot2 <-  tab_plot2 %>%
  inner_join(dat_set %>%
               bind_rows(
                 data.frame(
                   dam = id_dam,
                   dam2 = id_dam,
                   dam2_name = "ARAUCA",
                   orden = max(dat_set$orden) +   5,
                   mpios = "MAD"
                 )
               ))

tab_plot2 <-  tab_plot2 %>% 
  mutate(dam2_name = forcats::fct_reorder(dam2_name, orden),
         dam2_name = factor(dam2_name, levels = c(levels_originales)),
         mpios = forcats::fct_reorder(mpios, orden),
         mpios = factor(mpios, levels = c(levels_originales2))
  )


p3 <- ggplot(tab_plot2, 
             aes(x = dam2_name,
                 y = estimacion, fill = nbi)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw(15) +
  facet_grid(tipo ~ mpios, scales = "free_x", space = "free_x") +  # Ajusta espacio
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "", x = "", y = "")

ggsave(plot = p3,
       filename = "Modelo_bayes_HxA_Hogar/COL/Output/plot_contribucion/contribucion_dam_ARAUCA_dim.jpeg",
       width = 18,height = 12
)
