#Abrimos todas las librerias necesarias
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar tidytext                                                       
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}

# Instalar - Cargar tidytext                                                       
if(require(tidylog) == FALSE){                                                
  install.packages('tidylog')                                                 
  library(tidylog)                                                            
}else{                                                                          
  library(tidylog)                                                            
}

# Instalar - Cargar stopwords                                                       
if(require(stopwords) == FALSE){                                                
  install.packages('stopwords')                                                 
  library(stopwords)                                                            
}else{                                                                          
  library(stopwords)                                                            
}

# Instalar - Cargar SnowballC                                                       
if(require(SnowballC) == FALSE){                                                
  install.packages('SnowballC')                                                 
  library(SnowballC)                                                            
}else{                                                                          
  library(SnowballC)                                                            
}

# Instalar - Cargar tm                                                       
if(require(tm) == FALSE){                                                
  install.packages('tm')                                                 
  library(tm)                                                            
}else{                                                                          
  library(tm)                                                            
}

# Instalar - Cargar topicmodels                                                       
if(require(topicmodels) == FALSE){                                                
  install.packages('topicmodels')                                                 
  library(topicmodels)                                                            
}else{                                                                          
  library(topicmodels)                                                            
}

## Instalacion de librerias
# Instalar - Cargar wordcloud                                                     
if(require(wordcloud) == FALSE){                                                
  install.packages('wordcloud')                                                 
  library(wordcloud)                                                            
}else{                                                                          
  library(wordcloud)                                                            
}

library(lubridate)
library(zoo)
library(scales)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(modelsummary)
library(fixest)
library (readr)

#Cargamos las bases de datos del ENIGH
dir.bases<-"/Users/marianacornejo/Desktop/Experimentos/"
enigh_2014<-read.csv(paste0(dir.bases,"ENIGH2014.csv"))
enigh_2016<-read.csv(paste0(dir.bases,"ENIGH2016.csv"))
enigh_2018<-read.csv(paste0(dir.bases,"ENIGH2018.csv"))
enigh_2020<-read.csv(paste0(dir.bases,"ENIGH2020.csv"))

#Unimos todas las bases en una sola
base_final <- rbind.data.frame(enigh_2014,
                               enigh_2016,
                               enigh_2018,
                               enigh_2020)

#Guardamos la base
save(base_final, file = "base_final.csv")

#Dividir un grupo de tratamiento y uno de control
base_final_tratamiento <- base_final %>% 
  mutate(grupo_bene = ifelse(base_final$bene_gob > 0,1,0))

base_final_tratamiento <- base_final_tratamiento %>%
  mutate(despues.transferencias = periodo > 2014,
         fuera.programa = periodo > 2019) %>%
  mutate(azucar_log = log(azucar+1),
         aceites_log = log(aceites+1),
         salud_log = log(salud + 1)) %>%
  mutate(mean_salud_log = mean(salud_log)) %>%
  mutate(consumosaludable = cereales + carnes + pescado + leche + huevo + tuberculo + verduras + frutas,
         consumonosaludable = aceites + azucar + ali_fuera,
         gastosalud = salud + atenc_ambu + hospital + medicinas) %>%
  mutate(consumosaludable_log = log(consumosaludable+1),
         consumonosaludable_log = log(consumonosaludable+1),
         gastosalud_log = log(gastosalud + 1)) %>%
  mutate(grupo_bene = ifelse(base_final$bene_gob > 0,1,0),
         grupo_trans = ifelse(base_final$trans_inst > 0,1,0),
         grupo_donativos = ifelse(base_final$donativos > 0,1,0)) %>%
  mutate(Tratamiento = grupo_bene == 1 & despues.transferencias == TRUE,
         # State = estado,
         Quarter = periodo,
         Despues_Tratamiento = despues.transferencias == TRUE,
         California = grupo_bene == 1) %>%
  mutate(Tratamiento1 = grupo_bene == 1 & fuera.programa == TRUE,
         # State = estado,
         Quarter1 = periodo,
         Despues_Tratamiento1 = fuera.programa == TRUE,
         California1 = grupo_bene == 1) 

#### ACEITE #####
#Visualización del efecto para gasto en aceite del cambio de suplementos a transferencias
  base_final_tratamiento %>%
    filter(aceites!=0) %>%
    mutate(aceite_log = log(aceites+1)) %>%
    group_by(periodo,grupo_bene) %>%
    mutate(mean_aceite = mean(aceite_log),
           sd_aceite = sd(aceite_log)) %>%
    distinct(periodo,grupo_bene,.keep_all=T)%>%
    ggplot(aes(x = periodo, y = mean_aceite, color = factor(grupo_bene))) +
    geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
    geom_line() + 
    xlab('Tiempo') + 
    ylab('Gasto en Aceite') +
    ggtitle('Gasto en Aceite a través de los años') + 
    # coord_cartesian(ylim = c(4,8)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
    geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#### AZUCAR #####
#Visualización del efecto para gasto en AZUCAR del cambio de suplementos a transferencias
base_final_tratamiento %>%
  filter(azucar!=0) %>%
  mutate(azucar_log = log(azucar+1)) %>%
  group_by(periodo,grupo_bene) %>%
  mutate(mean_azucar = mean(azucar_log),
         sd_azucar = sd(azucar_log)) %>%
  distinct(periodo,grupo_bene,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_azucar, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Azucar') +
  ggtitle('Gasto en Azucar a través de los años') + 
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#### CONSUMO SALUDABLE #####
#NO HAY PARALLEL TREND ASSUMPTION
#Visualización del efecto para gasto en CONSUMO SALUDABLE del cambio de suplementos a transferencias
base_final_tratamiento %>%
  filter(consumosaludable!=0) %>%
  mutate(consumosaludable_log = log(consumosaludable+1)) %>%
  group_by(periodo,grupo_bene) %>%
  mutate(mean_consumosaludable = mean(consumosaludable_log),
         sd_consumosaludable = sd(consumosaludable_log)) %>%
  distinct(periodo,grupo_bene,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo Saludable') +
  ggtitle('Gasto en Consumo Saludable a través de los años') + 
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)
  
##### CONSUMO NO SALUDABLE #####
#Visualización del efecto para gasto en CONSUMO NO SALUDABLE del cambio de suplementos a transferencias
base_final_tratamiento %>%
  filter(consumonosaludable!=0) %>%
  mutate(consumonosaludable_log = log(consumonosaludable+1)) %>%
  group_by(periodo,grupo_bene) %>%
  mutate(mean_consumonosaludable = mean(consumonosaludable_log),
         sd_consumonosaludable = sd(consumonosaludable_log)) %>%
  distinct(periodo,grupo_bene,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumonosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo NO Saludable') +
  ggtitle('Gasto en Consumo NO Saludable a través de los años') + 
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### SALUD #####
#Visualización del efecto para gasto en SALUD del cambio de suplementos a transferencias
base_final_tratamiento %>%
  filter(salud!=0) %>%
  mutate(salud_log = log(salud+1)) %>%
  group_by(periodo,grupo_bene) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_bene,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#################################################################################
########################       POR ESTADO      ##################################

##### ACEITE #####
base_final_tratamiento %>%
  filter(aceites!=0) %>%
  mutate(aceites_log = log(aceites+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_aceites = mean(aceites_log),
         sd_aceites = sd(aceites_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_aceites, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Aceites') +
  ggtitle('Gasto en Aceites a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### AZUCAR #####
base_final_tratamiento %>%
  filter(azucar!=0) %>%
  mutate(azucar_log = log(azucar+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_azucar = mean(azucar_log),
         sd_azucar = sd(azucar_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_azucar, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Azucar') +
  ggtitle('Gasto en Azucar a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#### CONSUMO SALUDABLE #####
base_final_tratamiento %>%
  filter(consumosaludable!=0) %>%
  mutate(consumosaludable_log = log(consumosaludable+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_consumosaludable = mean(consumosaludable_log),
         sd_consumosaludable = sd(consumosaludable_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo Saludable') +
  ggtitle('Gasto en Consumo Saludable a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### CONSUMO NO SALUDABLE #####
base_final_tratamiento %>%
  filter(consumonosaludable!=0) %>%
  mutate(consumonosaludable_log = log(consumonosaludable+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_consumonosaludable = mean(consumonosaludable_log),
         sd_consumonosaludable = sd(consumonosaludable_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumonosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo NO Saludable') +
  ggtitle('Gasto en Consumo NO Saludable a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### SALUD #####
base_final_tratamiento %>%
  filter(salud!=0) %>%
  mutate(salud_log = log(salud+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#################################################################################
########################      POR INGRESO    ##################################

##### ACEITE #####
base_final_tratamiento %>%
  filter(aceites!=0) %>%
  mutate(aceite_log = log(aceites+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_bene,cut(ing_log, breaks = 10)) %>%
  mutate(mean_aceites = mean(aceite_log),
         sd_aceites = sd(aceite_log)) %>%
  distinct(periodo,grupo_bene,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_aceites, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Aceite') +
  ggtitle('Gasto en Aceite a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### AZUCAR #####
base_final_tratamiento %>%
  filter(azucar!=0) %>%
  mutate(azucar_log = log(azucar+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_bene,cut(ing_log, breaks = 10)) %>%
  mutate(mean_azucar = mean(azucar_log),
         sd_azucar = sd(azucar_log)) %>%
  distinct(periodo,grupo_bene,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_azucar, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Azucar') +
  ggtitle('Gasto en Azucar a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#### CONSUMO SALUDABLE #####
base_final_tratamiento %>%
  filter(consumosaludable!=0) %>%
  mutate(consumosaludable_log = log(consumosaludable+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_bene,cut(ing_log, breaks = 10)) %>%
  mutate(mean_consumosaludable = mean(consumosaludable_log),
         sd_consumosaludable = sd(consumosaludable_log)) %>%
  distinct(periodo,grupo_bene,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo Saludable') +
  ggtitle('Gasto en Consumo Saludable a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### CONSUMO NO SALUDABLE #####
base_final_tratamiento %>%
  filter(consumonosaludable!=0) %>%
  mutate(consumonosaludable_log = log(consumonosaludable+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_bene,cut(ing_log, breaks = 10)) %>%
  mutate(mean_consumonosaludable = mean(consumonosaludable_log),
         sd_consumonosaludable = sd(consumonosaludable_log)) %>%
  distinct(periodo,grupo_bene,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_consumonosaludable, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Consumo NO Saludable') +
  ggtitle('Gasto en Consumo NO Saludable a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### SALUD GRUPO-BENE #####
base_final_tratamiento %>%
  filter(salud!=0) %>%
  mutate(salud_log = log(salud+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_bene,cut(ing_log, breaks = 10)) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_bene,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### SALUD GRUPO TRANSFERENCIAS  #####
base_final_tratamiento %>%
  filter(salud!=0) %>%
  mutate(salud_log = log(salud+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_trans,cut(ing_log, breaks = 10)) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_trans,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_trans))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

##### SALUD DONATIVOS #####
base_final_tratamiento %>%
  filter(salud!=0) %>%
  mutate(salud_log = log(salud+1),
         ing_log = log(ing_cor + 1)) %>%
  group_by(periodo,grupo_donativos,cut(ing_log, breaks = 10)) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_donativos,cut(ing_log, breaks = 10),.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_donativos))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~cut(ing_log, breaks = 10), nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#################################################################################
######################## DIFFERENCE IN DIFFERENCE ###############################
#################################################################################

###################################################
##### CAMBIO DE SUPLEMENTOS A TRANSFERENCIAS #####

#EFECTO EN SALUD
# Ahora podemos ver las diferencias antes y despues de cada grupo
promedios <- base_final_tratamiento %>%
  group_by(grupo_bene,despues.transferencias) %>%
  summarize(salud_log=mean(salud_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promedios,grupo_bene==0,despues.transferencias==TRUE)$salud_log - filter(promedios,grupo_bene==0,despues.transferencias==FALSE)$salud_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promedios,grupo_bene==1,despues.transferencias==TRUE)$salud_log - filter(promedios,grupo_bene==1,despues.transferencias==FALSE)$salud_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

clfe <- feols(salud_log ~ Tratamiento | grupo_bene + periodo + estado,
              data = base_final_tratamiento)

# Alternativa I:
clfe_2 <-lm(salud_log ~ Tratamiento + factor(grupo_bene) + factor(periodo) + factor(estado) ,
            data = base_final_tratamiento)

# Alternativa II:
clfe_3 <-lm(salud_log ~ despues.transferencias*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
            data = base_final_tratamiento)

msummary(list(clfe,clfe_2,clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))

#EFECTO EN CONSUMO SALUDABLE
# Ahora podemos ver las diferencias antes y despues de cada grupo
promedios_consumosaludable <- base_final_tratamiento %>%
  group_by(grupo_bene,despues.transferencias) %>%
  summarize(consumosaludable_log=mean(consumosaludable_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promedios_consumosaludable,grupo_bene==0,despues.transferencias==TRUE)$consumosaludable_log - filter(promedios_consumosaludable,grupo_bene==0,despues.transferencias==FALSE)$consumosaludable_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promedios_consumosaludable,grupo_bene==1,despues.transferencias==TRUE)$consumosaludable_log - filter(promedios_consumosaludable,grupo_bene==1,despues.transferencias==FALSE)$consumosaludable_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

cs_clfe <- feols(consumosaludable_log ~ Tratamiento | grupo_bene + periodo + estado,
              data = base_final_tratamiento)

# Alternativa I:
cs_clfe_2 <-lm(consumosaludable_log ~ Tratamiento + factor(grupo_bene) + factor(periodo) + factor(estado) ,
            data = base_final_tratamiento)

# Alternativa II:
cs_clfe_3 <-lm(consumosaludable_log ~ despues.transferencias*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
            data = base_final_tratamiento)

msummary(list(cs_clfe,cs_clfe_2,cs_clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))

#EFECTO EN CONSUMO NO SALUDABLE
# Ahora podemos ver las diferencias antes y despues de cada grupo
promedios_consumonosaludable <- base_final_tratamiento %>%
  group_by(grupo_bene,despues.transferencias) %>%
  summarize(consumonosaludable_log=mean(consumonosaludable_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promedios_consumonosaludable,grupo_bene==0,despues.transferencias==TRUE)$consumonosaludable_log - filter(promedios_consumonosaludable,grupo_bene==0,despues.transferencias==FALSE)$consumonosaludable_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promedios_consumonosaludable,grupo_bene==1,despues.transferencias==TRUE)$consumonosaludable_log - filter(promedios_consumonosaludable,grupo_bene==1,despues.transferencias==FALSE)$consumonosaludable_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

cns_clfe <- feols(consumonosaludable_log ~ Tratamiento | grupo_bene + periodo + estado,
              data = base_final_tratamiento)

# Alternativa I:
cns_clfe_2 <-lm(consumonosaludable_log ~ Tratamiento + factor(grupo_bene) + factor(periodo) + factor(estado) ,
            data = base_final_tratamiento)

# Alternativa II:
cns_clfe_3 <-lm(consumonosaludable_log ~ despues.transferencias*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
            data = base_final_tratamiento)

msummary(list(cns_clfe,cns_clfe_2,cns_clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))

###################################################
##### CANCELACIÓN DEL PROGRAMA #####

#EFECTO EN SALUD
# Ahora podemos ver las diferencias antes y despues de cada grupo
promediosfuera <- base_final_tratamiento %>%
  group_by(grupo_bene,fuera.programa) %>%
  summarize(salud_log=mean(salud_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promediosfuera,grupo_bene==0,fuera.programa==TRUE)$salud_log - filter(promediosfuera,grupo_bene==0,fuera.programa==FALSE)$salud_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promediosfuera,grupo_bene==1,fuera.programa==TRUE)$salud_log - filter(promediosfuera,grupo_bene==1,fuera.programa==FALSE)$salud_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

fuera_cclfe <- feols(salud_log ~ Tratamiento1 | grupo_bene + periodo + estado,
              data = base_final_tratamiento)

# Alternativa I:
fuera_clfe_2 <-lm(salud_log ~ Tratamiento1 + factor(grupo_bene) + factor(periodo) + factor(estado) ,
            data = base_final_tratamiento)

# Alternativa II:
fuera_clfe_3 <-lm(salud_log ~ fuera.programa*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
            data = base_final_tratamiento)

msummary(list(fuera_cclfe,fuera_clfe_2,fuera_clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))



#EFECTO EN CONSUMO SALUDABLE
# Ahora podemos ver las diferencias antes y despues de cada grupo
promediosfuera_cs <- base_final_tratamiento %>%
  group_by(grupo_bene,fuera.programa) %>%
  summarize(consumosaludable_log=mean(consumosaludable_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promediosfuera_cs,grupo_bene==0,fuera.programa==TRUE)$consumosaludable_log - filter(promediosfuera_cs,grupo_bene==0,fuera.programa==FALSE)$consumosaludable_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promediosfuera_cs,grupo_bene==1,fuera.programa==TRUE)$consumosaludable_log - filter(promediosfuera_cs,grupo_bene==1,fuera.programa==FALSE)$consumosaludable_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

cs_fuera_cclfe <- feols(consumosaludable_log ~ Tratamiento1 | grupo_bene + periodo + estado,
                     data = base_final_tratamiento)

# Alternativa I:
cs_fuera_clfe_2 <-lm(consumosaludable_log ~ Tratamiento1 + factor(grupo_bene) + factor(periodo) + factor(estado) ,
                  data = base_final_tratamiento)

# Alternativa II:
cs_fuera_clfe_3 <-lm(consumosaludable_log ~ fuera.programa*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
                  data = base_final_tratamiento)

msummary(list(cs_fuera_cclfe,cs_fuera_clfe_2,cs_fuera_clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))


#EFECTO EN CONSUMO NO SALUDABLE
# Ahora podemos ver las diferencias antes y despues de cada grupo
promediosfuera_cns <- base_final_tratamiento %>%
  group_by(grupo_bene,fuera.programa) %>%
  summarize(consumonosaludable_log=mean(consumonosaludable_log))

#Before-after difference for untreated, has time effect only
antes_despues_control <- filter(promediosfuera_cns,grupo_bene==0,fuera.programa==TRUE)$consumonosaludable_log - filter(promediosfuera_cns,grupo_bene==0,fuera.programa==FALSE)$consumonosaludable_log
#Before-after for treated, has time and treatment effect
antes_despues_tratado <- filter(promediosfuera_cns,grupo_bene==1,fuera.programa==TRUE)$consumonosaludable_log - filter(promediosfuera_cns,grupo_bene==1,fuera.programa==FALSE)$consumonosaludable_log

#Difference-in-Difference! Take the Time + Treatment effect, and remove the Time effect
DID <- antes_despues_tratado - antes_despues_control
DID

# La funciÃ³n feols = Fixed Effects OLS... Todo lo que viene despuÃ©s
# del | son las variables por las cuales quiero hacer efecto fijo:
# feols tambiÃ©n hace cluster de los errores standard del primer efecto fijo
# lo cual es algo deseable...

cns_fuera_cclfe <- feols(consumonosaludable_log ~ Tratamiento1 | grupo_bene + periodo + estado,
                        data = base_final_tratamiento)

# Alternativa I:
cns_fuera_clfe_2 <-lm(consumonosaludable_log ~ Tratamiento1 + factor(grupo_bene) + factor(periodo) + factor(estado) ,
                     data = base_final_tratamiento)

# Alternativa II:
cns_fuera_clfe_3 <-lm(consumonosaludable_log ~ fuera.programa*grupo_bene + factor(grupo_bene) + factor(periodo) + factor(estado)  ,
                     data = base_final_tratamiento)

msummary(list(cns_fuera_cclfe,cns_fuera_clfe_2,cns_fuera_clfe_3), stars = c('*' = .1, '**' = .05, '***' = .01),
         vcov = list(~grupo_bene,"robust","robust"),
         coef_omit = "factor", notes = "Coeficientes del efecto fijo en el Model 2 estÃ¡n omitidos.",
         gof_map = c('nobs',"adj.r.squared",'FE: State','FE: Quarter','vcov.type'))


#Correr el código para distintos grupos de estados
#CDMX vs Oaxaca
cdmxoaxaca <- c('Oaxaca','CDMX')

base_final_tratamiento %>%
  filter(salud!=0,
         estado == cdmxoaxaca) %>%
  mutate(salud_log = log(salud+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)

#Nuevo León vs Chiapas
nlchiapas <- c('Nuevo Leon','Chiapas')

base_final_tratamiento %>%
  filter(salud!=0,
         estado == nlchiapas) %>%
  mutate(salud_log = log(salud+1)) %>%
  group_by(periodo,grupo_bene,estado) %>%
  mutate(mean_salud = mean(salud_log),
         sd_salud = sd(salud_log)) %>%
  distinct(periodo,grupo_bene,estado,.keep_all=T)%>%
  ggplot(aes(x = periodo, y = mean_salud, color = factor(grupo_bene))) +
  geom_point(color = 'slateblue', size = 2, alpha = 0.6) +
  geom_line() + 
  xlab('Tiempo') + 
  ylab('Gasto en Salud') +
  ggtitle('Gasto en Salud a través de los años') + 
  facet_wrap(~estado, nrow=4) +
  # coord_cartesian(ylim = c(4,8)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2015, linetype = "dashed", alpha = .5) +
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = .5)



