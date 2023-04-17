library(ggplot2)

leerdatos<-"/Users/marianacornejo/Desktop/Gobierno/"
base<-read.csv(paste0(leerdatos,"baseinegi.csv"))

library(tidyverse)
library(tidylog)
library(readstata13)
library(wesanderson)


#CANTIDAD DE JÓVENES QUE DISPONEN DE CELULAR
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  group_by(EDAD, DOMINIO) %>%
  ungroup() %>%
  ggplot() +
  aes(x=as.factor(P8_1), fill=as.factor(P8_1)) +
  geom_bar() +
  scale_fill_manual(values = c("dodgerblue4", "dodgerblue3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Disponibilidad de celular", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Dispone usted de celular? Zonas Urbanas:")

#CANTIDAD DE JÓVENES QUE DISPONEN DE CELULAR EN ZONAS URBANAS Y RURALES
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(DOMINIO != "R") %>%
  ggplot() +
  aes(x=as.factor(P8_1), fill=as.factor(P8_1)) +
  geom_bar() +
  scale_fill_manual(values = c("dodgerblue4", "dodgerblue3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Disponibilidad de celular", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Dispone usted de celular?")

#RAZONES POR LAS QUE NO DISPONEN DE UN CELULAR
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(DOMINIO != "R") %>%
  filter(!is.na(P8_2)) %>%
  mutate(P8_2 = recode(P8_2, "1" = "A", "2" = "B", "3" = "C", "4" = "C", "5" = "D", "6" = "E", "7" = "D", "8" = "B")) %>%
  ggplot() +
  aes(x=as.factor(P8_2), fill=as.factor(P8_2)) +
  geom_bar() +
 # scale_fill_manual(values = c("dodgerblue4", "dodgerblue3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Razón", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Por qué no dispone de un celular (común o Smartphone)?")


#El CELULAR ES INTELIGENTE
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_4_2)) %>%
  filter(DOMINIO != "R") %>%
  group_by(EDAD, DOMINIO) %>%
  ungroup() %>%
  ggplot() +
  aes(x=as.factor(P8_4_2), fill=as.factor(P8_4_2)) +
  geom_bar() +
  scale_fill_manual(values = c("cadetblue4", "cadetblue3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿El celular que usa es inteligente (Smartphone)?")

#PROPORCION DE JOVENES QUE SE CONECTAN A INTERNET POR MEDIO DE SU TELEFONO
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_10)) %>%
  ggplot() +
  aes(x=as.factor(P8_10), fill=as.factor(P8_10)) +
  geom_bar() +
  scale_fill_manual(values = c("yellow4", "yellow3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Se conecta a Internet por medio de su Smartphone?")

#CONEXION POR WIFI
basefinal <- base %>%
  filter(!is.na(P8_11_1)) %>%
  filter(!is.na(P8_11_2)) %>%
  mutate(P8_11_1 = recode(P8_11_1, "1" = "A", "2" = "B")) %>%
  mutate(P8_11_2 = recode(P8_11_2, "1" = "C", "2" = "D")) %>%
  mutate(conexion= paste(P8_11_1, P8_11_2, sep = " "))

basefinal %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_11_1)) %>%
  filter(!is.na(P8_11_2)) %>%
  filter(DOMINIO != "R") %>%
  ggplot() +
  aes(x=as.factor(conexion), fill=as.factor(conexion)) +
  geom_bar() +
  #scale_fill_manual(values = c("slateblue4", "slateblue3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "Conexión a internet")

#CONEXIÓN POR DATOS
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_11_2)) %>%
  ggplot() +
  aes(x=as.factor(P8_11_2), fill=as.factor(P8_11_2)) +
  geom_bar() +
  scale_fill_manual(values = c("springgreen4", "springgreen3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Se conecta a Internet mediante red celular (conexión de datos)?")

#JOVENES QUE USAN APPS DE JUEGO
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_12_5)) %>%
  filter(DOMINIO != "R") %>%
  ggplot() +
  aes(x=as.factor(P8_12_5), fill=as.factor(P8_12_5)) +
  geom_bar() +
  scale_fill_manual(values = c("mediumpurple4", "mediumpurple3") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Ha usado aplicaciones para jugar?")

#JOVENES QUE USAN REDES SOCIALES
base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  filter(!is.na(P8_12_6)) %>%
  filter(DOMINIO != "R") %>%
  ggplot() +
  aes(x=as.factor(P8_12_6), fill=as.factor(P8_12_6)) +
  geom_bar() +
  scale_fill_manual(values = c("blue4", "blue1") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "¿Ha usado aplicaciones para acceder a redes sociales?")

base %>%
  filter(EDAD>=12 & EDAD<=19) %>%
  ggplot() +
  aes(x=as.factor(EDAD), fill=as.factor(EDAD)) +
  geom_bar() +
  #scale_fill_manual(values = c("blue4", "blue1") ) +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "Cantidad de jóvenes", color = "", fill = "",
       title = "Cantidad de jóvenes")

basejovenes <- base %>%
  filter(EDAD>=12 & EDAD<=19)



