#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("deSolve")
library(ggplot2)
library(ggpubr)
library(gridExtra)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Estrategia de Mitigación para impulsar la reducción de GEI"),
    p("Una herramienta interactiva de pensamiento sistémico para estudiar las energías fósiles y renovables en México, desarrollada por Mariana Cornejo y Sabrina Ciscomani",
      tags$a(href="https://drive.google.com/drive/folders/11FatDJmKIwo_6uY6zBhCMmmc9MZ9820A?usp=sharing", "Aquí puedes encontrar el link")),
    hr(),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Plan Nacional de Energías Renovables"),
            sliderInput("Impuesto_a_la_inversion",
                        "Impuesto a las ganancias por la inversion",
                        min = 0,
                        max = 0.2,
                        value = 0,
                        animate = TRUE),
            sliderInput("x.precios.hidrocarburos",
                        "Impuesto a los precios de hidrocarburos",
                        min = 0,
                        max = 1,
                        value = 0.01,
                        animate = TRUE),
            
            h3("NO Fósil Privada"),
            sliderInput("Costo_Desarrollo_Capacidad_NoFosil_privada",
                        "Costo de Desarrollo de Capacidad",
                        min = 0,
                        max = 10000,
                        value = 1228,
                        animate = TRUE),
            sliderInput("Inversion_base_NoFosil_privada",
                        "Inversión Base",
                        min = 0,
                        max = 15000,
                        value = 500*20,
                        animate = TRUE),
            
            h3("NO Fósil Pública"),
            sliderInput("Costo_Desarrollo_Capacidad_NoFosil_publica",
                        "Costo de Desarrollo de Capacidad",
                        min = 0,
                        max = 10000,
                        value = 2857,
                        animate = TRUE),
            sliderInput("Inversion_base_NoFosil_publica",
                        "Inversión Base",
                        min = 0,
                        max = 15000,
                        value = 100*20,
                        animate = TRUE),
            
            h3("Fósil Privada"),
            sliderInput("Costo_Desarrollo_Capacidad_Fosil_privada",
                        "Costo de Desarrollo de Capacidad",
                        min = 0,
                        max = 10000,
                        value = 1739,
                        animate = TRUE),
            sliderInput("Inversion_base_Fosil_privada",
                        "Inversión Base",
                        min = 0,
                        max = 15000,
                        value = 500*20,
                        animate = TRUE),
            
            h3("Fósil Pública"),
            sliderInput("Costo_Desarrollo_Capacidad_Fosil_publica",
                        "Costo de Desarrollo de Capacidad",
                        min = 0,
                        max = 10000,
                        value = 5822,
                        animate = TRUE),
            sliderInput("Inversion_base_Fosil_publica",
                        "Inversión Base",
                        min = 0,
                        max = 15000,
                        value = 100*20,
                        animate = TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tabs", 
                              tabPanel("Capacidad de Energía",
                                       plotOutput('EnerMX')),
                              tabPanel("Emisiones",
                                       plotOutput('EnerMX2')),
                              tabPanel("Inversión",
                                       plotOutput('EnerMX5')),
                              tabPanel("Total de Inversión",
                                       plotOutput('EnerMX3')),
                              tabPanel("Costo para el Gobierno",
                                       plotOutput('EnerMX4')),
                              tabPanel("Diagrama de Flujo",
                                       img(src = "2VENSIM123.png", height = 400, width = 800)))
                  
        )
    )
))
