# Entrañas y el interior de nuestro modelo

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$EnerMX <- renderPlot({
        
        #PLOT 1
        library("deSolve")
        
        EnerMX <- function(t, state, parameters) {
            with(as.list(c(state,parameters)), {
                #impact exógenos de la demanda
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.demanda.electricidad <- 300+10*times # demanda [TWH], mu= [TWH/año] de incremento de la demanda
                
                Demanda.Meta.Datos <- approxfun( x = times, #times
                                                 y = x.demanda.electricidad, # [TWH], basado en análisis de Mariana y Sabrina
                                                 method = "linear",
                                                 rule = 2)
                
                #
                
                #impacto exógeno, precios hidrocarburos
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.precios.hidrocarburos <- 1.0+(input$x.precios.hidrocarburos)*times #impuesto al carbono
                
                Precio_Hidrocarburos_Datos <- approxfun( x = times, #times
                                                         y = x.precios.hidrocarburos , # [1], basado en análisis de Mariana y Sabrina
                                                         method = "linear",
                                                         rule = 2)
                
                
                
                #choque de baterías
                inicio_choque <- 30
                efecto_baterias <- 1.0
                x.capacidad.baterias <- ifelse(times>inicio_choque, efecto_baterias+1.0,1.0 )
                
                Efecto_baterias_Factor_de_planta_NoFosil <- approxfun( x = times, #times
                                                                       y =  x.capacidad.baterias  , # [1], basado en análisis de Mariana y Sabrina
                                                                       method = "linear",
                                                                       rule = 2)
                
                #Total de Inversión Sector Público
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                
                Impuesto_a_la_inversion <- approx(c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0),
                                                  c(0,0,0,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion),
                                                  xout = t)$y
                
                #Auxiliary variables
                Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
                Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                  c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
                
                #NoFosil_privada
                Efecto_baterias_Factor_de_planta_NoFosil<-Efecto_baterias_Factor_de_planta_NoFosil(t)
                Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
                
                #NoFosil_publica
                Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
                
                #Fosil_privada
                Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
                CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
                
                #Fosil_publica
                Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
                CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
                
                #Costo Promedio Sistema
                CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
                
                
                #Precios relativos
                #dentro de sectores
                CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
                CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
                
                #entre sectores
                CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
                CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
                
                
                #Efecto de precios relativos
                #dentro sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,1e6),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                #Entre sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                          c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                
                
                #Niveles de inversión
                #Nofosil_privada
                Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada*(1+(Impuesto_a_la_inversion))
                
                #Nofosil_publica
                Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
                
                #Fosil_privada
                Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
                
                #Fosil_publica
                Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
                
                
                
                #Flow Variables
                
                #Capacidad_NoFosil_privada
                Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
                Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
                
                #Capacidad_Fosil_privada
                Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
                Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
                
                #Capacidad_NoFosil_publica
                Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
                Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
                
                #Capacidad_Fosil_publica
                Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_publica
                Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
                
                
                #Variables de salida
                EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
                Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
                Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
                Total_Inversion_Sector_Publica <- Inversion_NoFosil_publica + Inversion_Fosil_publica
                Total_Inversion_Sector_Privada <- Inversion_NoFosil_privada + Inversion_Fosil_privada
                Costo_Gobierno_Politica_NoFosil_Privada <- Costo_Desarrollo_Capacidad_NoFosil_privada * Impuesto_a_la_inversion
                Costo_Gobierno_Politica_NoFosil_Publica <- Costo_Desarrollo_Capacidad_NoFosil_publica * Impuesto_a_la_inversion
                
                #State Variable
                dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                
                
                #Output model results
                list(
                    c(
                        #State Variables
                        dCapacidad_NoFosil_privada,
                        dCapacidad_Fosil_privada,
                        dCapacidad_NoFosil_publica,
                        dCapacidad_Fosil_publica
                    ),
                    #Flow variables
                    EmisionesCO2e = EmisionesCO2e,
                    CNG_sistema = CNG_sistema, # [Millones MXN/ TWH]
                    Deficit_Generacion =  Deficit_Generacion, # [TWH]
                    Efecto_baterias_Factor_de_planta_NoFosil= Efecto_baterias_Factor_de_planta_NoFosil
                    
                )
            })
        }
        
        parameters<-c(
            #NoFosil_privada
            Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector público
            Costo_Desarrollo_Capacidad_NoFosil_privada = input$Costo_Desarrollo_Capacidad_NoFosil_privada, #1228, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_privada = input$Inversion_base_NoFosil_privada, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
            Vida_util_Capacidad_NoFosil_privada = 23, #[años]
            #NoFosil_publica
            Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_NoFosil_publica = input$Costo_Desarrollo_Capacidad_NoFosil_publica, #2857, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_publica = input$Inversion_base_NoFosil_publica, #100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_NoFosil_publica = 23, #[años]
            #Fosil_privada
            Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_privada = input$Costo_Desarrollo_Capacidad_Fosil_privada, #1739,  # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_privada = input$Inversion_base_Fosil_privada, #500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
            Vida_util_Capacidad_Fosil_privada = 28 , #[años]
            #Fosil_publica
            Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_publica = input$Costo_Desarrollo_Capacidad_Fosil_publica, #5822 , # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_publica = input$Inversion_base_Fosil_publica, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_Fosil_publica = 28 #[años]
        )
        
        
        InitialConditions <- c(
            Capacidad_NoFosil_privada = 15*1/2 , # [GW] , asumimos que el sector privado tiene 1/2 de la capacidad instalada
            Capacidad_Fosil_privada = 15 *1/2, # [GW] , asumimos que el sector público tiene 1/2 de la capacidad instalada
            Capacidad_NoFosil_publica = 44*1/2 , # [GW]
            Capacidad_Fosil_publica = 44*1/2  #[GW]
        )
        
        
        #years
        times <- seq(0, #initial time
                     40, #end of simulation #[year]
                     0.1)#time step #[year]
        
        intg.method<-c("rk4")
        
        #Simulate model
        
        out <- ode(y = InitialConditions,
                   times = times,
                   func = EnerMX,
                   parms = parameters,
                   method =intg.method
        )
        
        
        #Convertir Out en data frame
        out<-data.frame(out)
        
        #Agregar línea de tendencia en color rojo
        #Geom_smooth es lo que hace que exista esa área gris y la línea roja
        No.fosil.privada <- ggplot(data = out, aes(x = time, y = Capacidad_NoFosil_privada)) + 
            #geom_point(color = 'springgreen4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'green') + 
            xlab('Tiempo') + 
            ylab('Capacidad') +
            ggtitle('Capacidad NO Fósil Privada') + 
            theme_minimal()
        
        Fosil.privada <- ggplot(data = out, aes(x = time, y = Capacidad_Fosil_privada)) + 
            #geom_point(color = 'plum2', size = 2, alpha = 0.6) +
            geom_smooth(color = 'hotpink2') + 
            xlab('Tiempo') + 
            ylab('Capacidad') +
            ggtitle('Capacidad Fósil Privada') + 
            theme_minimal()
        
        No.fosil.publica <- ggplot(data = out, aes(x = time, y = Capacidad_NoFosil_publica)) + 
            #geom_point(color = 'violetred4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'darkorange4') + 
            xlab('Tiempo') + 
            ylab('Capacidad') +
            ggtitle('Capacidad NO Fósil Pública') + 
            theme_minimal()
        
        Fosil.publica <- ggplot(data = out, aes(x = time, y = Capacidad_Fosil_publica)) + 
            #geom_point(color = 'lightslateblue', size = 2, alpha = 0.6) +
            geom_smooth(color = 'darkorchid4') + 
            xlab('Tiempo') + 
            ylab('Capacidad') +
            ggtitle('Capacidad Fósil Pública') + 
            theme_minimal()
        
        grid.arrange(No.fosil.privada, Fosil.privada, No.fosil.publica, Fosil.publica,
                     ncol = 2, nrow = 2)
        
    })
    
    #PLOT 2
    output$EnerMX2 <- renderPlot({
        library("deSolve")
        
        EnerMX2 <- function(t, state, parameters) {
            with(as.list(c(state,parameters)), {
                #impact exógenos de la demanda
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.demanda.electricidad <- 300+10*times # demanda [TWH], mu= [TWH/año] de incremento de la demanda
                
                Demanda.Meta.Datos <- approxfun( x = times, #times
                                                 y = x.demanda.electricidad, # [TWH], basado en análisis de Mariana y Sabrina
                                                 method = "linear",
                                                 rule = 2)
                
                #
                
                #impacto exógeno, precios hidrocarburos
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.precios.hidrocarburos <- 1.0+(input$x.precios.hidrocarburos)*times #impuesto al carbono
                
                Precio_Hidrocarburos_Datos <- approxfun( x = times, #times
                                                         y = x.precios.hidrocarburos , # [1], basado en análisis de Mariana y Sabrina
                                                         method = "linear",
                                                         rule = 2)
                
                
                
                #choque de baterías
                inicio_choque <- 30
                efecto_baterias <- 1.0
                x.capacidad.baterias <- ifelse(times>inicio_choque, efecto_baterias+1.0,1.0 )
                
                Efecto_baterias_Factor_de_planta_NoFosil <- approxfun( x = times, #times
                                                                       y =  x.capacidad.baterias  , # [1], basado en análisis de Mariana y Sabrina
                                                                       method = "linear",
                                                                       rule = 2)
                
                #Total de Inversión Sector Público
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                
                Impuesto_a_la_inversion <- approx(c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0),
                                                  c(0,0,0,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion),
                                                  xout = t)$y
                
                #Auxiliary variables
                Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
                Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                  c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
                
                #NoFosil_privada
                Efecto_baterias_Factor_de_planta_NoFosil<-Efecto_baterias_Factor_de_planta_NoFosil(t)
                Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
                
                #NoFosil_publica
                Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
                
                #Fosil_privada
                Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
                CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
                
                #Fosil_publica
                Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
                CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
                
                #Costo Promedio Sistema
                CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
                
                
                #Precios relativos
                #dentro de sectores
                CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
                CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
                
                #entre sectores
                CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
                CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
                
                
                #Efecto de precios relativos
                #dentro sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,1e6),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                #Entre sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                          c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                
                
                #Niveles de inversión
                #Nofosil_privada
                Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada*(1+(Impuesto_a_la_inversion))
                
                #Nofosil_publica
                Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
                
                #Fosil_privada
                Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
                
                #Fosil_publica
                Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
                
                
                
                #Flow Variables
                
                #Capacidad_NoFosil_privada
                Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
                Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
                
                #Capacidad_Fosil_privada
                Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
                Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
                
                #Capacidad_NoFosil_publica
                Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
                Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
                
                #Capacidad_Fosil_publica
                Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_publica
                Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
                
                
                #Variables de salida
                EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
                Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
                Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
                Total_Inversion_Sector_Publica <- Inversion_NoFosil_publica + Inversion_Fosil_publica
                Total_Inversion_Sector_Privada <- Inversion_NoFosil_privada + Inversion_Fosil_privada
                Costo_Gobierno_Politica_NoFosil_Privada <- Costo_Desarrollo_Capacidad_NoFosil_privada * Impuesto_a_la_inversion
                Costo_Gobierno_Politica_NoFosil_Publica <- Costo_Desarrollo_Capacidad_NoFosil_publica * Impuesto_a_la_inversion
                
                #State Variable
                dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                
                
                #Output model results
                list(
                    c(
                        #State Variables
                        dCapacidad_NoFosil_privada,
                        dCapacidad_Fosil_privada,
                        dCapacidad_NoFosil_publica,
                        dCapacidad_Fosil_publica
                    ),
                    #Flow variables
                    EmisionesCO2e = EmisionesCO2e,
                    CNG_sistema = CNG_sistema, # [Millones MXN/ TWH]
                    Deficit_Generacion =  Deficit_Generacion, # [TWH]
                    Efecto_baterias_Factor_de_planta_NoFosil= Efecto_baterias_Factor_de_planta_NoFosil
                    
                )
            })
        }
        
        parameters<-c(
            #NoFosil_privada
            Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector público
            Costo_Desarrollo_Capacidad_NoFosil_privada = input$Costo_Desarrollo_Capacidad_NoFosil_privada, #1228 # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_privada = input$Inversion_base_NoFosil_privada,#500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
            Vida_util_Capacidad_NoFosil_privada = 23, #[años]
            #NoFosil_publica
            Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_NoFosil_publica = input$Costo_Desarrollo_Capacidad_NoFosil_publica, #2857, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_publica = input$Inversion_base_NoFosil_publica, #100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_NoFosil_publica = 23, #[años]
            #Fosil_privada
            Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_privada = input$Costo_Desarrollo_Capacidad_Fosil_privada, #1739,  # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_privada = input$Inversion_base_Fosil_privada, #500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
            Vida_util_Capacidad_Fosil_privada = 28 , #[años]
            #Fosil_publica
            Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_publica = input$Costo_Desarrollo_Capacidad_Fosil_publica, #5822 , # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_publica = input$Inversion_base_Fosil_publica, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_Fosil_publica = 28 #[años]
        )
        
        
        InitialConditions <- c(
            Capacidad_NoFosil_privada = 15*1/2 , # [GW] , asumimos que el sector privado tiene 1/2 de la capacidad instalada
            Capacidad_Fosil_privada = 15 *1/2, # [GW] , asumimos que el sector público tiene 1/2 de la capacidad instalada
            Capacidad_NoFosil_publica = 44*1/2 , # [GW]
            Capacidad_Fosil_publica = 44*1/2  #[GW]
        )
        
        
        #years
        times <- seq(0, #initial time
                     40, #end of simulation #[year]
                     0.1)#time step #[year]
        
        intg.method<-c("rk4")
        
        #Simulate model
        
        out <- ode(y = InitialConditions,
                   times = times,
                   func = EnerMX2,
                   parms = parameters,
                   method =intg.method
        )
        #
        
        #Convertir Out en data frame
        out<-data.frame(out)
        
        #Agregar línea de tendencia en color rojo
        
        
        emisiones.co2 <- ggplot(data = out, aes(x = time, y = EmisionesCO2e)) + 
            #geom_point(color = 'greenyellow', size = 2, alpha = 0.6) +
            geom_smooth(color = 'green4') + 
            xlab('Tiempo') + 
            ylab('Emisiones') +
            ggtitle('Emisiones C02') + 
            theme_minimal()
        
        CNG.sistema <- ggplot(data = out, aes(x = time, y = CNG_sistema)) + 
            #geom_point(color = 'darkgoldenrod4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'darkgoldenrod2') + 
            xlab('Tiempo') + 
            ylab('Costo') +
            ggtitle('Costo Nivelado de Generación') + 
            theme_minimal()
        
        Deficit.generacion <- ggplot(data = out, aes(x = time, y = Deficit_Generacion)) + 
            #geom_point(color = 'yellow4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'yellow2') + 
            xlab('Tiempo') + 
            ylab('Deficit') +
            ggtitle('Deficit de Generación') + 
            theme_minimal()
        
        Efecto.factor.planta <- ggplot(data = out, aes(x = time, y = Efecto_baterias_Factor_de_planta_NoFosil)) + 
            #geom_point(color = 'magenta4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'magenta2') + 
            xlab('Tiempo') + 
            ylab('Efecto') +
            ggtitle('Efecto Baterías Factor de Planta No Fosil') + 
            theme_minimal()
        
        grid.arrange(emisiones.co2, CNG.sistema, Deficit.generacion, Efecto.factor.planta,
                     ncol = 2, nrow = 2)
        
        
    })
    
    #PLOT 3
    output$EnerMX3 <- renderPlot({
        library("deSolve")
        
        EnerMX3 <- function(t, state, parameters) {
            with(as.list(c(state,parameters)), {
                #impact exógenos de la demanda
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.demanda.electricidad <- 300+10*times # demanda [TWH], mu= [TWH/año] de incremento de la demanda
                
                Demanda.Meta.Datos <- approxfun( x = times, #times
                                                 y = x.demanda.electricidad, # [TWH], basado en análisis de Mariana y Sabrina
                                                 method = "linear",
                                                 rule = 2)
                
                #
                
                #impacto exógeno, precios hidrocarburos
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.precios.hidrocarburos <- 1.0+(input$x.precios.hidrocarburos)*times #impuesto al carbono
                
                Precio_Hidrocarburos_Datos <- approxfun( x = times, #times
                                                         y = x.precios.hidrocarburos , # [1], basado en análisis de Mariana y Sabrina
                                                         method = "linear",
                                                         rule = 2)
                
                
                
                #choque de baterías
                inicio_choque <- 30
                efecto_baterias <- 1.0
                x.capacidad.baterias <- ifelse(times>inicio_choque, efecto_baterias+1.0,1.0 )
                
                Efecto_baterias_Factor_de_planta_NoFosil <- approxfun( x = times, #times
                                                                       y =  x.capacidad.baterias  , # [1], basado en análisis de Mariana y Sabrina
                                                                       method = "linear",
                                                                       rule = 2)
                
                #Total de Inversión Sector Público
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                
                Impuesto_a_la_inversion <- approx(c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0),
                                                  c(0,0,0,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion),
                                                  xout = t)$y
                
                #Auxiliary variables
                Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
                Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                  c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
                
                #NoFosil_privada
                Efecto_baterias_Factor_de_planta_NoFosil<-Efecto_baterias_Factor_de_planta_NoFosil(t)
                Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
                
                #NoFosil_publica
                Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
                
                #Fosil_privada
                Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
                CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
                
                #Fosil_publica
                Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
                CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
                
                #Costo Promedio Sistema
                CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
                
                
                #Precios relativos
                #dentro de sectores
                CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
                CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
                
                #entre sectores
                CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
                CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
                
                
                #Efecto de precios relativos
                #dentro sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,1e6),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                #Entre sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                          c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                
                
                #Niveles de inversión
                #Nofosil_privada
                Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada*(1+(Impuesto_a_la_inversion))
                
                #Nofosil_publica
                Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
                
                #Fosil_privada
                Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
                
                #Fosil_publica
                Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
                
                
                
                #Flow Variables
                
                #Capacidad_NoFosil_privada
                Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
                Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
                
                #Capacidad_Fosil_privada
                Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
                Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
                
                #Capacidad_NoFosil_publica
                Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
                Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
                
                #Capacidad_Fosil_publica
                Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_publica
                Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
                
                
                #Variables de salida
                EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
                Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
                Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
                Total_Inversion_Sector_Publica <- Inversion_NoFosil_publica + Inversion_Fosil_publica
                Total_Inversion_Sector_Privada <- Inversion_NoFosil_privada + Inversion_Fosil_privada
                Costo_Gobierno_Politica_NoFosil_Privada <- Costo_Desarrollo_Capacidad_NoFosil_privada * Impuesto_a_la_inversion
                Costo_Gobierno_Politica_NoFosil_Publica <- Costo_Desarrollo_Capacidad_NoFosil_publica * Impuesto_a_la_inversion
                
                #State Variable
                dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                
                
                #Output model results
                list(
                    c(
                        #State Variables
                        dCapacidad_NoFosil_privada,
                        dCapacidad_Fosil_privada,
                        dCapacidad_NoFosil_publica,
                        dCapacidad_Fosil_publica
                    ),
                    #Flow variables
                    EmisionesCO2e = EmisionesCO2e,
                    CNG_sistema = CNG_sistema, # [Millones MXN/ TWH]
                    Deficit_Generacion =  Deficit_Generacion, # [TWH]
                    Efecto_baterias_Factor_de_planta_NoFosil= Efecto_baterias_Factor_de_planta_NoFosil,
                    Total_Inversion_Sector_Publica = Total_Inversion_Sector_Publica,
                    Total_Inversion_Sector_Privada = Total_Inversion_Sector_Privada
                    
                )
            })
        }
        
        parameters<-c(
            #NoFosil_privada
            Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector público
            Costo_Desarrollo_Capacidad_NoFosil_privada = input$Costo_Desarrollo_Capacidad_NoFosil_privada, #1228 # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_privada = input$Inversion_base_NoFosil_privada,#500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
            Vida_util_Capacidad_NoFosil_privada = 23, #[años]
            #NoFosil_publica
            Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_NoFosil_publica = input$Costo_Desarrollo_Capacidad_NoFosil_publica, #2857, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_publica = input$Inversion_base_NoFosil_publica, #100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_NoFosil_publica = 23, #[años]
            #Fosil_privada
            Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_privada = input$Costo_Desarrollo_Capacidad_Fosil_privada, #1739,  # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_privada = input$Inversion_base_Fosil_privada, #500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
            Vida_util_Capacidad_Fosil_privada = 28 , #[años]
            #Fosil_publica
            Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_publica = input$Costo_Desarrollo_Capacidad_Fosil_publica, #5822 , # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_publica = input$Inversion_base_Fosil_publica, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_Fosil_publica = 28 #[años]
        )
        
        
        InitialConditions <- c(
            Capacidad_NoFosil_privada = 15*1/2 , # [GW] , asumimos que el sector privado tiene 1/2 de la capacidad instalada
            Capacidad_Fosil_privada = 15 *1/2, # [GW] , asumimos que el sector público tiene 1/2 de la capacidad instalada
            Capacidad_NoFosil_publica = 44*1/2 , # [GW]
            Capacidad_Fosil_publica = 44*1/2  #[GW]
        )
        
        
        #years
        times <- seq(0, #initial time
                     40, #end of simulation #[year]
                     0.1)#time step #[year]
        
        intg.method<-c("rk4")
        
        #Simulate model
        
        out <- ode(y = InitialConditions,
                   times = times,
                   func = EnerMX3,
                   parms = parameters,
                   method =intg.method
        )
        #
        
        #Convertir Out en data frame
        out<-data.frame(out)
        
        #Agregar línea de tendencia en color rojo
        
        
        graf.total.inversion.publica <- ggplot(data = out, aes(x = time, y = Total_Inversion_Sector_Publica)) + 
            #geom_point(color = 'orchid2', size = 2, alpha = 0.6) +
            geom_smooth(color = 'orchid4') + 
            xlab('Tiempo') + 
            ylab('Inversion') +
            ggtitle('Total de Inversión Sector Público') + 
            theme_minimal()
        
        graf.total.inversion.privada <- ggplot(data = out, aes(x = time, y = Total_Inversion_Sector_Privada)) + 
            #geom_point(color = 'aquamarine4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'aquamarine2') + 
            xlab('Tiempo') + 
            ylab('Inversion') +
            ggtitle('Total de Inversión Sector Privado') + 
            theme_minimal()
        
        grid.arrange(graf.total.inversion.publica, graf.total.inversion.privada,
                     ncol = 2)
        
        
    })
    
    #PLOT 4
    output$EnerMX4 <- renderPlot({
        library("deSolve")
        
        EnerMX4 <- function(t, state, parameters) {
            with(as.list(c(state,parameters)), {
                #impact exógenos de la demanda
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.demanda.electricidad <- 300+10*times # demanda [TWH], mu= [TWH/año] de incremento de la demanda
                
                Demanda.Meta.Datos <- approxfun( x = times, #times
                                                 y = x.demanda.electricidad, # [TWH], basado en análisis de Mariana y Sabrina
                                                 method = "linear",
                                                 rule = 2)
                
                #
                
                #impacto exógeno, precios hidrocarburos
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.precios.hidrocarburos <- 1.0+(input$x.precios.hidrocarburos)*times #impuesto al carbono
                
                Precio_Hidrocarburos_Datos <- approxfun( x = times, #times
                                                         y = x.precios.hidrocarburos , # [1], basado en análisis de Mariana y Sabrina
                                                         method = "linear",
                                                         rule = 2)
                
                
                
                #choque de baterías
                inicio_choque <- 30
                efecto_baterias <- 1.0
                x.capacidad.baterias <- ifelse(times>inicio_choque, efecto_baterias+1.0,1.0 )
                
                Efecto_baterias_Factor_de_planta_NoFosil <- approxfun( x = times, #times
                                                                       y =  x.capacidad.baterias  , # [1], basado en análisis de Mariana y Sabrina
                                                                       method = "linear",
                                                                       rule = 2)
                
                #Total de Inversión Sector Público
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                
                Impuesto_a_la_inversion <- approx(c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0),
                                                  c(0,0,0,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion),
                                                  xout = t)$y
                
                #Auxiliary variables
                Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
                Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                  c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
                
                #NoFosil_privada
                Efecto_baterias_Factor_de_planta_NoFosil<-Efecto_baterias_Factor_de_planta_NoFosil(t)
                Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
                
                #NoFosil_publica
                Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
                
                #Fosil_privada
                Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
                CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
                
                #Fosil_publica
                Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
                CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
                
                #Costo Promedio Sistema
                CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
                
                
                #Precios relativos
                #dentro de sectores
                CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
                CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
                
                #entre sectores
                CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
                CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
                
                
                #Efecto de precios relativos
                #dentro sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,1e6),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                #Entre sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                          c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                
                
                #Niveles de inversión
                #Nofosil_privada
                Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada*(1+(Impuesto_a_la_inversion))
                
                #Nofosil_publica
                Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
                
                #Fosil_privada
                Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
                
                #Fosil_publica
                Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
                
                
                
                #Flow Variables
                
                #Capacidad_NoFosil_privada
                Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
                Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
                
                #Capacidad_Fosil_privada
                Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
                Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
                
                #Capacidad_NoFosil_publica
                Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
                Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
                
                #Capacidad_Fosil_publica
                Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_publica
                Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
                
                
                #Variables de salida
                EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
                Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
                Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
                Total_Inversion_Sector_Publica <- Inversion_NoFosil_publica + Inversion_Fosil_publica
                Total_Inversion_Sector_Privada <- Inversion_NoFosil_privada + Inversion_Fosil_privada
                Costo_Gobierno_Politica_NoFosil_Privada <- Costo_Desarrollo_Capacidad_NoFosil_privada * Impuesto_a_la_inversion
                Costo_Gobierno_Politica_NoFosil_Publica <- Costo_Desarrollo_Capacidad_NoFosil_publica * Impuesto_a_la_inversion
                
                #State Variable
                dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                
                
                #Output model results
                list(
                    c(
                        #State Variables
                        dCapacidad_NoFosil_privada,
                        dCapacidad_Fosil_privada,
                        dCapacidad_NoFosil_publica,
                        dCapacidad_Fosil_publica
                    ),
                    #Flow variables
                    EmisionesCO2e = EmisionesCO2e,
                    CNG_sistema = CNG_sistema, # [Millones MXN/ TWH]
                    Deficit_Generacion =  Deficit_Generacion, # [TWH]
                    Efecto_baterias_Factor_de_planta_NoFosil= Efecto_baterias_Factor_de_planta_NoFosil,
                    Costo_Gobierno_Politica_NoFosil_Privada = Costo_Gobierno_Politica_NoFosil_Privada,
                    Costo_Gobierno_Politica_NoFosil_Publica = Costo_Gobierno_Politica_NoFosil_Publica
                    
                )
            })
        }
        
        parameters<-c(
            #NoFosil_privada
            Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector público
            Costo_Desarrollo_Capacidad_NoFosil_privada = input$Costo_Desarrollo_Capacidad_NoFosil_privada, #1228 # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_privada = input$Inversion_base_NoFosil_privada,#500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
            Vida_util_Capacidad_NoFosil_privada = 23, #[años]
            #NoFosil_publica
            Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_NoFosil_publica = input$Costo_Desarrollo_Capacidad_NoFosil_publica, #2857, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_publica = input$Inversion_base_NoFosil_publica, #100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_NoFosil_publica = 23, #[años]
            #Fosil_privada
            Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_privada = input$Costo_Desarrollo_Capacidad_Fosil_privada, #1739,  # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_privada = input$Inversion_base_Fosil_privada, #500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
            Vida_util_Capacidad_Fosil_privada = 28 , #[años]
            #Fosil_publica
            Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_publica = input$Costo_Desarrollo_Capacidad_Fosil_publica, #5822 , # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_publica = input$Inversion_base_Fosil_publica, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_Fosil_publica = 28 #[años]
        )
        
        
        InitialConditions <- c(
            Capacidad_NoFosil_privada = 15*1/2 , # [GW] , asumimos que el sector privado tiene 1/2 de la capacidad instalada
            Capacidad_Fosil_privada = 15 *1/2, # [GW] , asumimos que el sector público tiene 1/2 de la capacidad instalada
            Capacidad_NoFosil_publica = 44*1/2 , # [GW]
            Capacidad_Fosil_publica = 44*1/2  #[GW]
        )
        
        
        #years
        times <- seq(0, #initial time
                     40, #end of simulation #[year]
                     0.1)#time step #[year]
        
        intg.method<-c("rk4")
        
        #Simulate model
        
        out <- ode(y = InitialConditions,
                   times = times,
                   func = EnerMX4,
                   parms = parameters,
                   method =intg.method
        )
        #
        
        #Convertir Out en data frame
        out<-data.frame(out)
        
        #Agregar línea de tendencia en color rojo
        
        
        graf.costo.gobierno.publica <- ggplot(data = out, aes(x = time, y = Costo_Gobierno_Politica_NoFosil_Publica)) + 
            #geom_point(color = 'turquoise3', size = 2, alpha = 0.6) +
            geom_smooth(color = 'turquoise1') + 
            xlab('Tiempo') + 
            ylab('Costo') +
            ggtitle('Costo de la política para el gobierno en el sector público') + 
            theme_minimal()
        
        graf.costo.gobierno.privada <- ggplot(data = out, aes(x = time, y = Costo_Gobierno_Politica_NoFosil_Privada)) + 
            #geom_point(color = 'dodgerblue4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'dodgerblue1') + 
            xlab('Tiempo') + 
            ylab('Costo') +
            ggtitle('Costo de la política para el gobierno en el sector privado') + 
            theme_minimal()
        
        grid.arrange(graf.costo.gobierno.publica, graf.costo.gobierno.privada,
                     ncol = 2)
        
        
    })
    
    #PLOT 5
    
    output$EnerMX5 <- renderPlot({
        library("deSolve")
        
        EnerMX5 <- function(t, state, parameters) {
            with(as.list(c(state,parameters)), {
                #impact exógenos de la demanda
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.demanda.electricidad <- 300+10*times # demanda [TWH], mu= [TWH/año] de incremento de la demanda
                
                Demanda.Meta.Datos <- approxfun( x = times, #times
                                                 y = x.demanda.electricidad, # [TWH], basado en análisis de Mariana y Sabrina
                                                 method = "linear",
                                                 rule = 2)
                
                #
                
                #impacto exógeno, precios hidrocarburos
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                x.precios.hidrocarburos <- 1.0+(input$x.precios.hidrocarburos)*times #impuesto al carbono
                
                Precio_Hidrocarburos_Datos <- approxfun( x = times, #times
                                                         y = x.precios.hidrocarburos , # [1], basado en análisis de Mariana y Sabrina
                                                         method = "linear",
                                                         rule = 2)
                
                
                
                #choque de baterías
                inicio_choque <- 30
                efecto_baterias <- 1.0
                x.capacidad.baterias <- ifelse(times>inicio_choque, efecto_baterias+1.0,1.0 )
                
                Efecto_baterias_Factor_de_planta_NoFosil <- approxfun( x = times, #times
                                                                       y =  x.capacidad.baterias  , # [1], basado en análisis de Mariana y Sabrina
                                                                       method = "linear",
                                                                       rule = 2)
                
                #Total de Inversión Sector Público
                times<-c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0)
                
                Impuesto_a_la_inversion <- approx(c(0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0),
                                                  c(0,0,0,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion,input$Impuesto_a_la_inversion),
                                                  xout = t)$y
                
                #Auxiliary variables
                Precio_Hidrocarburos<-Precio_Hidrocarburos_Datos(t)
                Efecto_Precio_Hidrocarburos_en_CNG_Fosil<-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                  c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=Precio_Hidrocarburos)$y
                
                #NoFosil_privada
                Efecto_baterias_Factor_de_planta_NoFosil<-Efecto_baterias_Factor_de_planta_NoFosil(t)
                Generacion_NoFosil_privada <- Capacidad_NoFosil_privada*Factor_de_plata_base_NoFosil_privada*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_privada <- Costo_Desarrollo_Capacidad_NoFosil_privada*Capacidad_NoFosil_privada/Generacion_NoFosil_privada
                
                #NoFosil_publica
                Generacion_NoFosil_publica <- Capacidad_NoFosil_publica*Factor_de_plata_base_NoFosil_publica*24*365*Efecto_baterias_Factor_de_planta_NoFosil # [GWH]
                CNG_NoFosil_publica <- Costo_Desarrollo_Capacidad_NoFosil_publica*Capacidad_NoFosil_publica/Generacion_NoFosil_publica
                
                #Fosil_privada
                Generacion_Fosil_privada <- Capacidad_Fosil_privada*Factor_de_plata_base_Fosil_privada*24*365 # [GWH]
                CNG_Fosil_privada <- Costo_Desarrollo_Capacidad_Fosil_privada*Capacidad_Fosil_privada*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_privada
                
                #Fosil_publica
                Generacion_Fosil_publica <- Capacidad_Fosil_publica*Factor_de_plata_base_Fosil_publica*24*365 # [GWH]
                CNG_Fosil_publica <- Costo_Desarrollo_Capacidad_Fosil_publica*Capacidad_Fosil_publica*Efecto_Precio_Hidrocarburos_en_CNG_Fosil/Generacion_Fosil_publica
                
                #Costo Promedio Sistema
                CNG_sistema <- mean(c(CNG_NoFosil_privada,CNG_NoFosil_publica,CNG_Fosil_privada,CNG_Fosil_publica))
                
                
                #Precios relativos
                #dentro de sectores
                CNG_relativo_NoFosil_privada_crd_Fosil_privada <- CNG_NoFosil_privada/CNG_Fosil_privada
                CNG_relativo_NoFosil_publica_crd_Fosil_publica <- CNG_Fosil_privada/CNG_Fosil_publica
                
                #entre sectores
                CNG_relativo_NoFosil_privada_crd_NoFosil_publica <- CNG_NoFosil_privada/CNG_NoFosil_publica
                CNG_relativo_Fosil_privada_crd_Fosil_publica <- CNG_NoFosil_publica/CNG_Fosil_publica
                
                
                #Efecto de precios relativos
                #dentro sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,1e6),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_privada_crd_Fosil_privada)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                              c(2.0,1.5,1.2,1.0,0.8,0.5,0.1,0.01),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                            c(0.0,05.5,0.8,1.0,1.2,1.5,2.0,2.0),xout=CNG_relativo_NoFosil_publica_crd_Fosil_publica)$y
                #Entre sectores
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                               c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_NoFosil_privada_crd_NoFosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada <- approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                          c(1.5,1.15,1.1,1.0,0.8,0.7,0.65,0.6),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica <-approx (c(0.0,0.5,0.8,1.0,1.2,1.5,2.0,100),
                                                                                                         c(0.65,0.7,0.8,1.0,1.1,1.15,1.5,1.5),xout=CNG_relativo_Fosil_privada_crd_Fosil_publica)$y
                
                
                
                
                #Niveles de inversión
                #Nofosil_privada
                Inversion_NoFosil_privada <- Inversion_base_NoFosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_NoFosil_Privada*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Privada*(1+(Impuesto_a_la_inversion))
                
                #Nofosil_publica
                Inversion_NoFosil_publica <- Inversion_base_NoFosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_NoFosil_publica*Efecto_CNG_relativo_NoFosil_privada_crd_NoFosil_publica_en_Inversion_NoFosil_Publica
                
                #Fosil_privada
                Inversion_Fosil_privada <- Inversion_base_Fosil_privada*Efecto_CNG_relativo_NoFosil_privada_crd_Fosil_privada_en_Inversion_Fosil_Privada*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Privada
                
                #Fosil_publica
                Inversion_Fosil_publica <- Inversion_base_Fosil_publica*Efecto_CNG_relativo_NoFosil_publica_crd_Fosil_publica_en_Inversion_Fosil_publica*Efecto_CNG_relativo_Fosil_privada_crd_Fosil_publica_en_Inversion_Fosil_Publica
                
                
                
                #Flow Variables
                
                #Capacidad_NoFosil_privada
                Perdida_Anual_Capacidad_NoFosil_privada <-  Capacidad_NoFosil_privada/Vida_util_Capacidad_NoFosil_privada
                Incremento_Anual_Capacidad_NoFosil_privada <- Inversion_NoFosil_privada/Costo_Desarrollo_Capacidad_NoFosil_privada
                
                #Capacidad_Fosil_privada
                Perdida_Anual_Capacidad_Fosil_privada <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_privada
                Incremento_Anual_Capacidad_Fosil_privada <- Inversion_Fosil_privada/Costo_Desarrollo_Capacidad_Fosil_privada
                
                #Capacidad_NoFosil_publica
                Perdida_Anual_Capacidad_NoFosil_publica <-  Capacidad_NoFosil_publica/Vida_util_Capacidad_NoFosil_publica
                Incremento_Anual_Capacidad_NoFosil_publica <- Inversion_NoFosil_publica/Costo_Desarrollo_Capacidad_NoFosil_publica
                
                #Capacidad_Fosil_publica
                Perdida_Anual_Capacidad_Fosil_publica <-  Capacidad_Fosil_privada/Vida_util_Capacidad_Fosil_publica
                Incremento_Anual_Capacidad_Fosil_publica <- Inversion_Fosil_publica/Costo_Desarrollo_Capacidad_Fosil_publica
                
                
                #Variables de salida
                EmisionesCO2e <- (Generacion_Fosil_privada + Generacion_Fosil_publica)*0.001 # , 0.001 [MTCO2e/GWH] fuente: https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
                Generacion_Total <- (Generacion_NoFosil_privada + Generacion_Fosil_privada+Generacion_NoFosil_publica + Generacion_Fosil_publica)/1000 # [TWH]
                Deficit_Generacion<- Demanda.Meta.Datos(t)-Generacion_Total
                Total_Inversion_Sector_Publica <- Inversion_NoFosil_publica + Inversion_Fosil_publica
                Total_Inversion_Sector_Privada <- Inversion_NoFosil_privada + Inversion_Fosil_privada
                Costo_Gobierno_Politica_NoFosil_Privada <- Costo_Desarrollo_Capacidad_NoFosil_privada * Impuesto_a_la_inversion
                Costo_Gobierno_Politica_NoFosil_Publica <- Costo_Desarrollo_Capacidad_NoFosil_publica * Impuesto_a_la_inversion
                
                #State Variable
                dCapacidad_NoFosil_privada <- Incremento_Anual_Capacidad_NoFosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_privada <- Incremento_Anual_Capacidad_Fosil_privada - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_NoFosil_publica <- Incremento_Anual_Capacidad_NoFosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                dCapacidad_Fosil_publica <- Incremento_Anual_Capacidad_Fosil_publica - Perdida_Anual_Capacidad_NoFosil_privada
                
                
                #Output model results
                list(
                    c(
                        #State Variables
                        dCapacidad_NoFosil_privada,
                        dCapacidad_Fosil_privada,
                        dCapacidad_NoFosil_publica,
                        dCapacidad_Fosil_publica
                    ),
                    #Flow variables
                    EmisionesCO2e = EmisionesCO2e,
                    CNG_sistema = CNG_sistema, # [Millones MXN/ TWH]
                    Deficit_Generacion =  Deficit_Generacion, # [TWH]
                    Efecto_baterias_Factor_de_planta_NoFosil= Efecto_baterias_Factor_de_planta_NoFosil,
                    Inversion_NoFosil_privada = Inversion_NoFosil_privada,
                    Inversion_NoFosil_publica = Inversion_NoFosil_publica,
                    Inversion_Fosil_privada = Inversion_Fosil_privada,
                    Inversion_Fosil_publica = Inversion_Fosil_publica
                    
                    
                    
                )
            })
        }
        
        parameters<-c(
            #NoFosil_privada
            Factor_de_plata_base_NoFosil_privada = 0.33*1.10 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector público
            Costo_Desarrollo_Capacidad_NoFosil_privada = input$Costo_Desarrollo_Capacidad_NoFosil_privada, #1228 # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_privada = input$Inversion_base_NoFosil_privada,#500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html
            Vida_util_Capacidad_NoFosil_privada = 23, #[años]
            #NoFosil_publica
            Factor_de_plata_base_NoFosil_publica = 0.33*0.9 , # [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_NoFosil_publica = input$Costo_Desarrollo_Capacidad_NoFosil_publica, #2857, # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_NoFosil_publica = input$Inversion_base_NoFosil_publica, #100*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_NoFosil_publica = 23, #[años]
            #Fosil_privada
            Factor_de_plata_base_Fosil_privada = 0.47*1.19 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_privada = input$Costo_Desarrollo_Capacidad_Fosil_privada, #1739,  # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_privada = input$Inversion_base_Fosil_privada, #500*20, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,
            Vida_util_Capacidad_Fosil_privada = 28 , #[años]
            #Fosil_publica
            Factor_de_plata_base_Fosil_publica = 0.47*0.90 ,# [1] asumimos que el factor de planta en el sector privado es 10% mayor que el del sector privado
            Costo_Desarrollo_Capacidad_Fosil_publica = input$Costo_Desarrollo_Capacidad_Fosil_publica, #5822 , # [Millones MXN/GW instalado], este costo marginal es estimado en función del factor de planta estimado para el estudio y asumiendo que todas las centrales tienen la misma vida económica y mismo tiempo de construcción
            Inversion_base_Fosil_publica = input$Inversion_base_Fosil_publica, #[Millones MXN], https://www.eleconomista.com.mx/empresas/Cambia-politica-en-electricidad-y-cae-IED-del-sector-20201124-0020.html,,
            Vida_util_Capacidad_Fosil_publica = 28 #[años]
        )
        
        
        InitialConditions <- c(
            Capacidad_NoFosil_privada = 15*1/2 , # [GW] , asumimos que el sector privado tiene 1/2 de la capacidad instalada
            Capacidad_Fosil_privada = 15 *1/2, # [GW] , asumimos que el sector público tiene 1/2 de la capacidad instalada
            Capacidad_NoFosil_publica = 44*1/2 , # [GW]
            Capacidad_Fosil_publica = 44*1/2  #[GW]
        )
        
        
        #years
        times <- seq(0, #initial time
                     40, #end of simulation #[year]
                     0.1)#time step #[year]
        
        intg.method<-c("rk4")
        
        #Simulate model
        
        out <- ode(y = InitialConditions,
                   times = times,
                   func = EnerMX5,
                   parms = parameters,
                   method =intg.method
        )
        #
        
        #Convertir Out en data frame
        out<-data.frame(out)
        
        #Agregar línea de tendencia en color rojo
        
        
        graf.inversion.no.fosil.priv <- ggplot(data = out, aes(x = time, y = Inversion_NoFosil_privada)) + 
            #geom_point(color = 'turquoise3', size = 2, alpha = 0.6) +
            geom_smooth(color = 'coral2') + 
            xlab('Tiempo') + 
            ylab('Millones') +
            ggtitle('Inversión Sector No Fosil Privada') + 
            theme_minimal()
        
        graf.inversion.no.fosil.pub <- ggplot(data = out, aes(x = time, y = Inversion_NoFosil_publica)) + 
            #geom_point(color = 'dodgerblue4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'olivedrab1') + 
            xlab('Tiempo') + 
            ylab('Millones') +
            ggtitle('Inversión Sector No Fosil Pública') + 
            theme_minimal()
        
        graf.inversion.fosil.priv <- ggplot(data = out, aes(x = time, y = Inversion_Fosil_privada)) + 
            #geom_point(color = 'turquoise3', size = 2, alpha = 0.6) +
            geom_smooth(color = 'navy') + 
            xlab('Tiempo') + 
            ylab('Millones') +
            ggtitle('Inversión Sector Fosil Privada') + 
            theme_minimal()
        
        graf.inversion.fosil.pub <- ggplot(data = out, aes(x = time, y = Inversion_Fosil_publica)) + 
            #geom_point(color = 'dodgerblue4', size = 2, alpha = 0.6) +
            geom_smooth(color = 'thistle3') + 
            xlab('Tiempo') + 
            ylab('Millones') +
            ggtitle('Inversión Sector Fosil Pública') + 
            theme_minimal()
        
        grid.arrange(graf.inversion.no.fosil.priv, graf.inversion.no.fosil.pub, graf.inversion.fosil.priv, graf.inversion.fosil.pub,
                     nrow = 2, ncol = 2)
        
        
    })
    
    
})
