## Instalacion de librerias
# Instalar - Cargar tidyverse                                                       
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

#OBTENER NÚMERO NETO DE ARTÍCULOS CON PALABRA VIOLÓ Y VIOLADA

#Al tener la base de datos con los artículos netos podemos utilizar una gráfica
#para visualizar cuantos artículos coinciden con la búsqueda

dir.articulos<-"/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/"
articulos<-read.csv(paste0(dir.articulos,"ArticulosNetos.csv"))

#Gráfica para contabilizar la cantidad de artículos por "Violada" y "Violo"
articulos %>%
  ggplot() +
  aes(x=as.factor(Palabra), fill=as.factor(Palabra)) +
  geom_bar() +
  scale_fill_manual(values = c("purple", "green") ) +
  theme(legend.position = "top")

#Guardar gráfica en buena calidad
ggsave(
  'NumArticulos.jpg',
  path = "/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/",
  width = 30,
  height = 20,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

#ANÁLISIS DE SENTIMIENTOS EN LOS MEDIOS DE COMUNICACIÓN

#Ya que la muestra era muy pequeña, decidimos incluir artículos que dentro de sus 
# resumenes incluyeran la palabra "violó" y "violada"

dir.articulos<-"/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/"
articulos<-read.csv(paste0(dir.articulos,"240Art.csv"))

#Elegimos la visualización de los datos
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif",size=13),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#BF55FF", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#BF55FF", colour = NA))

#Usaremos el léxico Afinn para ver que tan positiva o negativamente son percibidas las palabras
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  as_tibble()

#Distribuimos los artículos por fechas
articulos <- 
  articulos %>%
  separate(Fecha, into = "Fecha", sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(Titular))

#Convertir las títulos a palabras
articulos_afinn <- 
  articulos %>%
  unnest_tokens(input = "Titular", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))

#Obtenemos tambíen una puntuación por Artículos
articulos <-
  articulos_afinn %>%
  group_by(ID) %>%
  summarise(Puntuacion_art = mean(Puntuacion)) %>%
  left_join(articulos, ., by = "ID") %>% 
  mutate(Puntuacion_art = ifelse(is.na(Puntuacion_art), 0, Puntuacion_art)) 

#Exploramos los artículos por medio
articulos_afinn %>%
  count(Medio)

# Artículos únicos por medio
articulos_afinn %>% 
  group_by(Medio) %>% 
  distinct(Palabra) %>% 
  count()

#Palabras positivas y negativas más usadas por cada medio en sus artículos
map(c("Positiva", "Negativa"), function(sentimiento) {
  articulos_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Medio) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Medio) +
    geom_col() +
    facet_wrap("Medio", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})

#Guardar gráfica en buena calidad
ggsave(
  'positivo.jpg',
  path = "/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/",
  width = 30,
  height = 20,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)


#Quizá la muestra esta un poco sesgada por lo que podemos nivelarlo un poco
articulos_afinn_medio <-
  articulos_afinn %>%
  group_by(ID) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Medio, Fecha) %>%
  summarise(Media = mean(Puntuacion))

#Graficamos
articulos_afinn_medio %>%
  ggplot() +
  aes(Fecha, Media, color = Medio) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")

#La separamos por medio
articulos_afinn_medio %>%
  ggplot() +
  aes(Fecha, Media, color = Medio) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Medio~.) +
  tema_graf +
  theme(legend.position = "none")

#Guardar gráfica en buena calidad
ggsave(
  'MediaxMedio.jpg',
  path = "/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/",
  width = 30,
  height = 20,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

#Regresion local para encontrar tendencias en los medios
articulos_afinn_medio %>%
  ggplot() +
  aes(Fecha, Media, color = Medio) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

#Tendencias más silimilares de las puntuaciones
articulos_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Medio) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

#Guardar gráfica en buena calidad
ggsave(
  'TendenciasxMedio.jpg',
  path = "/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/",
  width = 30,
  height = 20,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)


#Separamos la linea por medio para obtener un resultado más claro
articulos_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Medio) +
  geom_point(color = "#BF55FF") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Medio) +
  tema_graf

#Aplicamos una Regresión lineal ordinaria para ajustar una recta
articulos_afinn_medio %>%
  ggplot() +
  aes(Fecha, Media, color = Medio) +
  geom_point(color = "#BF55FF") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~Medio) +
  tema_graf

#Comparamos sentimientos positivos y negativos
articulos_afinn %>%
  count(Medio, Tipo) %>%
  group_by(Medio) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Medio, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")

#Guardar gráfica en buena calidad
ggsave(
  'PositivoVSNegativo.jpg',
  path = "/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/",
  width = 30,
  height = 20,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

#Comparamos sentimientos positivos y negativos en el tiempo
articulos_afinn %>%
  group_by(Medio, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Medio~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")

#ALGORITMO DE ALOCACIÓN LATENTE DE DIRICHLET PARA MODELADO DE TÓPICOS

#Iniciaremos con las búsquedas por "violó"
#Cargamos nuestras noticias
dir.articulos<-"/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/"
articulos<-read.csv(paste0(dir.articulos,"ArticulosViolo.csv"))

# Creamos un ID y nuestra categoria
articulos_1 = articulos %>%
  mutate(
    # Creamos un ID unico
    id2 = Titular,
    # Corregimos los nombres en autoria
    Autor = str_trim(
      str_remove_all(
        string = Autor,
        pattern = '\n|\\*|\\d+|V?\\.|/ ?[\\w]+'), 
      side = 'both'
    ),
    Autor = if_else(Autor=="", 'Editorial', Autor),
    # Eliminamos urls y espacios dobles
    texto_limpio = str_remove_all(
      string = Texto,
      pattern = 'https://.*|bit\\.ly.+'),
    texto_limpio = str_replace_all(texto_limpio, ' {2,}',' '),
  )

# Extraemos para encontrar nombres posibles victimas y agresores
nombres = articulos_1 %>%
  unnest_tokens(
    input = Titular, 
    output = 'nombres', 
    token = 'words',
    to_lower = FALSE
  ) %>%
  # Contamos cuantas veces aparecen
  count(nombres,  sort = TRUE) %>%
  # Filtramos con una expression regular para obtener solo nombres propios
  filter(
    str_detect(
      string = nombres,
      pattern = '[A-ZAÉÍÓÚÑ]\\w+ ?[[A-ZAÉÍÓÚÑ]\\w+ ?]*'
    )
  ) %>%
  # Unimos con un _ en vez de un espacio
  mutate(
    ners = str_replace_all(nombres, ' ', '_')
  ) %>%
  # Nos quedamos con el nombre original y las ners
  select(
    nombres, ners
  ) 


# Creamos un diccionario
nombres = setNames(nombres$ners, nombres$nombres)

# Definimos nuestras palabras de paro
stop_words = tibble(palabra = c(stopwords('es'),'así','si','sí','sino','ayer',
                                'hoy','año','sólo'))

# Creamos nuestra bolsa de palabras
articulos_tokens = articulos_1 %>%
  # Replazamos los nombres por las ners
  mutate(
    texto_limpio = str_replace_all(texto_limpio,nombres)
  ) %>%
  # Tokenizamos
  unnest_tokens(
    output = "palabra", 
    token = "words", 
    input = texto_limpio
  ) %>%
  # Removemos las palabras de paro
  anti_join(
    stop_words
  )  %>%
  # filtramos todo lo que no sea una palabra
  filter(
    !str_detect(palabra,'[^\\w_]|\\d'),
  )

# Creamos nuestra DTM
articulos_matriz = articulos_tokens %>%
  # Conteo de palabras por articulo
  count(id2, palabra) %>%
  # Creamos la dtm
  cast_dtm(
    # Identificador de cada documento
    document = id2, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n, 
    # Ponderadores
    weighting = tm::weightTf
  )

# Fijamos nuestro conjunto de prueba y entrenamiento
sample_size = floor(0.90 * nrow(articulos_matriz))
set.seed(1111)
train_ind = sample(nrow(articulos_matriz), size = sample_size)
train = articulos_matriz[train_ind, ]
test = articulos_matriz[-train_ind, ]


# Ajustamos nuestro modelo
lda_model = LDA(train, k = 2, method = 'Gibbs',
                control = list(seed = 1111))

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Estructura
str(lda_model)

# Definiendo el optimo
modelos = c()
for(k in seq(2,20)){
  # Ajustamos nuestro modelo
  lda_model = LDA(train, k = k, method = 'Gibbs',
                  control = list(seed = 1111))
  metricas = tibble(
    topics = k,
    perplejidad = perplexity(lda_model, newdata = test),
    verosimilitud = logLik(lda_model)[[1]]
  )
  modelos = bind_rows(modelos, metricas)
}

# Graficando las metricas
ggplot(
  data = modelos,
  aes(
    x = topics,
    y = perplejidad
  )
) +
  geom_point() +
  geom_line()

ggplot(
  data = modelos,
  aes(
    x = topics,
    y = verosimilitud
  )
) +
  geom_point() +
  geom_line()

#Podemos observar que el óptimo es 10, más al reunir artículos que hablan sobre violencia de género
#decidimos reducirlo a 5 para obtener temas menos ambiguos

# Ajustamos nuestro modelo final
lda_model = LDA(train, k = 5, method = 'Gibbs',
                control = list(seed = 1111))

# Estructura
str(lda_model)

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Probabilidad de documento perteneciente a un topico (gamma)
gammas = tidy(lda_model, matrix="gamma")
head(gammas)

# Podemos visualizar la propension de topicos en los documentos
gammas %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(
    aes(
      x=gamma,
      y=document, 
      fill = topic
    )
  ) + 
  geom_col()

# La matriz de probabilidades
gammas %>%
  spread(topic, gamma) 

# Mapa de calor
gammas %>%
  ggplot(
    aes(
      x = topic,
      y = document,
      fill = gamma
    )
  ) +
  geom_tile()

#Si agrupamos las gammas
grouped_gammas <- gammas %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)

# Numero de documentos por topico preponderante
grouped_gammas %>% 
  tally(topic, sort=TRUE)

# Presencia promedio de cada topico en los documentos
grouped_gammas %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))

# Probabilidad de palabra perteneciente a un topico (beta)
betas = tidy(lda_model, matrix="beta")
head(betas)

# Top15 de palabras para cada topico
terms(lda_model, k=15)

##NUBE DE PALABRAS
# Display wordclouds one at a time
for (j in 1:5) {
  # Generate a table with word frequences for topic j
  word_frequencies <- betas %>% 
    mutate(n = trunc(beta * 10000)) %>% 
    filter(topic == j)
  
  # Display word cloud
  wordcloud(words = word_frequencies$term, 
            freq = word_frequencies$n,
            max.words = 1000,
            #  min.freq=1,
            #  max.words=10,
            scale = c(3, 1),
            colors = c("Purple", "CornflowerBlue", "DarkRed"), 
            rot.per = 0)
}

###Ahora haremos los mismo pero con los artículos con la palabra "violada"
#Cargamos nuestras noticias
dir.articulos<-"/Users/marianacornejo/Desktop/R/Ciencia de Datos 2/"
articulos<-read.csv(paste0(dir.articulos,"ArticulosViolada.csv"))

# Creamos un ID y nuestra categoria
articulos_1 = articulos %>%
  mutate(
    # Creamos un ID unico
    id2 = Titular,
    # Corregimos los nombres en autoria
    Autor = str_trim(
      str_remove_all(
        string = Autor,
        pattern = '\n|\\*|\\d+|V?\\.|/ ?[\\w]+'), 
      side = 'both'
    ),
    Autor = if_else(Autor=="", 'Editorial', Autor),
    # Eliminamos urls y espacios dobles
    texto_limpio = str_remove_all(
      string = Texto,
      pattern = 'https://.*|bit\\.ly.+'),
    texto_limpio = str_replace_all(texto_limpio, ' {2,}',' '),
  )

# Extraemos para encontrar nombres posibles victimas y agresores
nombres = articulos_1 %>%
  unnest_tokens(
    input = Titular, 
    output = 'nombres', 
    token = 'words',
    to_lower = FALSE
  ) %>%
  # Contamos cuantas veces aparecen
  count(nombres,  sort = TRUE) %>%
  # Filtramos con una expression regular para obtener solo nombres propios
  filter(
    str_detect(
      string = nombres,
      pattern = '[A-ZAÉÍÓÚÑ]\\w+ ?[[A-ZAÉÍÓÚÑ]\\w+ ?]*'
    )
  ) %>%
  # Unimos con un _ en vez de un espacio
  mutate(
    ners = str_replace_all(nombres, ' ', '_')
  ) %>%
  # Nos quedamos con el nombre original y las ners
  select(
    nombres, ners
  ) 


# Creamos un diccionario
nombres = setNames(nombres$ners, nombres$nombres)

# Definimos nuestras palabras de paro
stop_words = tibble(palabra = c(stopwords('es'),'así','si','sí','sino','ayer',
                                'hoy','año','sólo'))

# Creamos nuestra bolsa de palabras
articulos_tokens = articulos_1 %>%
  # Replazamos los nombres por las ners
  mutate(
    texto_limpio = str_replace_all(texto_limpio,nombres)
  ) %>%
  # Tokenizamos
  unnest_tokens(
    output = "palabra", 
    token = "words", 
    input = texto_limpio
  ) %>%
  # Removemos las palabras de paro
  anti_join(
    stop_words
  )  %>%
  # filtramos todo lo que no sea una palabra
  filter(
    !str_detect(palabra,'[^\\w_]|\\d'),
  )

# Creamos nuestra DTM
articulos_matriz = articulos_tokens %>%
  # Conteo de palabras por articulo
  count(id2, palabra) %>%
  # Creamos la dtm
  cast_dtm(
    # Identificador de cada documento
    document = id2, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n, 
    # Ponderadores
    weighting = tm::weightTf
  )

# Fijamos nuestro conjunto de prueba y entrenamiento
sample_size = floor(0.90 * nrow(articulos_matriz))
set.seed(1111)
train_ind = sample(nrow(articulos_matriz), size = sample_size)
train = articulos_matriz[train_ind, ]
test = articulos_matriz[-train_ind, ]


# Ajustamos nuestro modelo
lda_model = LDA(train, k = 2, method = 'Gibbs',
                control = list(seed = 1111))

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Estructura
str(lda_model)

# Definiendo el optimo
modelos = c()
for(k in seq(2,20)){
  # Ajustamos nuestro modelo
  lda_model = LDA(train, k = k, method = 'Gibbs',
                  control = list(seed = 1111))
  metricas = tibble(
    topics = k,
    perplejidad = perplexity(lda_model, newdata = test),
    verosimilitud = logLik(lda_model)[[1]]
  )
  modelos = bind_rows(modelos, metricas)
}

# Graficando las metricas
ggplot(
  data = modelos,
  aes(
    x = topics,
    y = perplejidad
  )
) +
  geom_point() +
  geom_line()

ggplot(
  data = modelos,
  aes(
    x = topics,
    y = verosimilitud
  )
) +
  geom_point() +
  geom_line()

#Podemos observar que el óptimo es 10, más al reunir artículos que hablan sobre violencia de género
#decidimos reducirlo a 5 para obtener temas menos ambiguos

# Ajustamos nuestro modelo final
lda_model = LDA(train, k = 5, method = 'Gibbs',
                control = list(seed = 1111))

# Estructura
str(lda_model)

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Probabilidad de documento perteneciente a un topico (gamma)
gammas = tidy(lda_model, matrix="gamma")
head(gammas)

# Podemos visualizar la propension de topicos en los documentos
gammas %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(
    aes(
      x=gamma,
      y=document, 
      fill = topic
    )
  ) + 
  geom_col()

# La matriz de probabilidades
gammas %>%
  spread(topic, gamma) 

# Mapa de calor
gammas %>%
  ggplot(
    aes(
      x = topic,
      y = document,
      fill = gamma
    )
  ) +
  geom_tile()

#Si agrupamos las gammas
grouped_gammas <- gammas %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)

# Numero de documentos por topico preponderante
grouped_gammas %>% 
  tally(topic, sort=TRUE)

# Presencia promedio de cada topico en los documentos
grouped_gammas %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))

# Probabilidad de palabra perteneciente a un topico (beta)
betas = tidy(lda_model, matrix="beta")
head(betas)

# Top15 de palabras para cada topico
terms(lda_model, k=15)


#NUBE DE PALABRAS 
# Display wordclouds one at a time
for (j in 1:5) {
  # Generate a table with word frequences for topic j
  word_frequencies <- betas %>% 
    mutate(n = trunc(beta * 10000)) %>% 
    filter(topic == j)
  
  # Display word cloud
  wordcloud(words = word_frequencies$term, 
            freq = word_frequencies$n,
            max.words = 1000,
            #  min.freq=1,
            #  max.words=10,
            scale = c(3, 1),
            colors = c("Purple", "CornflowerBlue", "DarkRed"), 
            rot.per = 0)
}

