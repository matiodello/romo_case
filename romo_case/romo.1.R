
# Carga de librerías ------------------------------------------------------


library(tibble)
library(tidyverse)
library(dplyr)
library(lubridate) # Manejo y transformación de datos FECHA
library(hrbrthemes) # temas adicionales, componentes de temas y utilidades para 'ggplot2'
library(viridis) # escalas de color ggplot
library(funModeling) # caja de herramientas de análisis exploratorio de datos y preparación de datos
library(tidytext) # preparación de datos en formato texto
library(stringr) # Funciones diseñadas para hacer que trabajar con cadenas 
library(ggplot2) # Paquete de visualizaciones
library(scales) # infraestructura de escalado interna utilizada por ggplot2
library(igraph) # Análisis y Visualización de Redes
library(ggraph) # Implementación de gramática de gráficos para gráficos y redes
library(rtweet) # Cliente R para acceder a las API REST y stream de Twitter
library(quanteda) # Paquete R para administrar y analizar datos textuales 
library(stringi) # Herramientas de procesamiento de cadenas de caracteres/texto/lenguaje natural para la búsqueda de patrones 
library(vroom) # leer y escribir datos (como 'csv', 'tsv' y 'fwf') rápidamente
library(readr) # leer y escribir datos (como 'csv', 'tsv' y 'fwf') rápidamente
library(readxl) # leer archivos de Excel
library(knitr) # generación de informes dinámicos en R utilizando técnicas de programación literaria.
library(openxlsx) # leer, escribir y editar archivos xlsx
library(simplevis) # Visualizaciones ggplot simplificadas
library(widyr) #Encontrar correlaciones entre textos
library(ggraph) #plottear redes/mapas
library(igraph) #plottear redes/mapas
library(janitor)
library(quanteda)
library(dplyr)
library(readr)
library(quanteda.textplots)

# Carga de datasets -------------------------------------------------------


df.1 <- read_xlsx("14.07.xlsx")
df.2 <- read_xlsx("21.07.xlsx")
df.3 <- read_xlsx("28.07.xlsx")

my_stopwords <- read_csv("my_stopwords.csv")




# Unificación de df -------------------------------------------------------

# Unificamos los 3 df a través de <rbind>

general <- rbind(df.1, df.2, df.3)


# Limpiamos df sobrantes
rm(df.1, df.2, df.3)


# Normalizamos los nombres de variables

general <- clean_names(general)
colnames(general)


# Exploración de la información -------------------------------------------

romo <- general %>% 
  filter(etiqueta == "Violencia Institucional")

write.csv(romo ,"romo.csv")


romo_coments <- read_xlsx("romo.xlsx")




# Análisis de texto -------------------------------------------------------


# Análisis de texto: SCHIARETTI -------------------------------------------



# Tokkenizamos las palabras en las menciones a Schiaretti:

tokken_mentions <- mentions_schiaretti %>% 
  unnest_tokens(word, "text")%>% 
  anti_join(my_stopwords) %>%
  filter(word != "na") %>% 
  count(word, sort = TRUE)




schiaretti_oracion <- mentions_schiaretti %>% 
  unnest_tokens(oracion, text, token = "sentences")

schiaretti_dm <- schiaretti_oracion %>%
  filter(str_detect(oracion, paste(c("schiaretti"),collapse = '|')))


schiaretti_dfm <- schiaretti_dm %>%
  unnest_tokens(word, oracion, drop = FALSE) %>%
  filter(!str_detect(word, paste(c("^http", "t.co", "dst_jpg_s600x600"),collapse = '|'))) %>%
  count(`oracion`, word) %>%
  anti_join(my_stopwords) %>%
  cast_dfm(`oracion`, word, n)


top_words_timelines <- names(topfeatures(schiaretti_dfm, 10)) 
words_timelines_fcm <- fcm(schiaretti_dfm)
words_fcm <- fcm_select(words_timelines_fcm, pattern = top_words_timelines) #Me quedo con las palabras  que estan en el top 10

textplot_network(words_fcm, min_freq = 0.1, edge_color = "#008037", edge_alpha = 0.5, edge_size = 0.8, omit_isolated = TRUE)






# Análisis de texto: ROMO -------------------------------------------------





romo_td <- romo_coments %>% 
  unnest_tokens(word, "comentarios_27")%>% 
  anti_join(my_stopwords) %>%
  filter(word != "na") %>% 
  count(word, sort = TRUE)

write.csv(romo_td, "romo_freq.csv")


romo_bigrams <- romo_coments %>%
  unnest_tokens(bigram, comentarios_27, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word) %>%
  count(word1, word2, sort = TRUE)


unite_romo <- romo_bigrams %>% 
  unite(bigram, c("word1", "word2"))
write.csv(unite_romo, "unite_romo.csv")



romo_oracion <- romo_coments %>% 
  unnest_tokens(oracion, comentarios_27, token = "sentences")

romo_schiaretti <- romo_oracion %>%
  filter(str_detect(oracion, paste(c("inseguridad"),collapse = '|')))


romo_dfm <- romo_schiaretti %>%
  unnest_tokens(word, oracion, drop = FALSE) %>%
  filter(!str_detect(word, paste(c("^http", "t.co", "dst_jpg_s600x600"),collapse = '|'))) %>%
  count(`oracion`, word) %>%
  anti_join(my_stopwords) %>%
  cast_dfm(`oracion`, word, n)


top_words_timelines <- names(topfeatures(romo_dfm, 10)) 
words_timelines_fcm <- fcm(romo_dfm)
words_fcm <- fcm_select(words_timelines_fcm, pattern = top_words_timelines) #Me quedo con las palabras  que estan en el top 10

textplot_network(words_fcm, min_freq = 0.1, edge_color = "#008037", edge_alpha = 0.5, edge_size = 0.8, omit_isolated = TRUE)

