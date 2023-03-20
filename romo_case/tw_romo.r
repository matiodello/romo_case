# Carga de librerías ------------------------------------------------------



library(readr)
library(readxl)
library(dplyr)
library(tidyverse) 
library(rvest) 
library(stringr)
library(pdftools)
library(textreadr)
library(pdftables)
library(tidytext)
library(curl)
library(rtweet)


# Bajada de Tweets --------------------------------------------------------



romo_cba1 <- search_tweets(q = paste(c("romo OR #romo OR #casoromo OR #jonathanromo"), 
                                                 collapse = '|'),n = 70000, 
                                       geocode = "-31.416668,-64.183334,350km",
                                       retryonratelimit = TRUE)

is.na(romo_cba1$bbox_coords)

asesinato_cba2 <- search_tweets(q = paste(c("asesinato OR detencion"), 
                                                 collapse = '|'),n = 70000, 
                                       geocode = "-31.416668,-64.183334,200km",
                                       retryonratelimit = TRUE)


mosquera_cba <- search_tweets(q = paste(c("mosquera OR @alfonmosquera OR #mosquera"),collapse = '|'),
                                      n = 70000,geocode = "-31.416668,-64.183334,350km",
                                       retryonratelimit = TRUE)


ministro_cba <- search_tweets(q = paste(c("'ministro de seguridad'"),collapse = '|'),
                              n = 70000,geocode = "-31.416668,-64.183334,200km",
                              retryonratelimit = TRUE)


ministerio_cba1 <- search_tweets(q = paste(c("@minsegcba"),collapse = '|'),
                              n = 70000,geocode = "-31.416668,-64.183334,200km",
                              retryonratelimit = TRUE)

ministerio_cba12 <- search_tweets(q = paste(c("'ministerio de seguridad'"),collapse = '|'),
                              n = 70000,geocode = "-31.416668,-64.183334,200km",
                              retryonratelimit = TRUE)

autopsia <- search_tweets(q = paste(c("autopsia"),collapse = '|'),
                                 n = 70000,geocode = "-31.416668,-64.183334,200km",
                                 retryonratelimit = TRUE)

asfixia <- search_tweets(q = paste(c("'asfixia mecanica'"),collapse = '|'),
                          n = 70000,geocode = "-31.416668,-64.183334,200km",
                          retryonratelimit = TRUE)




correa_cba <- search_tweets(q = "'blas correa'", n = 70000, 
                                       geocode = "-31.416668,-64.183334,350km",
                                       retryonratelimit = TRUE)

correa_cba_1 <- search_tweets(q = "'caso blas'", n = 70000, 
                            geocode = "-31.416668,-64.183334,350km",
                            retryonratelimit = TRUE)

correa_cba_2 <- search_tweets(q = "'blas correas'", n = 70000, 
                              geocode = "-31.416668,-64.183334,350km",
                              retryonratelimit = TRUE)

correa_cba_3 <- search_tweets(q = "blas", n = 70000, 
                              geocode = "-31.416668,-64.183334,350km",
                              retryonratelimit = TRUE)


policia_cba1 <- search_tweets(q = paste(c("policia OR policial OR cana OR yuta OR yutas OR policias OR cobani OR covani OR covanis OR cobanis OR canas OR agente OR comisario OR subcomisario OR comisaria OR subcomisaria OR sargento OR agente OR cabo OR comisarias OR subcomisarias OR subcomisarios OR comisarios OR sargentos OR cabos"), 
                                              collapse = '|'), 
                                    n = 70000, 
                                    geocode = "-31.416668,-64.183334,200km",
                                    retryonratelimit = TRUE)



policia_caba1 <- search_tweets(q = paste(c("policia OR policial OR cana OR yuta OR yutas OR policias OR cobani OR covani OR covanis OR cobanis OR canas OR agente OR comisario OR subcomisario OR comisaria OR subcomisaria OR sargento OR agente OR cabo OR comisarias OR subcomisarias OR subcomisarios OR comisarios OR sargentos OR cabos"), 
                                              collapse = '|'), 
                                    n = 70000, 
                                    geocode = "-36.166320,-60.265170,300km",
                                    retryonratelimit = TRUE)

policia_caba <- rbind(policia_caba1, policia_caba)

policia_caba <- policia_caba1 %>%
  filter(!duplicated(cbind(created_at, user_id)))



schiaretti_cba <- search_tweets(q = paste(c("gobernador OR Schiaretti OR Schoretti OR @Jschiaretti"), 
                                           collapse = '|'), 
                                 n = 70000, 
                                 geocode = "-31.416668,-64.183334,200km",
                                 retryonratelimit = TRUE)

schiaretti_cba1 <- search_tweets(q = paste(c("@JSchiaretti"), 
                                          collapse = '|'), 
                                n = 70000, 
                                geocode = "-31.416668,-64.183334,200km",
                                retryonratelimit = TRUE)

belletti_tw <- search_tweets(q = paste(c("belletti"), 
                                         collapse = '|'), 
                               n = 70000, 
                               geocode = "-31.416668,-64.183334,200km",
                               retryonratelimit = TRUE)

kelm <- search_tweets(q = paste(c("kelm"), 
                                       collapse = '|'), 
                             n = 70000, 
                             geocode = "-31.416668,-64.183334,200km",
                             retryonratelimit = TRUE)

ffss <- search_tweets(q = paste(c("'fuerzas de seguridad'"), 
                                collapse = '|'), 
                      n = 70000, 
                      geocode = "-31.416668,-64.183334,200km",
                      retryonratelimit = TRUE)

ivana <- search_tweets(q = paste(c("'Ivana Rossi'"), 
                                collapse = '|'), 
                      n = 70000, 
                      geocode = "-31.416668,-64.183334,400km",
                      retryonratelimit = TRUE)


# Construcción de dfs -----------------------------------------------------

#Vamos a juntar todos los tweets

tw_todos <- rbind(belletti_tw, correa_cba, correa_cba_1, correa_cba_2, ministerio_cba1, ministro_cba, mosquera_cba, policia_cba1, romo_cba1, schiaretti_cba, kelm, ffss, autopsia, asfixia, romo_cba1)

romo <- rbind(romo_cba1, romo_cba)

#Eliminamos aquellos que puedan estar repetidos

tw_todos <- tw_todos %>%
  filter(!duplicated(cbind(created_at, user_id)))

romo <- romo %>%
  filter(!duplicated(cbind(created_at, user_id)))

#Me voy a quedar con los tweets de los medios de comunicación

#La voz


tw_todos <- read_csv("tw_todos.csv")

#Vamos a ver la localización

location <- tw_todos  %>% group_by(screen_name) %>%
  count(location, sort = TRUE)

#Vamos a ver la localización

location <- filtrado  %>% group_by(screen_name) %>%
  count(location, sort = TRUE)

#Me trae muchos twrrts de otros lugares, voy a limpiar eso. Como la mayoría son de los tweets son de san juan y santa fe, voy a borrar los que empiezan con esa localización

         
tw_todos <- tw_todos %>%
  filter(!str_detect(location, "Santa Fe") & !str_detect(location, "^Rosario"))

lafalda <- tw_todos %>%
  filter(str_detect(location, "La Falda"))

#Una vez que hice esto, voy a analizar algunas cosas


#Quiero quedarme solamente con los tweets que hacen mención a los medios de comunicación

tw_medios <- tw_todos %>%
  filter(str_detect(screen_name, "LAVOZcomar|canalccordoba|telefecordoba|ctnewsweb|Lmdiariocomar|ElDoce|diariocarlospaz|hoydiacordoba|LagartoShowOk|radiosuquia|Cba24Ncomar|RadioSucesos|ContinentalCba|DiarioAlfil|Cadena3Com|CarlosPazVivo|viacordobacomar|APNLaPampa|ElDiariodelaRep|universalmedios|lajornadavcp|ElDiariodeOliva|DiarioPuntal|el_resaltadorok|EnfanTerribleOk|latintacba|TdcRed"))

lavoz_romo <- tw_todos %>%
  filter(str_detect(screen_name, "LAVOZcomar"))

lavoz <- read_csv("~/Documents/R-Projects/Proyectos_Universidad/lavoz.csv")

lavoz <- tw_todos %>%
  filter(!duplicated(cbind(created_at, user_id)))

tdcred <- get_timeline("TdcRed", n = 3200) %>%
  filter(created_at > "2021-07-01" & created_at <="2021-07-17")


library(ggplot2)

ggplot(tdcred, aes(x = created_at)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE)

tw_sinmedios <- tw_todos %>%
  filter(!str_detect(screen_name, "LAVOZcomar|canalccordoba|telefecordoba|ctnewsweb|Lmdiariocomar|ElDoce|diariocarlospaz|hoydiacordoba|LagartoShowOk|radiosuquia|Cba24Ncomar|RadioSucesos|ContinentalCba|DiarioAlfil|Cadena3Com|CarlosPazVivo|viacordobacomar|APNLaPampa|ElDiariodelaRep|universalmedios|lajornadavcp|ElDiariodeOliva|DiarioPuntal|el_resaltadorok|EnfanTerribleOk|latintacba|TdcRed"))%>%
  count(screen_name, sort = TRUE)

tw_todos <- tw_todos %>% 
  mutate(interaccion = (favorite_count + retweet_count)) 

# Cómo evolucionó el nivel de interacción? --------------------------------

#Vamos a considerar al nivel de interacción como una suma entre los favs y los rt

mentions_blas <- mentions_blas %>% 
  mutate(interaccion = (favorite_count + retweet_count)) 

ggplot(ritmo_interacción, aes(x = created_at)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE)

mentions_blas %>% 
  ggplot(aes(created_at, interaccion)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE)

median(ritmo_interacción$interaccion)

#Hay un solo tweet que me está ensuciando todo, y que encima no tiene que ver con el tema, lo voy a limpiar

tw_todos <- mentions_blas %>% 
  filter(interaccion < 5000)

# Cuál fue el ritmo de interacciones por día --------------------------------

#Voy a armar un df con el ritmo de publicación de los tweets

tw_todos <- tw_todos %>% 
  separate(created_at, c("fecha_ingreso", "hora_ingreso"), sep = " ", remove = FALSE)  


library(lubridate)

tw_todos <- tw_todos %>%
  mutate(fecha = floor_date(created_at, unit = "1 day"))

#Ahora quiero sumar todos aquellos interacciones que sean del mismo día

interaccion_diaria <- tw_todos  %>% 
  group_by(fecha) %>% 
  summarise(interaccion = sum(interaccion))

#Esto no me basta, me gustaría tener el promedio entre el nivel de interacciones y la cantidad de publicaciones por día

tw_todos <- tw_todos %>%
  group_by(fecha) %>% 
  mutate(tweets_pordia = count(created_at)) %>% 
  group_by(fecha) %>% 
  summarise(interaccion = sum(interaccion))
  


# ¿Cuál es el ritmo de publicación de los tweets? -------------------------

library(ggplot2)

ggplot(lavoz, aes(x = created_at)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE)

min(tw_todos$created_at)
max(tw_todos$created_at)

# # ¿cuál es el tweet que más interacción generó? ----------------------- --------

top_tw_int <- tw_medios %>%
  group_by(screen_name) %>% 
  top_n(3, interaccion) %>%
  arrange(desc(interaccion))

top_tw_fav_original <- tw_todos %>% 
  filter(!is_retweet == "TRUE") %>% 
  top_n(3, favorite_count)


# # ¿cuál es el tweet que más favs generó? ----------------------- --------

top_tw_fav <- tw_medios %>% top_n(3, favorite_count)

top_tw_fav_original <- tw_todos %>% 
  filter(!is_retweet == "TRUE") %>% 
  top_n(3, favorite_count)

# ¿cuál es el tweet que más retweets generó? ------------------------------

top_tw_rt <- tw_todos %>% top_n(3, retweet_count)

top_tw_rt_original <- tw_todos %>% 
  filter(!is_retweet == "TRUE")%>% 
  top_n(3, retweet_count)

# ¿cuál es el usuario que más rt generó? ------------------------------

top_tw_rt_user <- tw_todos %>% 
  group_by(screen_name) %>% 
  summarize(retweet_sum = sum(retweet_count)) %>% 
  top_n(5, retweet_sum) 

top_tw_rt_user_only <- only %>% 
  group_by(screen_name) %>% 
  summarize(retweet_sum = sum(retweet_count)) %>% 
  top_n(5, retweet_sum) 

top_tw_rt_user_original <- tw_todos %>% 
  filter(!is_retweet == "TRUE") %>% 
  group_by(screen_name) %>% 
  summarize(retweet_sum = sum(retweet_count)) %>% 
  top_n(3, retweet_sum) 

top_tw_rt_user_original_only <- only %>% 
  filter(!is_retweet == "TRUE") %>% 
  group_by(screen_name) %>% 
  summarize(retweet_sum = sum(retweet_count)) %>% 
  top_n(3, retweet_sum) 

# ¿cuál es el usuario que más favs generó? ------------------------------

top_tw_fav_user <- tw_todos %>% 
  group_by(screen_name) %>% 
  summarize(favorite_sum = sum(favorite_count)) %>% 
  top_n(3, favorite_sum) 

top_tw_fav_user_original <- tw_todos %>% 
  filter(!is_retweet == "TRUE") %>% 
  group_by(screen_name) %>% 
  summarize(favorite_sum = sum(favorite_count)) %>% 
  top_n(3, favorite_sum)

# # ¿cuál es el tweet que mencionó a la policia que más rts generó? ----------------------- --------

top_tw_rt_policia <- tw_todos %>% 
  filter(str_detect(text, "policía")) %>%
  top_n(3, retweet_count)

top_tw_rt_farias_original <- tw_todos %>% 
  filter(str_detect(text, "@fma01fma")) %>%
  filter(!is_retweet == "TRUE")%>% 
  top_n(3, retweet_count)

glimpse(top_tw_rt_cfk_original)

# # ¿cuál es el tweet que mencionó a farias que más favs generó? ----------------------- --------

top_tw_fav_farias <- tw_todos %>% 
  filter(str_detect(text, "@fma01fma")) %>%
  top_n(3, favorite_count)

top_tw_fav_farias_original <- tw_todos %>%
  filter(str_detect(text, "@fma01fma")) %>%
  filter(!is_retweet == "TRUE") %>% 
  top_n(3, favorite_count)

#Una cosa que podríamos hacer acá, que no estoy haciendo, es sacar un promedio de estos tweets, tanto favs como rts

# # ¿De los tweets cuál es el usuario más mencionado? ----------------------- --------
mentions_all <- tw_todos %>%
  count(mentions_screen_name, sort = TRUE)

#Esto es problemático, porque las menciones vienen todas juntas, entonces si en un tweet se mencionó a tres personas, esas tres menciones aparecen juntas, yo quiero que aparezcan separadas para poder contar cada una de las menciones.

#Lo que voy a hacer entonces es me quedo con los tweets que contengan por lo menos una mención, para reducir el universo de tweets y que me sea más fácil tokenizarlos. después, los tokenizo

tokkens_mentions <- tw_sinmedios %>%
  filter(str_detect(text, "@")) %>% 
  unnest_tokens(word, text, token = "tweets")

#Una vez que tokenicé, ahora me quedo solo con las menciones, esto me va a permitir individualizarlas. Y luego sí, les aplico un count

mentions_all_yes <- tokkens_mentions %>%
  filter(str_detect(word, "^@")) %>% 
  count(word, sort = TRUE)


# ¿De los tweets que mencionan a jschiaretti, a quién más se menciona y con qué frecuencia? ----------------------- --------

mentions <- tw_todos %>% 
  filter(str_detect(text, "@JSchiaretti|Schiaretti|gobernador|Gobernador|Schoretti|schoretti|jschiaretti")) %>%
  filter(str_detect(text, "@")) %>% 
  unnest_tokens(word, text, token = "tweets")

#Una vez que tokenicé, ahora me quedo solo con las menciones, esto me va a permitir individualizarlas. Y luego sí, les aplico un count

mentions_all_yes <- mentions %>%
  filter(str_detect(word, "^@")) %>% 
  group_by(screen_name) %>%
  count(word, sort = TRUE)


# # ¿Cuando se lo mencionó a Schiaretti, de qué se habló? -----------------

#Antes voy a pasar todos los tweets a minuscula, porque el str_detect es muy sensible

mentions_schiaretti <- tw_todos %>%
  filter(str_detect(text, "@JSchiaretti|Schiaretti|gobernador|Gobernador|Schoretti|schoretti|jschiaretti"))

write.csv(mentions_schiaretti, "menciones_schiaretti.csv")

# ¿cuál es el tweet que más favs generó? 

mentions_schiaretti_fav <- mentions_schiaretti %>% 
  top_n(3, favorite_count)

mentions_schiaretti_rt <- mentions_schiaretti %>% 
  top_n(3, retweet_count)

#Me voy a quedar con los tweets que hablan de 

# # ¿De los tweets de farias cuál es el usuario más mencionado? ----------------------- --------

#Esto es problemático, porque las menciones vienen todas juntas, entonces si en un tweet se mencionó a tres personas, esas tres menciones aparecen juntas, yo quiero que aparezcan separadas para poder contar cada una de las menciones.

#Lo que voy a hacer entonces es me quedo con los tweets que contengan por lo menos una mención, para reducir el universo de tweets y que me sea más fácil tokenizarlos. después, los tokenizo

tokkens_mentions_farias <- farias_tw %>%
  filter(str_detect(text, "@")) %>%
  unnest_tokens(word, text, token = "tweets")

#Una vez que tokenicé, ahora me quedo solo con las menciones, esto me va a permitir individualizarlas. Yluego sí, les aplico un count

mentions_all_farias <- tokkens_mentions_farias %>%
  filter(str_detect(word, "^@")) %>% 
  count(word, sort = TRUE)

# # ¿De los tweets de farias cuál es el usuario más mencionado? ----------------------- --------

#Esto es problemático, porque las menciones vienen todas juntas, entonces si en un tweet se mencionó a tres personas, esas tres menciones aparecen juntas, yo quiero que aparezcan separadas para poder contar cada una de las menciones.

#Lo que voy a hacer entonces es me quedo con los tweets que contengan por lo menos una mención, para reducir el universo de tweets y que me sea más fácil tokenizarlos. después, los tokenizo

tokkens_mentions_tassi <- tassi_tw_of %>%
  filter(str_detect(text, "@")) %>%
  unnest_tokens(word, text, token = "tweets")

#Una vez que tokenicé, ahora me quedo solo con las menciones, esto me va a permitir individualizarlas. Yluego sí, les aplico un count

mentions_all_tassi <- tokkens_mentions_tassi %>%
  filter(str_detect(word, "^@")) %>% 
  count(word, sort = TRUE)


# # ¿De dónde son los tweets? ---------------------------------------------


#Si bien todos los usuarios son de argentina, vamos a ver diferentes columnas de localización

#Primero vamos a observar dónde declaran que se encuentran

#localización

tw_todos  %>% count(location, sort = TRUE)

#pais

pais <- tw_todos  %>% group_by(screen_name) %>%
  count(country, sort = TRUE)

# ¿Quién sigue a quién? ---------------------------------------------

#Para hacer eso necesito generar un df con todos los IDs y los screen_names
glimpse(tw_todos)

id_names <- tw_todos %>% 
  select(screen_name, user_id) %>% 
  distinct()

id_names_tassi <- tassi_fd %>% 
  mutate(¿sigue? = "Si")

#Tassi

tassi_flw <- id_names %>%  
  left_join(tassi_fd)


# Frecuencia de palabras --------------------------------------------------

#Averiguamos cuántas veces se usó cada palabra en cada texto

frequency <- tw_todos %>%
  unnest_tokens(word, text) %>%
  anti_join(my_stopwords, by = "word") %>%
  count(word, sort = TRUE)

marx_words

#Averiguamos cuántas veces se usó cada palabra en cada texto cuando se lo mencionó a schiaretti

frequency <- mentions_schiaretti %>%
  unnest_tokens(word, text) %>%
  anti_join(my_stopwords, by = "word") %>%
  count(word, sort = TRUE)

write.csv(frequency, "freq_schiaretti.csv")


marx_words

# Bigramas más comunes ----------------------------------------------------
library(tidytext)
my_stopwords <- read.csv("~/Documents/R-Projects/Clases/my_stopwords.csv")


words <- tw_todos %>%
  unnest_tokens(word, text) %>%
  anti_join(my_stopwords, by = "word")%>% 
  count(word, sort = TRUE)

#Tal y como observamos acá encontramos muchas palabras que no nos interesan. Lo que vamos a hacer entonces es a utilizar la función tidyr's separate(), que divide una columna en múltiples basándose en un delimitador. Esto nos permite separarlo en dos columnas, “palabra1” y “palabra2”, así podemos eliminar los casos en los que cualquiera de las dos sea una stopword
bigrams <- tw_todos %>%
  unnest_tokens(bigram, `text`, token = "ngrams", n = 2)

bigrams_select <- bigrams %>% 
  select(screen_name, bigrams, created_at)

bigrams_separados <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

filtrado <- bigrams_separados %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word) 

# nuevo conteo de bigramas:

bigram_counts <- filtrado %>% 
  count(word1, word2, sort = TRUE)


#Para graficar tenemos dos alternativas. la que vimos anteriormente, en la que podemos generar un
#gráfico de columnas. Para esto tenemos que unir palabras separadas en bigramas nuevamente

unidos <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")

unidos %>%
  top_n(20)


write.csv(unidos, "bigramas_schiaretti.csv")

unidos %>%
  slice_max(n, n = 10) %>% 
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col(fill = "#BFA7F5") +
  labs(y = NULL)



# trigramas más comunes ----------------------------------------------------

trigrams <- tw_todos %>%
  unnest_tokens(trigram, `text`, token = "ngrams", n = 3)

#Tal y como observamos acá encontramos muchas palabras que no nos interesan. Lo que vamos a hacer
#entonces es a utilizar la función tidyr's separate(), que divide una columna en múltiples basándose 
#en un delimitador. Esto nos permite separarlo en dos columnas, “palabra1” y “palabra2”, 
#así podemos eliminar los casos en los que cualquiera de las dos sea una stopword

trigrams_separados <- trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")


trigrams_filtrado <- trigrams_separados %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word) %>%
  filter(!word3 %in% my_stopwords$word)

# nuevo conteo de bigramas:
trigram_counts <- trigrams_filtrado %>% 
  count(word1, word2, word3, sort = TRUE)


#Para graficar tenemos dos alternativas. la que vimos anteriormente, en la que podemos generar un
#gráfico de columnas. Para esto tenemos que unir palabras separadas en bigramas nuevamente

trigram_unidos <- trigram_counts %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigram_unidos %>%
  top_n(20)



trigram_unidos %>%
  slice_max(n, n = 10) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(n, trigram)) +
  geom_col(fill = "#BFA7F5") +
  labs(y = NULL)



# Relación con Blas -------------------------------------------------------

#Del total de tweets, cúantos mencionan a blas?

mentions_blas <- tw_todos %>%
  filter(str_detect(text, "blas|correa|correas|#casoblas|Blas|Correa|Correas|#blascorreas"))

#De los tweets de los medios, cúantos mencionan a blas?

mentions_blas_md <- tw_medios %>%
  filter(str_detect(text, "blas|correa|correas|#casoblas|Blas|Correa|Correas|#blascorreas"))

#De los tweets que mencionan a blas, cuál es el usuario más mencionado?

tokkens_mentions <- mentions_blas %>%
  filter(str_detect(text, "@")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(str_detect(word, "^@")) %>% 
  count(word, sort = TRUE)

write.csv(tokkens_mentions, "menciones_blas.csv")


#De los tweets que mencionan a Blas, cuál fue el primero?

mentions_blas_date_arrange <- mentions_blas_md %>% arrange(created_at)

#Y cual fue la primera mención al caso?

tw_date_arrange <- romo %>% arrange(created_at)
tw_date_arrange <- lafalda %>% arrange(created_at)
tw_date_arrange <- lafalda %>% arrange(created_at)

#Cual es el usuario que más mencionó a Bás?

top_usuarios_blas <- tw_todos %>%
  filter(str_detect(text, "blas|correa|correas|#casoblas|Blas|Correa|Correas|#blascorreas")) %>% 
  count(screen_name, sort = TRUE)

#Quiero un df con las menciones a Blas

blas <- tw_todos %>%
  filter(str_detect(text, "blas|correa|correas|#casoblas|Blas|Correa|Correas|#blascorreas"))

#Cuántos tweets de la voz fueron acompañados por un URL

tw_medios %>%
  count(!is.na(urls_url))


# Cuánto se lo mencionó al ministro de seguridad --------------------------


