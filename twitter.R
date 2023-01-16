#Librerias
library(tidyverse)
library(rtweet)

# Query de twitter 
query <- "CDMX"
tweets <- search_tweets(query,
              n = 500,
              include_rts = FALSE,
              retryonratelimit = TRUE)

query2 <- "Metro de la ciudad de México  (Metro, OR CDMX) lang:es until:2023-01-16 since:2022-12-01 -filter:replies"
tweets_2 <- search_tweets(query2,
                          n = 500,
                          include_rts = FALSE,
                          retryonratelimit = FALSE)

#tweets de usa 
usa <- stream_tweets(c(-125, 26, -65, 49),timeout = 30)

#tweets de CDMX
cdmx <- stream_tweets(c(-99.39,19.04,-98.84,19.58), timeout = 30)

# EXTRAER DATA DEL TIMELINE DE UN USUARIO
tweetsMyAccount = get_timeline(user = "@POTUS", n = 1500, lang="en", 
                               include_rts = FALSE)
saveRDS(tweetsMyAccount, "myTweets.RDS")
tweetsMyAccount %>% 
  head()

# TOP TUITS POR CONTEO DE LIKES
tweetsMyAccount = tweetsMyAccount %>% 
  arrange(desc(favorite_count))
tweetsMyAccount %>% 
  head(10) %>% 
  select(text, created_at, favorite_count) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 12)

# FRECUENCIA DIARIA DE TUITS deepskyblue3
ts_plot(tweetsMyAccount, by="day", color="darkred") +
  labs(x = "MES", y = "# DE TWEETS", title = "¿Cuál es la frecuencia de tweets a lo largo del tiempo?", 
       subtitle = "Frecuencia de tweets por día")

# FRECUENCIA DE TUITS POR DÍA DE LA SEMANA
ggplot(tweetsMyAccount, mapping = aes(x=wday(created_at, label = TRUE))) + 
  labs(y = "Count", x = "Día de la semana", 
       title = "¿Cuál es la frecuencia de mis tuits a lo largo del tiempo?", 
       subtitle = "Frecuencia de tuits por día de la semana") +
  geom_bar(aes(fill = ..count..))

# FRECUENCIA DE TUITS POR DÍA DE LA SEMANA Y HORA 
tweetsMyAccount %>% 
  mutate(day = wday(created_at, label = TRUE)) %>% 
  mutate(hour = hour(with_tz(created_at, "America/Mexico_City"))) %>% 
  plot_ly(x = ~day, y = ~hour, colors = "PuBu") %>% 
  add_histogram2d(nbinsx = 7, nbinsy = 24) %>%
  layout(title = "Tweets per day of the week and hour")

# UBICACIÓN DE TUITS
tweetsMyAccount %>%
  filter(place_full_name != "", !is.na(place_full_name)) %>% 
  count(place_full_name) %>% 
  top_n(13, n) %>% 
  ggplot(aes(x = reorder(place_full_name, n), y = n)) +
  geom_col(aes(fill = n)) +
  coord_flip() +
  labs(title = "¿Desde qué ubicaciones he publicado más tuits?", 
       subtitle = "Top 13 lugares",
       x = "Ubicación / Nombre del lugar",
       y = "Count") +
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")


#------------------------------------------------------------------

tweets <- search_tweets(q = "#ClimateEmergency", 
                        n = 18000,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        lang = "en")

#CONVERTIR LOS TWEETS EN UN .CSV y .xlsx
write_as_csv(tweets, "tweets.csv")

#TOP HASHTAG "checarlo"
tweets %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#ClimateEmergency") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)

# MAPA UBICACIÓN DE TUITS
mapMyTweets <- lat_lng(tweets_2)
mapMyTweetsGeo <- mapMyTweets %>%
  filter(is.na(lat) == FALSE & is.na(lng) == FALSE)
mapMyTweetsGeoSF <- st_as_sf(mapMyTweetsGeo, coords = c("lng", "lat"), 
                             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")
leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircles(data = mapMyTweetsGeoSF, 
             color = "blue")




