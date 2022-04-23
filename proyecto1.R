pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr,dbscan, tidyverse, Rtsne, dplyr)


set.seed(42)

data  <- read_rds("beats.rds") %>%
  filter(!(is.na(data) | is.na(data))) %>% 
  as_tibble()

data2 <- sample(1:nrow(data),1000, replace=FALSE)
data3 <- data[data2,]
data3 %>% glimpse()


data_clean <- data3%>%
  filter(!(is.na(danceability) | is.na(energy)| is.na(key)|) %>%
           select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,track_number)
         
         
         data_clean %>% glimpse()