pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr,dbscan, tidyverse, Rtsne, dplyr)


set.seed(43)

data  <- read_rds("beats.rds") %>%
  filter(!(is.na(data) | is.na(data))) %>% 
  as_tibble()

data2 <- sample(1:nrow(data),1000, replace=FALSE)
data2 <- data[data2,]
data2 %>% glimpse()


data_clean <- data3%>%

filter(!(is.na(danceability) | is.na(energy))) %>%
  
select(danceability,energy,loudness,mode,speechiness,acousticness,liveness,valence,time_signature,track_number)

data_clean %>% glimpse()

data_PCAproy <- data_clean %>% 
  prcomp(scale. = TRUE) %>% 
  predict() %>% 
  .[,1:2] %>% 
  as_tibble()

ggplot(data_PCAproy, aes(PC1, PC2)) + 
  geom_point(alpha = 0.5)

data_tsne <- data_clean %>% 
  unique() %>% 
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()
# visualizo tsne
ggplot(data_tsne, aes(V1, V2)) + 
  geom_point(alpha = 0.5)


# visualizo grafico de kNN
kNNdistplot(data_tsne, k = 4)
abline(h=1.7, col = "red", lty = 2)
dev.off() 

# hago un DBScan con parametro observado
modelo_dbscan <- dbscan(data_tsne, eps = 1.7, minPts = 5)

#visualizo
ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none")

modelo_dbscan$cluster %>% max()

max(modelo_dbscan$cluster)

modelo_dbscan$cluster %>% unique() %>% length()

estad <- data_clean %>% 
  mutate(cluster = modelo_dbscan$cluster) %>% 
  group_by(cluster) %>%
  summarise(mean(User_Score),
            mean(Critic_Score),
            mean(Global_Sales),
            mean(User_Count))

