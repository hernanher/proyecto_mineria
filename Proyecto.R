pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr,dbscan, tidyverse, Rtsne, dplyr)


set.seed(43)

data  <- read_rds("beats.rds") 


data2 <- sample(1:nrow(data),1000, replace=FALSE)
data3 <- data[data2,]
data3 %>% glimpse()


data_clean <- data3%>%

select(danceability,energy,loudness,mode,speechiness,acousticness,liveness,valence,time_signature,track_number)

data_clean %>% glimpse()

validar=0
i=1
song <- readline(prompt = "Ingrese el nombre de la canción: ")
while(validar!=1){
  for (i in 1:nrow(data3)) {
    if(song==data3[i,27]) {
      validar=1
      indice=i
      cat("La canción",song,"está en la base de datos :D")
      break
    }
  }
  if(validar==0){
    song <- readline(prompt = "Por favor ingrese una canción que esté en la base: ")
  }
}



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
abline(h=2, col = "red", lty = 2)
dev.off() 

# hago un DBScan con parametro observado
modelo_dbscan <- dbscan(data_tsne, eps = 2, minPts = 9)

#visualizo
ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none")

modelo_dbscan$cluster %>% max()

max(modelo_dbscan$cluster)

modelo_dbscan$cluster %>% unique() %>% length()



clusters= as.data.frame(modelo_dbscan$cluster)
cat("La canción",song,"pertenece al cluster",clusters[indice,1])


