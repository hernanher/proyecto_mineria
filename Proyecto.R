pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr,dbscan, tidyverse, Rtsne, dplyr)

#Escogemos Semilla
set.seed(43)
#Leemos Data
data  <- read_rds("beats.rds") 

#Creamos una muestra de 1000 datos
data2 <- sample(1:nrow(data),1000, replace=FALSE)
data3 <- data[data2,]

#Vemos cuales variables nos interesan y las que no, las descartamos
#Por ejemplo, el año no lo consideramos relevante puesto que no es significativo para
#Clasificar una canción de acuerdo a un tipo
data3 %>% glimpse()

#Limpiamos data en general
data_clean <- data3%>%
  unique()

#Separamos data con variables que identifiquen la canción
data_id<-data_clean%>%
  select(track_name,duration_ms,track_number)%>%
  unique()

#Separamos data con variables que nos entreguen datos de la canción (Todos númericos)
data_num <- data_clean%>%
select(danceability,energy,loudness,mode,speechiness,acousticness,liveness,valence,track_number)%>% 
  unique()

#Creamos el data Tsne para realizar el módelo Dbscan
data_tsne <- data_num %>% 
  unique() %>% 
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()

#Gráficamos la data Tsne para ver como queda
ggplot(data_tsne, aes(V1, V2)) + 
  geom_point(alpha = 0.5)


#Gráficamos Knn para escoger eps, a lo cual notamos que el punto de inflexíon se aproxima en 2
kNNdistplot(data_tsne, k = 4)
abline(h=2, col = "red", lty = 2)
dev.off() 

#Creamos el modelo Dbscan con el parametro escogido
modelo_dbscan <- dbscan(data_tsne, eps = 2, minPts = 9)

#Gráficamos Dbscan
ggplot(data_tsne, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none")

#Vemos cuantos cluster tenemos
modelo_dbscan$cluster %>% max()
max(modelo_dbscan$cluster)

#Creamos data frame de los clusters
clusters= as.data.frame(modelo_dbscan$cluster)

#Unimos la data_id con la recién creada clusters para conocer el nombre, duración 
#Y cluster correspondiente a la base
base <- data_id %>% 
  mutate(cluster = modelo_dbscan$cluster) %>%
  group_by(cluster)

#Le preguntamos al usuario el nombre de la canción que desea utilizar
#Y comprobamos que efectivamente se encuentre en la muestra
#Ejemplo: "Train"
validar=0
i=1
song <- readline(prompt = "Ingrese el nombre de la canción: ")
while(validar!=1){
  for (i in 1:nrow(data3)) {
    if(song==data3[i,27]) {
      validar=1
      #Con la variable indice, guardamos la posición de la canción, lo cual servirá para saber su cluster 
      indice=i
      cat("La canción",song,"está en la muestra de la base de datos")
      break
    }
  }
  if(validar==0){
    song <- readline(prompt = "Por favor ingrese una canción que esté en la base: ")
  }
}

#Con la variable cltr guardamos el cluster de la canción ingresada
tiempo=0
j=1
cltr=clusters[indice,1]

#Creamos dataframe con las canciones
lista=data.frame(Cancion=character(),Duracion_Acumulado=numeric())
#Notamos que 10800000 milisegundos son 3 horas, por lo que el while se detendrá
#Una vez haya superado ese tiempo
#Pd: Si la muestra tomada obtiene clusters que no logren satisfacer todo el tiempo,
#Las canciones se comenzarán a repetir hasta cumplir las 3 horas, siendo todas ellas,
#Del mismo cluster
while(tiempo<=10800000){
  for (j in 1:nrow(base)) {
    if(cltr==base[j,4]) {
      if (tiempo >=10800000){
        break
      }
      tiempo=tiempo+base[j,2]
      cancion=base[j,1]
      vector=c(cancion,tiempo)
      lista=rbind(lista,vector)
    }
  }
}
view(lista)



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


