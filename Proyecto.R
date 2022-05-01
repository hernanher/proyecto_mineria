pacman::p_load(mclust, e1071, cluster, flexclust, factoextra, magrittr,dbscan, tidyverse, Rtsne, dplyr,factoextra)

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
  select(track_name,duration_ms,track_number,artist_name)%>%
  unique()

#Separamos data con variables que nos entreguen datos de la canción (Todos númericos)
data_num <- data_clean%>%
  select(danceability,energy,instrumentalness,tempo,loudness,speechiness,acousticness,liveness,valence,track_number)%>% 
  unique()

#Creamos el data Tsne para realizar el modelo Dbscan y modelo GMM
data_tsne <- data_num %>% 
  unique() %>% 
  Rtsne() %>% 
  .$Y %>% 
  as.data.frame()

#Gráficamos la data Tsne para ver como queda
ggplot(data_tsne, aes(V1, V2)) + 
  geom_point(alpha = 0.5)


#Le preguntamos al usuario el nombre de la canción que desea utilizar
#Y comprobamos que efectivamente se encuentre en la muestra
#Ejemplo: "Train" o "26j26hmfGEyzWo222T8Eqx"
validar=0
i=1
song <- readline(prompt = "Ingrese el nombre o ID de la canción: ")
while(validar!=1){
  for (i in 1:nrow(data3)) {
    if(song==data_clean[i,27] | song==data_clean[i,19]) {
      validar=1
      #Con la variable indice, guardamos la posición de la canción, lo cual servirá para saber su cluster 
      indice=i
      cat("La canción o Id",song,"está en la muestra de la base de datos")
      break
    }
  }
  if(validar==0){
    song <- readline(prompt = "Por favor ingrese una canción que esté en la base: ")
  }
}




#========================================== Función Crear Playlist ==============================================

#Creamos dataframe con las canciones
lista=data.frame(Autor_Grupo=character(), Cancion=character(),Duracion_Acumulado=numeric())
#Notamos que 10800000 milisegundos son 3 horas, por lo que el while se detendrá
#Una vez haya superado ese tiempo
#Pd: Si la muestra tomada obtiene clusters que no logren satisfacer todo el tiempo,
#Las canciones se comenzarán a repetir hasta cumplir las 3 horas, siendo todas ellas,
#Del mismo cluster

playlist <- function(Base,tiempo, j, cltr, lista){
  while(tiempo<=10800000){
    for (j in 1:nrow(Base)) {
      if(cltr==Base[j,5]) {
        if (tiempo >=10800000){
          break
        }
        tiempo=tiempo+Base[j,2]
        cancion=Base[j,1]
        autor=Base[j,4]
        vector=c(autor,cancion,tiempo)
        lista=rbind(lista,vector)
      }
      
    }
  }
  return(lista)
}
#==================================================DB-SCAN========================================================


#Gráficamos Knn para escoger eps, a lo cual notamos que el punto de inflexíon se aproxima en 1,7
kNNdistplot(data_tsne, k = 4)
abline(h=1.7, col = "red", lty = 2)
dev.off() 

#Creamos el modelo Dbscan con el parametro escogido
modelo_dbscan <- dbscan(data_tsne, eps = 1.7, minPts = 7)


#Vemos cuantos cluster tenemos
n_clusters= (modelo_dbscan$cluster %>% max())+1

#Creamos data frame de los clusters
clusters_DbScan= as.data.frame(modelo_dbscan$cluster)

#Unimos la data_id con la recién creada clusters para conocer el nombre, duración 
#Y cluster correspondiente a la base
Base_DbScan <- data_id %>% 
  mutate(clusters_DbScan = modelo_dbscan$cluster) %>%
  group_by(clusters_DbScan)

#Indicamos el Cluster de la canción
tiempo=0
j=1
cltr_DbScan=clusters_DbScan[indice,1]
#Utilizamos función crear playlist para el modelo DbScan

Playlist_DbScan<- playlist(Base_DbScan,tiempo,j,cltr_DbScan,lista)

view(Playlist_DbScan)

#========================================================= GMM =============================================================

# Usamos función vista en clases para graficar cualquier version del modelo
gmm_func <- function(data, k, model, threshold=NULL){
  #Se crea modelo con los parametros dados
  model_gmm <- Mclust(data, G=k, modelNames = model)
  
  # si el parametro es nulo usar clasificacion, de lo contrario usar la maxima membresia siempre que supere el umbral
  if(is.null(threshold)){
    fuzz <- model_gmm$classification
  } else {
    fuzz <- apply(model_gmm$z,1, function(x) ifelse(max(x) > threshold, which(x==max(x)),0))
  }
  
  #Grafica
  ggplot(data[fuzz != 0,]) +
    geom_point(aes(V1,V2, color=factor(fuzz[fuzz != 0]))) +
    geom_point(data=data[fuzz == 0,], aes(V1,V2), alpha = 0.5, size=0.8) +
    ggtitle(paste0(round(nrow(data[fuzz == 0,])/nrow(data)*100,0),"% de los datos son cluster"))
  
}
#Probamos un modelo con los mismos clusters que en dbscan y ya notamos diferencias con clusters más unidos y notorios
gmm_func(data_tsne, n_clusters, "VEE", 0.7)


#Calculamos un modelo con todas las opciones posibles
model_all <- Mclust(data_tsne, G=1:30)

#Visualizamos
plot(model_all, what = "BIC")

#Graficamos el Optimo
ggplot(data_tsne) + 
  aes(x=V1, y=V2, color=factor(model_all$classification)) + 
  geom_point(alpha=0.5) 

#Creamos data frame de los clusters
clusters_GMM= as.data.frame(model_all$classification)

#Unimos la data_id con la recién creada clusters para conocer el nombre, duración, autor y
#Cluster correspondiente a la base
Base_Gmm <- data_id %>% 
  mutate(clusters_GMM = model_all$classification) %>%
  group_by(clusters_GMM)

tiempo=0
j=1
#Indicamos el Cluster de la canción
cltr_Gmm=clusters_GMM[indice,1]

#Utilizamos función crear playlist para el modelo gmm
Playlist_GMM<- playlist(Base_Gmm,tiempo,j,cltr_Gmm,lista)
view(Playlist_GMM)

analisis <- function(datos, modelo, n){
  clusters <- modelo$cluster
  
  # inspeccion visual de matriz de distancias
  
  # calculamos las distancias de los datos
  distancias <- dist(datos) %>% as.matrix()
  
  # generamos indices con la ubicacion de los clusters ordenados
  clusters_i <-  sort(clusters, index.return=TRUE)
  
  #reordeno filas y columnas en base al cluster obtenido
  distancias <- distancias[clusters_i$ix, clusters_i$ix]
  rownames(distancias) <- c(1:nrow(datos))
  colnames(distancias) <- c(1:nrow(datos))
  
  # pero la matriz de distancias es muy grande para graficar
  print(object.size(distancias), units = "Mb")
  
  # la extraemos 1 de cada 10 filas y columnas
  ids <- (1:floor(nrow(distancias)/n))*n
  dist_reducida <- distancias[ids,ids]
  
  # bajo considerablemente el tamaño
  print(object.size(dist_reducida), units = "Mb")
  
  # generamos la imagen de la matriz para la inspececion visual
  image(dist_reducida)
}
analisis(data_tsne, modelo_dbscan, 10)
analisis(data2_tsne, modelo_kmeans, 10)
