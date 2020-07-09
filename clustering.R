set.seed(8953)
######################################################

#Dataframe de Dia, HL
clustering1<-data.frame(datos_accidentes$Dia, datos_accidentes$HL)

#Uso de la función kmeans con 9 clusters
kmeans.result1 <- kmeans(clustering1, 9)
kmeans.result1$cluster
t<-table(kmeans.result1$cluster)
d<-as.data.frame(t)
cuenta<-ggplot(d, aes(x=d$Var1,y=d$Freq,fill=""))+ geom_bar(stat="identity") +xlab("Cluster")+ylab("Número de elementos")+scale_fill_discrete(name = "Apariciones")
cuenta
#Representación de los resultados
p1<-plot(clustering1,col=kmeans.result1$cluster, main="Gráfica con k = 9",pch=16)
p1
plotcluster(clustering1, kmeans.result1$cluster)
Clusters = as.factor(kmeans.result1$cluster)
ggplot(clustering1, aes(datos_accidentes$Dia,datos_accidentes$HL, color= Clusters,label=kmeans.result1$cluster)) + geom_label()


######################################################


#Dataframe de Dia,HG (no hay ningun accidente HG)
clustering2<-data.frame(datos_accidentes$Dia, datos_accidentes$HG)
#Uso de la función kmeans con 8 clusters
kmeans.result2 <- kmeans(clustering2, 8)
kmeans.result2$cluster
#Representación de los resultados
p2<-plot(clustering2,col=kmeans.result2$cluster)
Clusters = as.factor(kmeans.result2$cluster)
ggplot(clustering1, aes(datos_accidentes$Dia,datos_accidentes$HG, color= Clusters)) + geom_point()



#####################################################

#Dataframe de Dia, VM
clustering3<-data.frame(datos_accidentes$Dia, datos_accidentes$VM)
#Uso de la función kmeans con 8 clusters
kmeans.result3 <- kmeans(clustering3, 8)
kmeans.result3$cluster
#Representación de los resultados
plot(clustering3,col=kmeans.result3$cluster,main="Gráfica con k = 8",pch=16)
Clusters = as.factor(kmeans.result3$cluster)
ggplot(clustering1, aes(datos_accidentes$Dia,datos_accidentes$VM, color= Clusters)) + geom_point()

###################################################
#Metodo para la eleccion de K
silhouette_score <- function(k){
  km <- kmeans(clustering1, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(clustering1))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

