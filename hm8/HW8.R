##loading the required library
install.packages("ClusterR")
install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
install.packages("car")
library(ClusterR)
library(ggplot2)
library(NbClust)
library(cluster)
library(dlookr)
library(dbplyr)
library(factoextra)
library(tidyverse)
library(car)
library(useful)

##uploading the data 
US_Superstore_data-> usData

head(usData)

usData<-usData[,c("Sales","Profit")]

  

head(usData)  ##checking the first 6 files of the data
summary(usData)

usDataScaled<-data.frame(scale(usData))

head(usDataScaled)
#kmeans
usDataKM <- kmeans(usDataScaled,10)
usDataKM


fviz_cluster(usDataKM, data = usDataScaled)
usDataKM$centers
usDataKM$size
usDataKM$withinss
usDataKM$tot.withinss

p<-qplot(data=usData, x=Sales, y=Profit, color=factor(usDataKM$cluster))  #plot the 2 variables and the cluster color
g <- guide_legend("Cluster")                  #retitle the legend...
p + guides(color = g, size = g, shape = g)    #retitle the legend...

wssplot <- function(data, nc=18, set.seed=1234){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}

wssplot(usDataScaled)


usDataKM <- kmeans(usDataScaled,10)
usDataKM

##k-Medoids
fviz_nbclust(usDataScaled, pam, method="silhouette")
usDataKMed<-pam(usDataScaled,2,metric = 'euclidean',stand = FALSE)
fviz_cluster(usDataKMed, data = usDataScaled)
usDataKMed

usDataKMed$medoids
usDataKMed$clusinfo
usDataKMed$silinfo
usDataKMed$isolation
usDataKMed$cluster

##hierarchical clustering
dmat <- dist(usDataScaled,method = "euclidean")
dmat
Hcl <- hclust(dmat)
Hcl
plot(Hcl)
Hcl$cluster
Hcl
