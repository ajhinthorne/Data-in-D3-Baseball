#K-Means Practice
install.packages("dplyr")
install.packages("ggplot2")


#IRIS dataset
library(datasets)
library(dplyr)
library(ggplot2)
head(iris)
iris%>%ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+geom_jitter()
#Actual Species
iris%>%ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species))+geom_jitter()

set.seed(47)
irisCluster <- kmeans(iris[,3:4],3, nstart=20)

irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
iris<-cbind(iris,irisCluster$cluster)
iris<-iris%>%mutate(cluster=ifelse(irisCluster$cluster=="1","setosa",ifelse(irisCluster$cluster=="3","versicolor","virginica")))
iris<-iris%>%mutate(different=ifelse(cluster==Species,"correct class","missclassified"))
View(iris)
#Clusters
iris%>%ggplot(aes(Petal.Length,Petal.Width,color=cluster))+geom_jitter()
#Showing Misclassifications
iris%>%ggplot(aes(Petal.Length,Petal.Width, color = different, shape = cluster))+geom_jitter()

#Compstats lab
set.seed(34)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
x<-as.data.frame(x)
x%>%ggplot(aes(x[,1],x[,2]))+geom_jitter()

km.out<-kmeans(x,2,nstart=20)
km.out$cluster
x%>%ggplot(aes(x[,1],x[,2],color=km.out$cluster))+geom_jitter()


set.seed(29)
km.out2<-kmeans(x,3,nstart=20)
km.out2
km.out2$cluster<-as.factor(km.out2$cluster)
x%>%ggplot(aes(x[,1],x[,2],color=km.out2$cluster))+geom_jitter()

#higher dimensional clustering
set.seed(29)
km.hd<-kmeans(iris[,1:4],3,nstart=20)
km.hd
table(km.hd$cluster,iris$Species)

iris%>%ggplot(aes(x=Petal.Length,y=Petal.Width,color=as.factor(km.hd$cluster),shape=Species,size=Sepal.Width))+geom_jitter()

