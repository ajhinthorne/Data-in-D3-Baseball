###KL.dist
library(dplyr)
library(entropy)

data1<-rbeta(100,5,2)
data2<-rbeta(100,2,5)
data3<-rbeta(100,5,5)


plot(density(data3),main="Density")
lines(density(data1),col="BLUE")
lines(density(data2),col="RED")

den1<-density(data1)
den2<-density(data2)
den3<-density(data3)


KL.plugin(den1$y,den2$y)+KL.plugin(den2$y,den1$y)
KL.plugin(den3$y,den2$y)+KL.plugin(den2$y,den3$y)
KL.plugin(den3$y,den1$y)+KL.plugin(den1$y,den3$y)

View(as.data.frame(den1$y))

set.seed(47)
beta1<-c(sample(c(2,4),50,replace=TRUE,prob=c(1/2,1/2)))
beta2<-c(sample(c(2,4),50,replace=TRUE,prob=c(1/2,1/2)))

den<-matrix(c(rep(0,25600)),nrow=512,ncol=50)
for(i in 1:50){
  rand<-c()
  deny<-c()
  rand<-rbeta(100,beta1[i],beta2[i])
  deny<-density(rand)
  den[,i]<-deny$y}
  
dim(den)  
  
KL.dist<-function(a,b){KL.plugin(a,b)+KL.plugin(b,a)}
  
KL.dist(den[,1],den[,2])

KL<-as.data.frame(matrix(c(rep(0,2500)),50,50))
for(i in 1:50){
  for(j in 1:50){
    KL[i,j]<-KL.dist(den[,i],den[,j])}}
View(KL)

for(i in 1:50){
  KL[i,i]=100
}

View(KL)
class<-c()
for(i in 1:50){
class<-c(ifelse(beta1[i]==2, "RED", "BLUE"))}  
class
beta1
class<-c(ifelse(beta1[1:50]>2,"RED","BLUE"))
class

for(i in 1:50){
  for(j in 1:50){
  KL[i,j]<-ifelse(KL[i,j]==min(KL[,j]),1,0)}}
View(KL)

