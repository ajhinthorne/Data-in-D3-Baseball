#Creating KL Distances

library(ggplot2)
library(dplyr)
View(diamonds)
ggplot(diamonds, aes(carat))+geom_density()

ggplot(diamonds, aes(carat))+geom_density(adjust=1/5)

ggplot(diamonds, aes(carat))+geom_density(adjust=5)

bw.nrd(diamonds$carat)

#check bandwidth of lefties

Left.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_density_2d()
Left.den
Left.bwcheck<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
bw.nrd(Left.bwcheck$x)                                                                                         
bw.nrd(Left.bwcheck$y2)
bw.nrd0(Left.bwcheck$x)
bw.nrd0(Left.bwcheck$y2)
#Silverman's rueof thumb, more common variation

bandwidth.nrd(Left.bwcheck$x)
bandwidth.nrd(Left.bwcheck$y2)
#Via 'Normal Reference Distribution', calculated by taking the IQR of the vector, dividing by 1.34, then 4*1.06*min(sqrt(var(x)),h)*length(x^-1/5)
###calculate the bandwidth using this metric and then plot using this (test with Puig?)


###Choose which ones to actually use

names<-unique(spraycharts$batter.name)
topten<-names[1:10]
topfive<-names[1:5]


player.vector1<-spraycharts%>%filter(batter.name=="Yasiel Puig")%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
player.den1<-kde2d(player.vector1$x,player.vector1$y2)

player.vector2<-spraycharts%>%filter(batter.name=="Grady Sizemore")%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
player.den2<-kde2d(player.vector2$x,player.vector2$y2)

#Be sure to change matrix size
data.den<-matrix(data=NA,nrow=5,ncol=5)
#matches i and j down here
for(i in 1:5){
  for(j in 1:5){

  player.vector.a<-c()
  player.den.a<-c()
  player.vector.a<-spraycharts%>%filter(batter.name==topfive[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
  player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
 
  player.vector.b<-c()
  player.den.b<-c()
  player.vector.b<-spraycharts%>%filter(batter.name==topfive[j])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
  player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2)
  
  KL.dist<-KL.plugin(player.den.a$z,player.den.b$z)+KL.plugin(player.den.b$z,player.den.a$z)
  
  data.den[i,j]<-KL.dist
 
  }
}

data.den

set.seed(47)
kmeans.topfive<-kmeans(data.den,2)
kmeans.topfive

###checking how the kmeans function did


spraycharts%>%filter(batter.name==topfive[1])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[3])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[5])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

spraycharts%>%filter(batter.name==topfive[2])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[4])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

#kmeans did ok, two batters clearly hit balls to left field, while the others hit more to right field

#now let's attempt to minimize the sum of least squares
bad <- c()
for (i in 1:length(names)) {
  if (sum(spraycharts$batter.name==names[i]) < 20)
    bad <- c(bad, i)
}
names <- names[-bad]
tophundred<-names[1:100]
topfifty<-names[1:50]

#Be sure to change matrix size
data.den2<-matrix(data=NA,nrow=100,ncol=100)
#matches i and j down here
for(i in 1:100){
  for(j in 1:100){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==tophundred[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==tophundred[j])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2)
    
    KL.dist<-KL.plugin(player.den.a$z,player.den.b$z)+KL.plugin(player.den.b$z,player.den.a$z)
    
    data.den2[i,j]<-KL.dist
    
  }
}
data.den2

set.seed(47)
km.2<-kmeans(data.den2,5,nstart=10)
km.3<-kmeans(data.den2,5)
km.3

#Gap Statistic/# of jumps
library(cstab)
clusters<-cDistance(data.den2,c(2:49),method="kmeans",kmIter=10)
clusters
plot(clusters$Jumps)
