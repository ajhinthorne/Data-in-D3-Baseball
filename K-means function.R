#K means function

library(ggplot2)
library(dplyr)
library(entropy)
library(MASS)
names<-c(unique(spraycharts$batter.name))
count<-spraycharts%>%count(batter.name)
good<-count%>%filter(n>100)%>%select(batter.name)
g.spray<-spraycharts%>%filter(batter.name %in% good$batter.name)
g.spray

topfive
topten<-sample(unique(g.spray$batter.name),10)
toptenb<-sample(unique(spraycharts$batter.name),10)
topfiveb<-sample(unique(spraycharts$batter.name),5)
topfiveb
data.den<-matrix(data=NA,nrow=10,ncol=10)
#matches i and j down here
for(i in 1:10){
  for(j in 1:10){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==topten[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==topten[j])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
   
    x<-c(0,250)
    y<-c(0,250)
    
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2,lims=c(range(x),range(y)))
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2,lims=c(range(x),range(y)))
    
    player.den.combine<-(.5*player.den.a$z)+(.5*player.den.b$z)
    
    JS.dist<-sqrt(.5*(KL.plugin(player.den.a$z,player.den.combine)+KL.plugin(player.den.b$z,player.den.combine)))
    
    data.den[i,j]<-JS.dist
    
  }
}

data.den

#tri.inequality check.options

tri<-c()
for(i in 1:25){
  for (j in 1:25){
    for (k in 1:25){
    tri[i]<-c(ifelse(max(data.den)>data.den[[j]]+data.den[[k]],0,1))
    }              
  }
}
sum(tri)
tri


coor.topten<-distance2coordinates(data.den)

km<-kmeans(coor.topten,2)

#Jensen-Shannon divergence (square root of Jensen-Shannon divergence)
#Do events look differenting
#Do a two by two matrix to check (good)

library(philentropy)

distance2coordinates <- function(D) {
  n <- nrow(D)
  maxDist <- which.max(D)
  p1 <- ((maxDist - 1) %% n) + 1
  p2 <- ((maxDist - 1) %/% n) + 1
  x2 <- D[p1, p2]
  r1sq <- D[p1,]^2
  r2sq <- D[p2,]^2
  x <- (r1sq - r2sq + x2^2)/(2*x2)
  y <- sqrt(r1sq - x^2)
  p3 <- which.max(y)
  x3 <- x[p3]
  y3 <- y[p3]
  plus <- abs(D[p3,]^2 - (x3 - x)^2 - (y3 - y)^2)
  minus <- abs(D[p3,]^2 - (x3 - x)^2 - (y3 + y)^2)
  y[minus < plus] <- -y[minus < plus]
  coords <- data.frame(x = x, y = y)
  return(coords)
}

#kmeans cv
tss<-c()
for (i in 1:(nrow(data.points)-1)){
  km<-kmeans(data.points,i)
  tss[i]<-km$betweenss
}
plot(tss)
tss


