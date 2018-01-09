library(MASS)
library(seewave)
library(dplyr)
library(ggplot2)
names <- unique(Doubles$batter.name)
#names <- c("Yadier Molina", "Albert Pujols", "Mike Trout","Kolten Wong","Grady Sizemore","Derek Jeter")
bad <- c()
for (i in 1:length(names)) {
  if (sum(Doubles$batter.name==names[i]) < 20)
    bad <- c(bad, i)
}
names <- names[-bad]
singles <- list()
doubles <- list() 
for (i in 1:length(names)) {
  singles[[i]] <- Singles[Singles$batter.name==names[i],1:2]
  doubles[[i]] <- Doubles[Doubles$batter.name==names[i],1:2]
}

sin.den <- list()
dou.den <- list()
for (i in 1:length(names)) {
  sin.den[[i]] <- kde2d(singles[[i]][,1]$x,singles[[i]][,2]$y, lims=c(0,250, 0, 250 ))$z
  dou.den[[i]] <- kde2d(doubles[[i]][,1]$x,doubles[[i]][,2]$y, lims=c(0,250, 0, 250))$z
}
abc <- kde2d(singles[[i]][,1]$x,singles[[i]][,2]$y, lims=c(0,250, 0, 250))
x.grid <- abc$x
y.grid <- abc$y

par(mfrow=c(3,2))
for (i in 1:6)
contour(x.grid, y.grid, sin.den[[i]])
for (i in 1:6)
  contour(x.grid, y.grid, dou.den[[i]])


SandD%>%filter(batter.name=="Freddie Freeman")%>%ggplot(aes(x=x,y=y,color=Description))+geom_jitter()
Freddiesingles<-Singles[Singles$batter.name=="Freddie Freeman",1:2]
Freddieden<-kde2d(Freddiesingles[,1]$x,Freddiesingles[,2]$y,lims=c(0,250,0,250))$z
fredx<-Freddieden$x
fredy<-Freddieden$y
contour(fredx,fredy,Freddieden)


Singles.den.all<-kde2d(Singles$x,Singles$y,lims=c(0,250,0,250))$z
Singles.xgrid<-kde2d(Singles$x,Singles$y,lims=c(0,250,0,250))$x
Singles.ygrid<-kde2d(Singles$x,Singles$y,lims=c(0,250,0,250))$y

contour(Singles.xgrid,Singles.ygrid,Singles.den.all)

Doubles.den.all<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$z
Doubles.xgrid<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$x
Doubles.ygrid<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$y

contour(Doubles.xgrid,Doubles.ygrid,Doubles.den.all)

KLdistSingle<-KL.plugin(Freddieden,Singles.den.all)+KL.plugin(Singles.den.all,Freddieden)
KLdistDouble<-KL.plugin(Freddieden,Doubles.den.all)+KL.plugin(Doubles.den.all,Freddieden)
ifelse(KLdistSingle<KLdistDouble,"Single","Double")
