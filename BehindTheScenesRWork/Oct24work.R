library(MASS)
library(dplyr)
library(ggplot2)
Triples<-as.data.frame(spraycharts%>%filter(Description=="Triple")%>%select(x,y,batter.id,batter.name))

namestrip<-unique(Triples$batter.name)
length(namestrip)
Nperplayer<-as.data.frame(Triples%>%group_by(batter.name)%>%summarise(x=n_distinct(x)))
View(Nperplayer)

triples <- list()
bad<-list()
for (i in 1:length(names)) {
  if (sum(Triples$batter.name==names[i]) < 2)
    bad <- c(bad, i)
}
namestrip<-namestrip[-bad]
for (i in 1:length(namestrip)) {
  triples[[i]] <- Triples[Triples$batter.name==namestrip[i],1:2]

}

tri.den <- list()

for (i in 1:length(namestrip)) {
  tri.den[[i]] <- kde2d(triples[[i]][,1],triples[[i]][,2], h=1, lims=c(0,250, 0, 250 ))$z
  
}

#what happens if you have a negative bandwidth?

tripcontour<- kde2d(triples[[i]][,1],triples[[i]][,2], h=1, lims=c(0,250, 0, 250))
x.grida<-tripcontour$x
y.grida<-tripcontour$y


par(mfrow=c(3,2))
for (i in 1:6)
  contour(x.grida, y.grida, tri.den[[i]])


spraycharts%>%filter(batter.name=="Dee Gordon")%>%filter(type=="H")%>%ggplot(aes(x=x,y=-y+250,color=Description))+geom_jitter()

DeeTrips<-Triples[Triples$batter.name=="Dee Gordon",1:2]
DeeDen<-kde2d(DeeTrips[,1],DeeTrips[,2],lims=c(0,250,0,250))$z
Deex<-kde2d(DeeTrips[,1],DeeTrips[,2],lims=c(0,250,0,250))$x
Deey<-kde2d(DeeTrips[,1],DeeTrips[,2],lims=c(0,250,0,250))$y
contour(Deex,Deey,DeeDen)

DeeDoubs<-as.data.frame(Doubles[Doubles$batter.name=="Dee Gordon",1:2])
Deedx<-kde2d(DeeDoubs[,1],DeeDoubs[,2],lims=c(0,250,0,250))$x
Deedy<-kde2d(DeeDoubs[,1],DeeDoubs[,2],lims=c(0,250,0,250))$y
Deedz<-kde2d(DeeDoubs[,1],DeeDoubs[,2],lims=c(0,250,0,250))$z
contour(Deedx,Deedy,Deedz)

Doubles.den.all<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$z
Doubles.xgrid<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$x
Doubles.ygrid<-kde2d(Doubles$x,Doubles$y,lims=c(0,250,0,250))$y

contour(Doubles.xgrid,Doubles.ygrid,Doubles.den.all)

Triples.den.all<-kde2d(Triples$x,Triples$y,lims=c(0,250,0,250))$z
Triples.xgrid<-kde2d(Triples$x,Triples$y,lims=c(0,250,0,250))$x
Triples.ygrid<-kde2d(Triples$x,Triples$y,lims=c(0,250,0,250))$y

contour(Triples.xgrid,Triples.ygrid,Triples.den.all)

library(seewave)
library(entropy)

KLdistDeeTvD<-KL.plugin(DeeDen,Doubles.den.all)+KL.plugin(Doubles.den.all,DeeDen)
KLdistDeeTvT<-KL.plugin(DeeDen,Triples.den.all)+KL.plugin(Triples.den.all,DeeDen)
ifelse(KLdistDeeTvD<KLdistDeeTvT,"Double","Triple")

#Answer: "Triple"

DeedistDoubles<-sum(DeeDen*log(DeeDen/Doubles.den.all))+sum(Doubles.den.all*log(Doubles.den.all/DeeDen))
DeedistTriples<-sum(DeeDen*log(DeeDen/Triples.den.all))+sum(Triples.den.all*log(Triples.den.all/DeeDen))
ifelse(DeedistDoubles<DeedistTriples,"Double","Triple")

#Answer:"Triple"

ifelse(KLdistDeeTvD>DeedistDoubles,"TRUE","FALSE")

#What function for KL distribution do I need to use

#Do we also need to look at where the outs land? Since a defender may be optimally placed? May be out of the scope of the thesis

#Ask question about KL symmetry, why does this give a better record for distance mathematically? Does my example work?

#is what I am doing experimentally sound?

#thomas and cover
