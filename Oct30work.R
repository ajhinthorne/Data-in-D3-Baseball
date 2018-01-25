library(MASS)
library(dplyr)
library(ggplot2)

Lefties<-as.data.frame(spraycharts%>%filter(bats=="L")%>%dplyr::select(x,y,batter.name, batter.id, type,Description)%>%filter(type=="H")%>%mutate(y2=-y+250))
Righties<-as.data.frame(spraycharts%>%filter(bats=="R")%>%dplyr::select(x,y,batter.name,batter.id, type,Description)%>%filter(type=="H")%>%mutate(y2=-y+250))

id.left<-unique(Lefties$batter.id)
id.right<-unique(Righties$batter.id)

namesleft<-unique(Lefties$batter.name)
namesright<-unique(Righties$batter.name)

Lefties.xgrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$x
Lefties.ygrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$y
Lefties.zgrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$z
contour(Lefties.xgrid,Lefties.ygrid,Lefties.zgrid)

bw<-40
Righties.xgrid<-kde2d(Righties$x,Righties$y2,h=bw,lims=c(0,250,0,250))$x
Righties.ygrid<-kde2d(Righties$x,Righties$y2,h=bw,lims=c(0,250,0,250))$y
Righties.zgrid<-kde2d(Righties$x,Righties$y2,h=bw,lims=c(0,250,0,250))$z


contour(Righties.xgrid,Righties.ygrid,Righties.zgrid)

library(entropy)

Puig<-Righties%>%filter(batter.name=="Yasiel Puig")
Puig.xgrid<-kde2d(Puig$x,Puig$y2,lims=c(0,250,0,250))$x
Puig.ygrid<-kde2d(Puig$x,Puig$y2,lims=c(0,250,0,250))$y
Puig.zgrid<-kde2d(Puig$x,Puig$y2,lims=c(0,250,0,250))$z
contour(Puig.xgrid,Puig.ygrid,Puig.zgrid)

Rdist<-KL.plugin(Puig.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,Puig.zgrid)
Ldist<-KL.plugin(Puig.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,Puig.zgrid)
ifelse(Rdist<Ldist,"R","L")



  
Handedness(429667)

Ryan<-spraycharts%>%filter(batter.id==429667)%>%filter(type=="H")%>%select(x,y)%>%mutate(y2=-y+250)

Ryan.xgrid<-kde2d(Ryan$x,Ryan$y2,lims=c(0,250,0,250))$x
Ryan.ygrid<-kde2d(Ryan$x,Ryan$y2,lims=c(0,250,0,250))$y
Ryan.zgrid<-kde2d(Ryan$x,Ryan$y2,lims=c(0,250,0,250))$z
  
contour(Ryan.xgrid,Ryan.ygrid,Ryan.zgrid)  

HowarddistR<-KL.plugin(Ryan.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,Ryan.zgrid)
HowarddistL<-KL.plugin(Ryan.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,Ryan.zgrid)
ifelse(HowarddistR<-HowarddistL,"R","L")

#Incorrect classification

Ortiz<-spraycharts%>%filter(batter.id==120074)%>%filter(type=="H")%>%select(x,y)%>%mutate(y2=-y+250)

Ortiz.xgrid<-kde2d(Ortiz$x,Ortiz$y2,lims=c(0,250,0,250))$x
Ortiz.ygrid<-kde2d(Ortiz$x,Ortiz$y2,lims=c(0,250,0,250))$y
Ortiz.zgrid<-kde2d(Ortiz$x,Ortiz$y2,lims=c(0,250,0,250))$z

#bandwidth issue here

contour(Ortiz.xgrid,Ortiz.ygrid,Ortiz.zgrid)  

OrtizdistR<-KL.plugin(Ortiz.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,Ortiz.zgrid)
OrtizdistL<-KL.plugin(Ortiz.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,Ortiz.zgrid)
ifelse(OrtizdistR<OrtizdistL,"R","L")

#Correct Classification

Handedness(120074)



HandL<-function(id){
  batter<-spraycharts%>%filter(batter.id==id)%>%filter(type=="H")%>%select(x,y)%>%mutate(y2=-y+250)
  batter.zgrid<-kde2d(batter$x,batter$y2,lims=c(0,250,0,250))$z
  batter.zgrid<-ifelse(batter.zgrid>0,batter.zgrid,batter.zgrid+1.0e-10000)
  batter.distR<-KL.plugin(batter.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,batter.zgrid)
  batter.distL<-KL.plugin(batter.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,batter.zgrid)
  return(as.numeric(batter.distR>batter.distL))
  #as.vector(ifelse(batter.distR<batter.distL,0,1))
}

#hit threshold

class <- c()
for(i in 1:length(id.left)){
 class[i] <- HandL(id.left[i])
 #print(sum(class))
}

class <- c()
for(i in 1:length(id.right)){
  print(i)
  class[i] <- HandL(id.right[i])
  #print(sum(class))
}

#84% classification

HandR<-function(id){
  batter<-spraycharts%>%filter(batter.id==id)%>%filter(type=="H")%>%select(x,y)%>%mutate(y2=-y+250)
  batter.zgrid<-kde2d(batter$x,batter$y2,h=50,lims=c(0,250,0,250))$z
  batter.zgrid<-ifelse(batter.zgrid>0,batter.zgrid,batter.zgrid+1.0e-10000)
  batter.distR<-KL.plugin(batter.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,batter.zgrid)
  batter.distL<-KL.plugin(batter.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,batter.zgrid)
  as.vector(ifelse(batter.distR<batter.distL,1,0))
}

for(i in 1:length(id.right)){
  class[i]<-as.vector(HandR(id.right[i]))
  print(sum(class))
}

#76% classification

length(id.right)

#vanishing argument in freqs2! #no function to return from
#fixed with the e-100000000
#bandwidth problem

batter<-spraycharts%>%filter(batter.id==120074)%>%filter(type=="H")%>%select(x,y)%>%mutate(y2=-y+250)
batter.zgrid<-kde2d(batter$x,batter$y2,lims=c(0,250,0,250))$z
batter.zgrid<-ifelse(batter.zgrid>0,batter.zgrid,batter.zgrid+1.0e-10000)
batter.distR<-KL.plugin(batter.zgrid,Righties.zgrid)+KL.plugin(Righties.zgrid,batter.zgrid)
batter.distL<-KL.plugin(batter.zgrid,Lefties.zgrid)+KL.plugin(Lefties.zgrid,batter.zgrid)
ifelse(batter.distR>batter.distL,"L","R")

###correct for Ortiz