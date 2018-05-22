library(dplyr)
library(tree)
library(entropy)

Lefties<-as.data.frame(spraycharts%>%filter(bats=="L")%>%select(x,y,batter.name, batter.id, type,Description%>%mutate(y2=-y+250)))
Righties<-as.data.frame(spraycharts%>%filter(bats=="R")%>%select(x,y,batter.name,batter.id, type,Description)%>%mutate(y2=-y+250))

Lefties.xgrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$x
Lefties.ygrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$y
Lefties.zgrid<-kde2d(Lefties$x,Lefties$y2,lims=c(0,250,0,250))$z

Righties.xgrid<-kde2d(Righties$x,Righties$y2,lims=c(0,250,0,250))$x
Righties.ygrid<-kde2d(Righties$x,Righties$y2,lims=c(0,250,0,250))$y
Righties.zgrid<-kde2d(Righties$x,Righties$y2, h=75,lims=c(0,250,0,250))$z

contour(Lefties.xgrid,Lefties.ygrid,Lefties.zgrid)
contour(Righties.xgrid,Righties.ygrid,Righties.zgrid)

library(ggplot2)

Righties%>%ggplot(aes(x,y2,Color=type))+geom_jitter()
Lefties%>%ggplot(aes(x,y2,),Color=type)+geom_jitter()

#too much righty data for a discernable matrix, the matrices aren't very different. Feels like data mining to leave out where the outs are.