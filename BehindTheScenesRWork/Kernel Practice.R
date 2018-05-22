kpractice<-rnorm(200,0,2)
hist(kpractice)
library(stats)
kp2<-rnorm(200,0,1)
hist(kp2)
smooth<-ksmooth(kpractice,kp2)
summary(smooth)
plot(smooth)


bkde(kpractice)
plot(bkde(kpractice))
summary(bkde(kpractice))
names(bkde(kpractice))

kpractice<-as.matrix(kpractice)

density<-kde(kpractice)
class(density)
names(density)
plot(density$x)
plot(density$estimate)
density$H

library(dplyr)
Puig.mat<-Puig%>%select(x,y2)
Puig.mat
Puig.den<-kde(Puig.mat)
names(Puig.den)
Puig.den$estimate
plot(Puig.den, display="slice")
Puig.den$H

Ortiz.mat<-Ortiz%>%select(x,y2)
Ortiz.den<-kde(Ortiz.mat)
plot(Ortiz.den,display="slice")
Ortiz.den$H

library(ggplot2)

Lefties.s<-as.data.frame(spraycharts%>%filter(bats=="L")%>%dplyr::select(x,y,batter.name, batter.id, type,Description)%>%filter(Description=="Single")%>%mutate(y2=-y+250))
Lefties.d<-as.data.frame(spraycharts%>%filter(bats=="L")%>%dplyr::select(x,y,batter.name, batter.id, type,Description)%>%filter(Description=="Double")%>%mutate(y2=-y+250))
Lefties.t<-as.data.frame(spraycharts%>%filter(bats=="L")%>%dplyr::select(x,y,batter.name, batter.id, type,Description)%>%filter(Description=="Triple")%>%mutate(y2=-y+250))
Lefties<-rbind(Lefties.s,Lefties.d,Lefties.t)

Left.mat<-Lefties%>%dplyr::select(x,y2)
Left.den<-kde(Left.mat)

persp(Left.den$eval.points[[1]],Left.den$eval.points[[2]],Left.den$estimate,phi=60)
pplot(Left.den$estimate,display="slice")


Righties<-as.data.frame(spraycharts%>%filter(bats=="R")%>%dplyr::select(x,y,batter.name,batter.id, type,Description)%>%filter(type=="H")%>%mutate(y2=-y+250))

Left.Outs<-as.data.frame(spraycharts%>%filter(bats=="L")%>%dplyr::select(x,y,batter.name, batter.id, type,Description)%>%filter(type=="O")%>%mutate(y2=-y+250))
LO.mat<-Left.Outs%>%dplyr::select(x,y2)
LO.den<-kde(LO.mat)
persp(LO.den$eval.points[[1]],LO.den$eval.points[[2]],LO.den$estimate,phi=60)
id.left<-unique(Lefties$batter.id)
id.right<-unique(Righties$batter.id)

bad<-list()
for (i in 1:length(id.left)) {
  if (Lefties%>%filter(batter.id==id.left[i])%>%n_distinct()< 20)
    bad <- c(i)
}

Lefties<-Lefties%>%filter(batter.id==!na)



namesleft<-unique(Lefties$batter.name)
namesright<-unique(Righties$batter.name)

Left.mat<-Lefties%>%select(x,y2)
Right.mat<-Righties%>%select(x,y2)

Left.den<-kde(Left.mat)
Right.den<-kde(Right.mat)

KL.Left<-ifelse(Left.den$estimate<=0,Left.den$estimate+.0000000000001,Left.den$estimate)
KL.Right<-ifelse(Right.den$estimate<=0,Right.den$estimate+.000000000000001,Right.den$estimate)

plot(Left.den,display="slice")
Left.den$H
plot(Right.den,display="slice")
Right.den$H

KL.plugin(Puig.den$estimate,KL.Right) #Test

HandL<-function(id){
  batter<-spraycharts%>%filter(batter.id==id)%>%filter(type=="H")%>%dplyr::select(x,y2)
  batter.den<-kde(batter)
  batter.est<-ifelse(batter.den$estimate>0,batter.den$estimate,ifelse(batter.den$estimate<0,-batter.den$estimate,batter.den$estimate+.0000000000001))
  batter.distR<-KL.plugin(batter.est,KL.Right)+KL.plugin(KL.Right,batter.est)
  batter.distL<-KL.plugin(batter.est,KL.Left)+KL.plugin(KL.Left,batter.est)
  return(as.numeric(batter.distR>batter.distL))
}

BW.func<-function(id){
  batter<-spraycharts%>%filter(batter.id==id)%>%filter(type=="H")%>%dplyr::select(x,y2)
  batter.den<-kde(batter)
  return(batter.den$H)
}

HandL(120074)

class <- c()
for(i in 1:length(id.right)){
  class[i] <- HandL(id.right[i])
}

id.right[6]
View(spraycharts%>%filter(batter.id==571771)%>%dplyr::select(batter.name,x,y2,Description))

KL.dist<-function(Density,KL.Den){
  KL.plugin(Density,KL.Den)+KL.plugin(KL.Den,Density)
}

bad<-list()
for (i in 1:length(id.left)) {
  if (sum(id.left==id.left[i]) < 20)
    bad <- c(i)
}

id.left<-id.left[-bad]

bad<-list()
for (i in 1:length(id.right)) {
  if (sum(id.left==id.right[i]) < 20)
    bad <- c(i)
}

id.right<-id.right[-bad]


class <- c()
for(i in 1:length(id.right)){
  class[i] <- HandL(id.right[i])
}
