###Practice Matrix
x<-c(runif(10,0,10))
y<-c(runif(10,0,10))
co<-cbind(as.integer(x),as.integer(y))
plot(co[,1],co[,2])
km.co<-kmeans(co,2)
library(ggplot2)
library(dplyr)
as.data.frame(co)%>%ggplot(aes(x=x,y=y,color=km.co$cluster))+geom_jitter()
d.co<-matrix(c(NA),10,10)
for (i in 1:10){
  for (j in 1:10){
    d.co[i,j]<-ifelse(i==j,0,sqrt(((co[i,1]-co[j,1])^2)+((co[i,2]-co[j,2])^2)))
  }
}
#d.co is now the test distance matrix
km.d<-kmeans(d.co,2)
km.co$cluster
km.d$cluster

d.mat<-d.co
numb<-2
cluster<-function(d.mat,numb){}
#starting clusters
  st<-sample(1:nrow(d.mat),numb,replace=FALSE)
  mins<-c()
  for (i in 1:nrow(d.mat)){
        mins[i]<-min(d.mat[st[1:numb],i])
  }
clust<-c()
 for (i in 1:nrow(d.mat)){
   TF<-c()
  TF<-d.co[,i]==mins[i]
  TF<-c(TF[st[1:length(st)]])
  clust[i]<-which(TF=="TRUE")
 }

#finding new centers
labels<-c(1:numb)
  f.st<-as.data.frame(cbind(d.mat,clust))
  cent<-c()
  group<-c()

  for (j in 1:length(labels)){
    group<-clust==labels[j]
    group<-which(group==TRUE)
    cs<-f.st[group,group]
  cent[j]<-mean(as.numeric(cs[labels[j],]))
  }
  

#new clusters
  newmins<-c()
  for (i in 1:nrow(d.co)){
    col<-matrix(c(NA),nrow(d.co),length(cent))
    for (j in 1:length(cent)){
    col[,j]<-abs(d.co[,1]-cent[j])
    }
  for (i in 1:nrow(col)){
    newclust<-which(col[,1])
  }
        newmins<-
  }
  newclust<-c()
  for (i in 1:nrow(d.co)){
    TF<-c()
    TF<-d.co[,i]==newmins[i]
    TF<-c(TF[cent[1:length(cent)]])
    newclust[i]<-which(TF=="TRUE")
  }
return(newclust)
  
}


  cent.b<-f.st%>%filter(clust==2)%>%summarise_all(funs(mean))
  cent.a<-c(cent.a$V1,cent.a$V2)
  cent.b<-c(cent.b$V1,cent.b$V2)
  
  clustb<-c()
  for (i in 1:10){
  clustb[i]<-c(ifelse(dist(rbind(co[i,],cent.a))<dist(rbind(co[i,],cent.b)),1,2))
  }
  f.tw<-as.data.frame(cbind(co,clust))
  cent.a<-f.tw%>%filter(clust==1)%>%summarise_all(funs(mean))
  cent.b<-f.tw%>%filter(clust==2)%>%summarise_all(funs(mean))
  cent.a<-c(cent.a$V1,cent.a$V2)
  cent.b<-c(cent.b$V1,cent.b$V2)
  
  
  
}  
d.co<-dist(co)
clu<-hclust(d.co)
plot(clu)

topten<-c("Jose Altuve","Miguel Cabrera","Ian Kinsler","Robinson Cano","Ben Revere","Adrian Beltre","Hunter Pence","Dee Gordon","Yonder Alonso","Howie Kendrick")
dataz<-matrix(c(NA),nrow=50,ncol=625)
for (i in 1:50){
  x<-c(0,250)
  y<-c(0,250)
  player.vector<-spraycharts%>%filter(batter.name==topfifty[i])%>%filter(Description!="Home Run")%>%filter(type=="H")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
  player.den<-kde2d(player.vector$x,player.vector$y2,lims=c(range(x),range(y)))
  den.vector<-as.vector(player.den$z)
  dataz[i,]<-den.vector
  
}

den.matrix<-matrix(c(den.vector),25,25)
for(i in 1:2){
contour(x=seq(1,25,1),y=seq(1,25,1),z=matrix(c(cent[i,]),25,25))

contour(x=seq(1,25,1),y=seq(1,25,1),z=matrix(c(newcent[i,]),25,25))}

kmeanie <- function(data, k) {
  n <- nrow(data)
  centpick <- sample(n,k)
  cent<-rbind(data[centpick[1:length(centpick)],])
  clust <- c()
  dis <- c()
  
  for (i in 1:n) {
    for (j in 1:k) {
      
      dist.combine<-(.5*(data[i,]+cent[j,]))
      JS.dist<-sqrt(.5*(KL.plugin(data[i,],dist.combine)+KL.plugin(cent[j,],dist.combine)))
      dis[j] <- JS.dist
    }
    clust[i] <- which.min(dis)
  }
  
  newcent<-matrix(c(NA),nrow=k,ncol=625)
  for (i in 1:k){
    for (j in 1:625){
      newcent[i,j]<-mean(data[clust==i,j])
    }}
  
  centdiff<-clust
  
  while(sum(centdiff)>0){
    dis.b<-c()
    clust.b<-c()
  for (i in 1:n) {
    for (j in 1:k) {
      
      dist.combine<-(.5*data[i,])+(.5*newcent[j,])
      JS.dist<-sqrt(.5*(KL.plugin(data[i,],dist.combine)+KL.plugin(newcent[j,],dist.combine)))
      dis.b[j] <- JS.dist
    }
    clust.b[i] <- which.min(dis.b)
  }
  newcent.b<-matrix(c(NA),nrow=k,ncol=625)
  for (i in 1:k){
    for (j in 1:625){
      newcent.b[i,j]<-mean(data[clust.b==i,j])
    }}
  
  centdiff<-clust.b-clust
  clust<-clust.b
  newcent<-newcent.b

  
  }
  return(list(clust,newcent))
  
}  

JS.dist<-function(dataa,datab){

  JS.dist<-c()
  dist.combine<-c()
  
  dist.combine<-(.5*dataa)+(.5*datab)
  JS.dist<-sqrt(.5*(KL.plugin(dataa,dist.combine)+KL.plugin(datab,dist.combine)))
  return(JS.dist)
}

WWS<-c()
BSS<-c()
k<-20
for (m in 1:k){
  tenclust<-list()
  tenclust<-kmeanie(dataz,m)
  
#Between centroid distance
JS.mat<-matrix(c(NA),m,m)
for (i in 1:m){
  for (j in 1:m){
    JS.mat[i,j]<-JS.dist(tenclust[[2]][i,],tenclust[[2]][j,])
}}
BSS[m]<-sum(JS.mat^2)/2

#distance to center:
wWSS<-c()
for (i in 1:m){
  center.mat<-c()
  table<-as.data.frame(table(tenclust[[1]]))
  newdata<-matrix(c(dataz[tenclust[[1]]==i]),table$Freq[i],625)
for (j in 1:nrow(newdata)){
  center.mat[j]<-JS.dist(newdata[j,],tenclust[[2]][i,])}
WSS[i]<-sum(center.mat^2)}

WWS[m]<-sum(WSS)
}

#p-value (permutation test)
pvals<-c()
for (z in 1:100){
finalclust<-list()
finalKL<-c()
KL.perms<-c()

finalclust<-kmeanie(dataz,6)
finalclust[[1]]

finaldist<-matrix(c(NA),6,6)
for (i in 1:6){
  for (j in 1:6){
  finaldist[i,j]<-.5*(KL.plugin(finalclust[[2]][i,],finalclust[[2]][j,])+KL.plugin(finalclust[[2]][j,],finalclust[[2]][i,]))
  }}
finalKL<-sum(finaldist)/2
#randomly assign clusters
KL.perms<-c()
for (m in 1:100){
permclust<-sample(finalclust[[1]],50,replace=FALSE)
table2<-as.data.frame(table(permclust))
centers<-matrix(c(NA),nrow=6,ncol=625)
for (i in 1:6){
  cen<-c()
  for (j in 1:625){
    cen[j]<-mean(dataz[permclust==i,j])
  }
centers[i,]<-cen
}
centerdist<-matrix(c(NA),6,6)
for (i in 1:6){
  for (j in 1:6){
    centerdist[i,j]<-.5*(KL.plugin(centers[i,],centers[j,])+KL.plugin(centers[j,],centers[i,]))
  }}
KL.perms[m]<-sum(centerdist)/2
}
pvals[z]<-sum(ifelse(KL.perms>finalKL,1,0))/100
}

finalclust[[1]]

#doubles to lcf
groupdens<-matrix(c(NA),6,625)
for (i in 1:6){
names<-c()
names<-topfifty[finalclust[[1]]==i]

double.player<-double.rcf%>%filter(batter.name %in% names)
groupdens[i,]<-kde2d(double.player$x,double.player$z,lims = c(0,250,0,250))$z
}

centerdist<-matrix(c(NA),6,6)
for (i in 1:6){
  for (j in 1:6){
    centerdist<-.5*(KL.plugin(groupdens[1,],groupdens[2,])+KL.plugin(groupdens[2,],groupdens[1,]))
  }}                      
