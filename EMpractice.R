#EM Practice

x<-da1$da
plot(x$x,x$y)
ret.em<-init.EM(x,nclass=10,method="em.EM")
ret.Rnd<-init.EM(x,nclass=10,method="Rnd.EM",EMC=.EMC.Rnd)
emobj<-simple.init(x,nclass=10)
ret.init<-emcluster(x, emobj, assign.class=TRUE)

par(mfrow=c(1,1))
plotem(ret.em,x)
plotem(ret.Rnd,x)
plotem(ret.init,x)

km.da1<-kmeans(x,10)
km.da1
plot(x$x,x$y,col=km.da1$cluster)

View(da1$da)

emobj<-simple.init(x,nclass=6)
summary(emobj)
length(da1$da$x)

ret<-emcluster(x,emobj,assign.class=TRUE)
summary(ret)

iris2<-iris[,1:4]
emoby<-simple.init(iris2,nclass=3)
summary(emoby)
plotem(emoby,iris2)

iris.em<-emcluster(iris2,emoby,assign.class=TRUE)
plotem(iris.em,iris2)
plotmd(iris2)
summary(iris.em)
iris%>%filter(Species=="setosa")
iris%>%filter(Species=="versicolor")
iris%>%filter(Species=="virginica")

iris4<-iris[,3:4]
emobz<-simple.init(iris4,nclass=3)
summary(emobz)
iris.em2<-emcluster(iris4,emobz,assign.class=TRUE)
plotem(iris.em2,iris4)

emoba<-simple.init(data.den2,nclass=6)
emobb<-simple.init(data.den,nclass=6)

km.2<-kmeans(data.den2,6,nstart=5)
hundcluster<-as.data.frame(cbind(tophundred,as.factor(km.2$cluster)))
View(hundcluster)
hundcluster%>%filter(V2=="2")

km.4<-kmeans(data.den2,8,nstart=10)
km.4

summary(km.4)
dim(km.4$centers)


den.x<-list(c())
for(i in 1:20){
  x<-matrix(rnorm(50*2),ncol=2)
  x[1:25,1]=x[1:25,1]+sample(seq(1,10,1),1)
  x[1:25,2]=x[1:25,2]-sample(seq(1,10,1),1)
  den.x[[i]]<-kde2d(x[,1],x[,2])
}

KL.dist.matrix<-matrix(NA,20,20)
for(i in 1:20){
  for(j in 1:20){
    KL.dist<-KL.plugin(den.x[[i]]$z,den.x[[j]]$z)+KL.plugin(den.x[[j]]$z,den.x[[i]]$z)
    KL.dist.matrix[i,j]<-KL.dist
  }
}

set.seed(24)
km.em<-kmeans(KL.dist.matrix,4)
km.em

y<-matrix(rnorm(50*2),ncol=2)
y[1:25,1]=y[1:25,1]+sample(seq(1,10,1),1)
y[1:25,2]=y[1:25,2]+sample(seq(1,10,1),1)
den.y<-kde2d(y[,1],y[,2])
contour(den.y$x,den.y$y,den.y$z)





contour(den.x[[1]]$x,den.x[[1]]$y,den.x[[1]]$z)
contour(den.x[[2]]$x,den.x[[2]]$y,den.x[[2]]$z)



x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x[,1],x[,2])

emobk<-simple.init(KL.dist.matrix,nclass=8)
