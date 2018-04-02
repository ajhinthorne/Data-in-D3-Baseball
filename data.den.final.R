library(ggplot2)
library(dplyr)
library(entropy)
library(MASS)
names<-c(unique(spraycharts$batter.name))
count<-spraycharts%>%count(batter.name)
good<-count%>%filter(n>100)%>%select(batter.name)
g.spray<-spraycharts%>%filter(batter.name %in% good$batter.name)
g.spray

tophundred<-sample(unique(g.spray$batter.name),100)
data.den.100<-matrix(data=NA,nrow=100,ncol=100)
#matches i and j down here
for(i in 1:100){
  for(j in 1:100){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==tophundred[i])%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==tophundred[j])%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    
    x<-c(0,250)
    y<-c(0,250)
    
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2,lims=c(range(x),range(y)))
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2,lims=c(range(x),range(y)))
    
    player.den.combine<-(.5*player.den.a$z)+(.5*player.den.b$z)
    
    JS.dist<-sqrt(.5*(KL.plugin(player.den.a$z,player.den.combine)+KL.plugin(player.den.b$z,player.den.combine)))
    
    data.den.100[i,j]<-JS.dist
    
  }
}

data.den.100

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

coor.100<-distance2coordinates(data.den.100)
coor.100<-cbind(tophundred,coor.100)
coor.1002<-coor.100%>%filter(x>.1)
plot(coor.1002$x,coor.1002$y)

tss<-c()
bss<-c()
wss<-c()
for (i in 1:15){
  kme<-kmeans(coor.1002[,2:3],i)
  tss[i]<-kme$totss
  bss[i]<-kme$betweenss
  wss[i]<-kme$tot.withinss
}

plot(seq(1,15,1),bss)
plot(seq(1,15,1),wss,ylim=c(0,.4))


km7<-kmeans(coor.1002[,2:3],7)
plot(coor.1002$x,coor.1002$y,col=km7$cluster)
km12<-kmeans(coor.1002[,2:3],12)
plot(coor.1002$x,coor.1002$y,col=km12$cluster)
coor.cluster<-cbind(coor.1002,km7$cluster,km12$cluster)
coor.clusternames<-coor.cluster[,1]
spray<-spraycharts%>%filter(batter.name %in% coor.clusternames)%>%mutate(z=-y+250)%>%dplyr::select("batter.name","Description","x","z","type")
coor.cluster<-rename(coor.cluster,batter.name = tophundred)
test<-right_join(spray,coor.cluster,by="batter.name")
test<-rename(test,km7="km7$cluster")
test<-rename(test,km12="km12$cluster")

d2<-test%>%filter(km7==2)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=50,xmax=125,ymin=125,ymax=225))+geom_jitter()+ggtitle("2")
d6<-test%>%filter(km7==6)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=50,xmax=125,ymin=125,ymax=225))+geom_jitter()+ggtitle("6")
d6
d2

bw<-10
d1<-test%>%filter(km7==1)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("1")
d2<-test%>%filter(km7==2)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("2")
d3<-test%>%filter(km7==3)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("3")
d4<-test%>%filter(km7==4)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("4")
d5<-test%>%filter(km7==5)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("5")
d6<-test%>%filter(km7==6)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("6")
d7<-test%>%filter(km7==7)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(h=bw)+ggtitle("7")

d1s<-test%>%filter(km7==1)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d2s<-test%>%filter(km7==2)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d3s<-test%>%filter(km7==3)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d4s<-test%>%filter(km7==4)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d5s<-test%>%filter(km7==5)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d6s<-test%>%filter(km7==6)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
d7s<-test%>%filter(km7==7)%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x.x)+722.5)%>%filter(z>=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)


ggplot()+geom_density2d(aes(x=x.x,y=z,color="d1s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d1s)+geom_density2d(aes(x=x.x,y=z,color="d2s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d2s)+geom_density2d(aes(x=x.x,y=z,color="d3s",h=bw,xmin=0,xmax=125,ymin=50,ymax=245),d3s)+geom_density2d(aes(x=x.x,y=z,color="d4s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d4s)+geom_density2d(aes(x=x.x,y=z,color="d5s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d5s)+geom_density2d(aes(x=x.x,y=z,color="d6s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d6s)+geom_density2d(aes(x=x.x,y=z,color="d7s",xmin=0,xmax=125,ymin=50,ymax=245,h=bw),d7s)
d1d<-kde2d(d1s$x.x,d1s$z)
d1d<-kde2d(d1s$x.x,d1s$z)
d1d<-kde2d(d1s$x.x,d1s$z)
d1d<-kde2d(d1s$x.x,d1s$z)
d1d<-kde2d(d1s$x.x,d1s$z)


multiplot(d1,d2,d3,d4,d5,d6,d7,cols=2)

s1<-test%>%filter(km7==1)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
s2<-test%>%filter(km7==2)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
ggplot()+geom_density2d(aes(x=x.x,y=z,color="s1",xmin=0,xmax=125,ymin=50,ymax=245),s1)
+geom_density2d(aes(x=x.x,y=z,color="s2",xmin=0,xmax=125,ymin=50,ymax=245),s2)
+geom_density2d(aes(x=x.x,y=z,color="s3",xmin=0,xmax=125,ymin=50,ymax=245),s3)
+geom_density2d(aes(x=x.x,y=z,color="s4",xmin=0,xmax=125,ymin=50,ymax=245),s4)
+geom_density2d(aes(x=x.x,y=z,color="s5",xmin=0,xmax=125,ymin=50,ymax=245),s5)
+geom_density2d(aes(x=x.x,y=z,color="s6",xmin=0,xmax=125,ymin=50,ymax=245),s6)
+geom_density2d(aes(x=x.x,y=z,color="s7",xmin=0,xmax=125,ymin=50,ymax=245),s7)



s3<-test%>%filter(km7==3)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
s4<-test%>%filter(km7==4)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
s5<-test%>%filter(km7==5)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
s6<-test%>%filter(km7==6)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)
s7<-test%>%filter(km7==7)%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)

multiplot(s1,s2,s3,s4,s5,s6,s7,cols=2)

test%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x.x>=5)%>%filter(z>=(-1.25*x.x)+191.25)%>%filter(z<=(-2.25*x.x)+316.25)%>%filter(z>=(.5*x.x)+82.5)%>%ggplot(aes(x=x.x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d(aes(col=km7))



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(d1,d2,d3,d4,d5,d6,d7,cols=2)









eventcheck<-coor.cluster%>%filter(km7$cluster==4)
eventchecknames<-eventcheck[,1]
eventchart<-spraycharts%>%filter(batter.name %in% eventchecknames)%>%mutate(z=-y+250)%>%dplyr::select("batter.name","Description","x","z","type")





single.lf<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
single.rf<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
single.cf<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
single.triangle1<-eventchart%>%filter(Description=="Single")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
single.triangle2<-eventchart%>%filter(Description=="Single")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
single.cf2<-bind_rows(single.cf,single.triangle1,single.triangle2)
single.utm<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
single.lcf<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
single.rcf<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
single.lfl<-eventchart%>%filter(Description=="Single")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
single.rfl<-eventchart%>%filter(Description=="Single")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
single.ss<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
single.2b<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
single.3b<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
single.1b<-eventchart%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
single.tls<-bind_rows(single.3b,single.ss)
single.trs<-bind_rows(single.2b,single.1b)

double.lf<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
double.rf<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
double.cf<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
double.triangle1<-eventchart%>%filter(Description=="Double")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
double.triangle2<-eventchart%>%filter(Description=="Double")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
double.cf2<-bind_rows(double.cf,double.triangle1,double.triangle2)
double.utm<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
double.lcf<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
double.rcf<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
double.lfl<-eventchart%>%filter(Description=="Double")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
double.rfl<-eventchart%>%filter(Description=="Double")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
double.ss<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
double.2b<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
double.3b<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
double.1b<-eventchart%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
double.tls<-bind_rows(double.3b,double.ss)
double.trs<-bind_rows(double.2b,double.1b)

triple.lf<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
triple.rf<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
triple.cf<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
triple.triangle1<-eventchart%>%filter(Description=="Triple")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
triple.triangle2<-eventchart%>%filter(Description=="Triple")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
triple.cf2<-bind_rows(triple.cf,triple.triangle1,triple.triangle2)
triple.utm<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
triple.lcf<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
triple.rcf<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
triple.lfl<-eventchart%>%filter(Description=="Triple")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
triple.rfl<-eventchart%>%filter(Description=="Triple")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
triple.ss<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
triple.2b<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
triple.3b<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
triple.1b<-eventchart%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
triple.tls<-bind_rows(triple.3b,triple.ss)
triple.trs<-bind_rows(triple.2b,triple.1b)

#Now the Densities
library(MASS)
single.lf.den<-single.lf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.rf.den<-single.rf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.cf2.den<-single.cf2%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.utm.den<-single.utm%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.lcf.den<-single.lcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.rcf.den<-single.rcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.lfl.den<-single.lfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.rfl.den<-single.rfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.ss.den<-single.ss%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.2b.den<-single.2b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.3b.den<-single.3b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.1b.den<-single.1b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.tls.den<-single.tls%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
single.trs.den<-single.trs%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()

double.lf.den<-double.lf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.rf.den<-double.rf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.cf2.den<-double.cf2%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.utm.den<-double.utm%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.lcf.den<-double.lcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.rcf.den<-double.rcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.lfl.den<-double.lfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.rfl.den<-double.rfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
sindouble.ss.den<-double.ss%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.2b.den<-double.2b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.3b.den<-double.3b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.1b.den<-double.1b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.tls.den<-double.tls%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
double.trs.den<-double.trs%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()

triple.lf.den<-triple.lf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.rf.den<-triple.rf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.cf2.den<-triple.cf2%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.utm.den<-triple.utm%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.lcf.den<-triple.lcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.rcf.den<-triple.rcf%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.lfl.den<-triple.lfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.rfl.den<-triple.rfl%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.ss.den<-triple.ss%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.2b.den<-triple.2b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.3b.den<-triple.3b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.1b.den<-triple.1b%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.tls.den<-triple.tls%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()
triple.trs.den<-triple.trs%>%ggplot(aes(x=x,y=z,xmin=0,xmax=250,ymin=0,ymax=250))+geom_density2d()

###outs
#popout
popout.lf<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
popout.rf<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
popout.cf<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
popout.triangle1<-eventchart%>%filter(Description=="Pop Out")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
popout.triangle2<-eventchart%>%filter(Description=="Pop Out")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
popout.cf2<-bind_rows(popout.cf,popout.triangle1,popout.triangle2)
popout.utm<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
popout.lcf<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
popout.rcf<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
popout.lfl<-eventchart%>%filter(Description=="Pop Out")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
popout.rfl<-eventchart%>%filter(Description=="Pop Out")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
popout.ss<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
popout.2b<-eventchart%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
popout.3b<-eventchart%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
popout.1b<-eventchart%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
popout.tls<-bind_rows(popout.3b,popout.ss)
popout.trs<-bind_rows(popout.2b,popout.1b)
popout.c<-eventchart%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=60)

lineout.lf<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
lineout.rf<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
lineout.cf<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
lineout.triangle1<-eventchart%>%filter(Description=="Lineout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
lineout.triangle2<-eventchart%>%filter(Description=="Lineout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
lineout.cf2<-bind_rows(lineout.cf,lineout.triangle1,lineout.triangle2)
lineout.utm<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
lineout.lcf<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
lineout.rcf<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
lineout.lfl<-eventchart%>%filter(Description=="Lineout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
lineout.rfl<-eventchart%>%filter(Description=="Lineout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
lineout.ss<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
lineout.2b<-eventchart%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
lineout.3b<-eventchart%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
lineout.1b<-eventchart%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
lineout.c<-eventchart%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=60)

groundout.lf<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
groundout.rf<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
groundout.cf<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
groundout.triangle1<-eventchart%>%filter(Description=="Groundout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
groundout.triangle2<-eventchart%>%filter(Description=="Groundout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
groundout.cf2<-bind_rows(groundout.cf,groundout.triangle1,groundout.triangle2)
groundout.utm<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
groundout.lcf<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
groundout.rcf<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
groundout.lfl<-eventchart%>%filter(Description=="Groundout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
groundout.rfl<-eventchart%>%filter(Description=="Groundout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
groundout.ss<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
groundout.2b<-eventchart%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
groundout.3b<-eventchart%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
groundout.1b<-eventchart%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
groundout.c<-eventchart%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=55)

flyout.lf<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
flyout.rf<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
flyout.cf<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
flyout.triangle1<-eventchart%>%filter(Description=="Flyout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
flyout.triangle2<-eventchart%>%filter(Description=="Flyout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
flyout.cf2<-bind_rows(flyout.cf,flyout.triangle1,flyout.triangle2)
flyout.utm<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
flyout.lcf<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
flyout.rcf<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
flyout.lfl<-eventchart%>%filter(Description=="Flyout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
flyout.rfl<-eventchart%>%filter(Description=="Flyout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
flyout.ss<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
flyout.2b<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
flyout.3b<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
flyout.1b<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
flyout.c<-eventchart%>%filter(Description=="Flyout")%>%filter(z<=55)

###errors
error.lf<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
error.rf<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
error.cf<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
error.triangle1<-eventchart%>%filter(type=="E")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
error.triangle2<-eventchart%>%filter(type=="E")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
error.cf2<-bind_rows(error.cf,error.triangle1,error.triangle2)
error.utm<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
error.lcf<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
error.rcf<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
error.lfl<-eventchart%>%filter(type=="E")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
error.rfl<-eventchart%>%filter(type=="E")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
error.ss<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
error.2b<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
error.3b<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
error.1b<-eventchart%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
error.c<-eventchart%>%filter(type=="E")%>%filter(z<=55)
error.leftfielder<-rbind(error.lf,error.lfl)
error.centerfielder<-rbind(error.cf,error.lcf,error.rcf)
error.rightfielder<-rbind(error.rf,error.rfl)

View(single.lcf)

