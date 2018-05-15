###FULL PROJECT CODE###

##DATA COLLECTION##

##D-III data scraping from Gabe Chandler, updated for new website configuration##
#background picture for visualizations
field <- readPNG("baseballfieldbnw.PNG")

#innings of a baseball game
innings<-c("1","2","3","4","5","6","7","8","9")

#pick website to scrape from 
website<-c("http://www.sagehens.com/sports/bsb/2016-17/schedule")

require(XML)
require(RCurl)
require(stringr)
require(png)

#Build functions, find.box.scores pulls information from URL
find.box.scores <- function(website) {
  url <- getURL(website)
  boxscore <- gregexpr("boxscores", url)[[1]]
  href <- gregexpr("href=", url)[[1]]
  good.href <- c()
  for (i in 1:length(boxscore)){
    good.href[i] <- max((href<boxscore[i])*href)}
  quotes <- gregexpr('"', url)[[1]]
  boxes <- c()
  for (i in 1:length(good.href))
    boxes <- rbind(boxes, quotes[min(which(quotes>good.href[i])):(min(which(quotes>good.href[i]))+1)])  
  site <- str_sub(website, 1, gregexpr("edu", website)[[1]][1]+2)
  site2 <- str_sub(website, 1, gregexpr("com", website)[[1]][1]+2)
  if (nchar(site2) < nchar(site) | nchar(site2) > 13)
    site <- site2
  urls.temp <- c()
  for (i in 1:nrow(boxes)) {
    urls.temp [i] <- str_c(str_c(site, str_sub(url, boxes[i,1]+1, boxes[i,2]-1), sep=""), "", sep="")
  }
  
  urls<-c()
  for (i in 1:nrow(boxes)){
    urls[i] <- str_c(urls.temp[i], "?view=plays&inning=1","",sep="")
  }
  
  pbp <- list()
  for (i in 1:length(urls)) {
    pbp[[i]] <- getURL(urls[i])
    trunc <- gregexpr("Top of         1st",pbp[[i]])
    pbp[[i]] <- str_sub(pbp[[i]],trunc[[1]][1],nchar(pbp[[i]]))
  }
  return(pbp)
}


#pulls table of all hits from find.box.scores output
get.table <- function(name, playbyplays) {
  actions <- c("grounded out to", "popped up to", "flied out to", "lined out to", "reached first on an error by", "lined into double play", "grounded into double play")
  positions <- c('p', 'c ', '1b', '2b', '3b', 'ss', 'lf', 'cf', 'rf')
  m <- 1
  outcomes <- c()
  locations <- c()
  occurences <- c()
  for (i in 1:length(actions)) {
    for (j in 1:length(positions)) {
      count <- 0
      for (k in 1:length(playbyplays)) {
        count <- count + length(gregexpr(str_c(name, str_c(actions[i], positions[j], sep=" "), sep=" "), playbyplays[[k]])[[1]])*(gregexpr(str_c(name, str_c(actions[i], positions[j], sep=" "), sep=" "), playbyplays[[k]])[[1]][1]!=-1)
      }
      outcomes[m] <- actions[i]
      locations[m] <- positions[j]
      occurences[m] <- count
      m <- m + 1
    }
  }
  actions <- c("singled", "doubled", "tripled", "homered")
  positions <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "to center field", "down the lf line", "down the rf line")
  m <- 1
  outcomes1 <- c()
  locations1 <- c()
  occurences1 <- c()
  for (i in 1:length(actions)) {
    for (j in 1:length(positions)) {
      count <- 0
      for (k in 1:length(playbyplays)) {
        count <- count + length(gregexpr(str_c(name, str_c(actions[i], positions[j], sep=" "), sep=" "), playbyplays[[k]])[[1]])*(gregexpr(str_c(name, str_c(actions[i], positions[j], sep=" "), sep=" "), playbyplays[[k]])[[1]][1]!=-1)
      }
      outcomes1[m] <- actions[i]#str_c(actions[i], positions[j], sep=" ")
      locations1[m] <- positions[j]
      occurences1[m] <- count
      m <- m + 1
    }
  }
  return(data.frame(outcomes = c(outcomes, outcomes1), locations=c(locations, locations1), occurences=c(occurences, occurences1)))
}

table.output <- function(dataframe) {
  n <- nrow(dataframe)
  m <- sum(dataframe[,3])
  stuff <- matrix(0,nrow=m, ncol=2)
  stuff <- as.data.frame(stuff)
  names(stuff) <- c('out', 'pos')
  increments <- c(0, cumsum(dataframe[which(dataframe[,3]>0),3]))
  j <- 1
  for (i in which(dataframe[,3]>0)) {
    stuff$out[(increments[j]+1):m] <- as.character(dataframe[i,1])
    stuff$pos[c(increments[j]+1):m] <- as.character(dataframe[i,2])
    j <- j + 1
  }
  return(table(stuff))
}

####################
### Sample Usage to get table for player
####################

website <-"http://www.sagehens.com/sports/bsb/2016-17/schedule"
names <- c("T. Nishioka")
playbyplays <- find.box.scores(website)



#if playbyplays is not playbyplay information, alter findboxscores (lines 21-23)
#look through playbyplays to find all inconsistencies in how names are written and fix them
for (i in 1:length(playbyplays)) {
  playbyplays[[i]] <- gsub("Dunn,Connor", "Dunn", playbyplays[[i]])
}

###Data collection from Dan Malter Github page

library(readr)
spraycharts <- read_csv("https://raw.githubusercontent.com/danmalter/Spray-Chart/master/spraycharts.csv")
View(spraycharts)

#The spraychart is built on a 250x250 grid. y values are flipped, with home plate at the bottom. Flip this value (-y+250) for better visualization
library(dplyr)
spraycharts<-spraycharts%>%mutate(z=-y+250)
#Cut down data to what we need, save spraycharts for reference
spraycharts2<-spraycharts%>%dplyr::select("Description","x","z","type","batter.name")

##Defining each hit type
#Densities for Each Hit Type
library(dplyr)
library(ggplot2)
#Types of Hits/Outs
positions <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "to center field", "down the lf line", "down the rf line")
hitscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(y>=25)%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
hitscatter

##hit scatter five breaks up each section of the field through the play by plays
hitscatter2<-hitscatter+geom_point(aes(x=125,y=35),color="black")+geom_abline(slope=-1,intercept=160)+geom_abline(slope=1,intercept=-90)
hitscatter3<-hitscatter2+geom_abline(slope=-1.25,intercept=191.25)+geom_abline(slope=1.25,intercept=-121.25)
hitscatter4<-hitscatter3+geom_abline(slope=-2.25,intercept=316.25)+geom_abline(slope=2.25,intercept=-246.25)+geom_abline(slope=-5.5,intercept=722.5)+geom_abline(slope=5.5,intercept=-652.5)
hitscatter4+geom_point(aes(x=125,y=200),color="black")+geom_point(aes(x=125,y=135),color="black")
hitscatter5<-hitscatter4+geom_abline(slope=-.5,intercept=207.5)+geom_abline(slope=.5,intercept=82.5)
hitscatter5

##hit charts fro all play by plays
single.lf<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
single.rf<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
single.cf<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
single.triangle1<-spraycharts2%>%filter(Description=="Single")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
single.triangle2<-spraycharts2%>%filter(Description=="Single")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
single.cf2<-bind_rows(single.cf,single.triangle1,single.triangle2)
single.utm<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
single.lcf<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
single.rcf<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
single.lfl<-spraycharts2%>%filter(Description=="Single")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
single.rfl<-spraycharts2%>%filter(Description=="Single")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
single.ss<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
single.2b<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
single.3b<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
single.1b<-spraycharts2%>%filter(Description=="Single")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
single.tls<-bind_rows(single.3b,single.ss)
single.trs<-bind_rows(single.2b,single.1b)
singles.of<-bind_rows(single.lfl,single.lf,single.lcf,single.cf2,single.rcf,single.rf,single.rfl)

double.lf<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
double.rf<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
double.cf<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
double.triangle1<-spraycharts2%>%filter(Description=="Double")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
double.triangle2<-spraycharts2%>%filter(Description=="Double")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
double.cf2<-bind_rows(double.cf,double.triangle1,double.triangle2)
double.utm<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
double.lcf<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
double.rcf<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
double.lfl<-spraycharts2%>%filter(Description=="Double")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
double.rfl<-spraycharts2%>%filter(Description=="Double")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
double.ss<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
double.2b<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
double.3b<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
double.1b<-spraycharts2%>%filter(Description=="Double")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
double.tls<-bind_rows(double.3b,double.ss)
double.trs<-bind_rows(double.2b,double.1b)
doubles.of<-bind_rows(double.lf,double.rf,double.cf2,double.lcf,double.rcf,double.lfl,double.rfl)

triple.lf<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
triple.rf<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
triple.cf<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
triple.triangle1<-spraycharts2%>%filter(Description=="Triple")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
triple.triangle2<-spraycharts2%>%filter(Description=="Triple")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
triple.cf2<-bind_rows(triple.cf,triple.triangle1,triple.triangle2)
triple.utm<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
triple.lcf<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
triple.rcf<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
triple.lfl<-spraycharts2%>%filter(Description=="Triple")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
triple.rfl<-spraycharts2%>%filter(Description=="Triple")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
triple.ss<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
triple.2b<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
triple.3b<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
triple.1b<-spraycharts2%>%filter(Description=="Triple")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
triple.tls<-bind_rows(triple.3b,triple.ss)
triple.trs<-bind_rows(triple.2b,triple.1b)
triples.of<-bind_rows(triple.lf,triple.rf,triple.cf2,triple.lcf,triple.rcf,triple.lfl,triple.rfl)






















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

finalclust[[1]]

#p-value (permutation test)
pvals<-c()
finalKL<-c()
KL.perms<-c()

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
  permclust<-sample(finalclust[[1]],100,replace=FALSE)
  table2<-as.data.frame(table(permclust))
  centers<-matrix(c(NA),nrow=6,ncol=625)
  for (i in 1:6){
    cen<-c()
    for (j in 1:625){
      cen[j]<-mean(dataz[permclust==i,j])
    }
    centers[i,]<-cen
  }
  
  centers<-na.omit(centers)
  centerdist<-matrix(c(NA),nrow(centers),nrow(centers))
  for (i in 1:nrow(centers)){
    for (j in 1:nrow(centers)){
      centerdist[i,j]<-.5*(KL.plugin(centers[i,],centers[j,])+KL.plugin(centers[j,],centers[i,]))
    }}
  KL.perms[m]<-sum(centerdist)/2
}
pvals<-sum(ifelse(KL.perms>finalKL,1,0))/100

total.pvals


#doubles to lcf
pvals<-c()
for (z in 1:length(single.des)){
  set.seed(47)
  groupdens<-matrix(c(NA),6,625)
  for (i in 1:6){
    names<-c()
    names<-tophundred[finalclust[[1]]==i]
    
    double.player<-single.des[[z]]%>%filter(batter.name %in% names)
    groupdens[i,]<-kde2d(double.player$x,double.player$z,lims = c(0,250,0,250),h=10)$z
  }
  
  groupdens<-na.omit(groupdens)
  
  for (i in 1:nrow(groupdens)){
    for (j in 1:ncol(groupdens)){
      
      groupdens[i,j]<-ifelse(groupdens[i,j]==0,1e-300,groupdens[i,j])
      
    }}
  
  centerdist<-matrix(c(NA),nrow(groupdens),nrow(groupdens))
  for (i in 1:nrow(groupdens)){
    for (j in 1:nrow(groupdens)){
      centerdist[i,j]<-.5*(KL.plugin(groupdens[i,],groupdens[j,])+KL.plugin(groupdens[j,],groupdens[i,]))
    }}  
  double.KL<-sum(centerdist)/2
  
  <<<<<<< HEAD
  #for positions
  
  pvals<-c()
  for (z in 1:100){
    finalclust<-list()
    finalKL<-c()
    KL.perms<-c()
    
    finalclust<-kmeanie(dataz,6)
    
    groupdens<-matrix(c(NA),6,625)
    for (i in 1:6){
      names<-c()
      names<-topfifty[finalclust[[1]]==i]
      
      double.player<-singles.of%>%filter(batter.name %in% names)
      if (nrow(double.player)==0){
        groupdens[i,]<-c(rep(NA,625))
      } else {
        groupdens[i,]<-kde2d(double.player$x,double.player$z,lims = c(0,250,0,250),h=10)$z
      }
    }
    
    groupdens<-na.omit(groupdens)
    
    
    
    for (i in 1:nrow(groupdens)){
      for (j in 1:ncol(groupdens)){
        
        groupdens[i,j]<-ifelse(groupdens[i,j]==0,1e-300,groupdens[i,j])
        
      }}
    
    
    finaldist<-matrix(c(NA),nrow(groupdens),nrow(groupdens))
    for (i in 1:nrow(groupdens)){
      for (j in 1:nrow(groupdens)){
        finaldist[i,j]<-.5*(KL.plugin(groupdens[i,],groupdens[j,])+KL.plugin(groupdens[j,],groupdens[i,]))
      }}
    finalKL<-sum(finaldist)/2
    =======
      >>>>>>> 30b21e72e22018f966b36c852219998b1ef96d7a
    
    #randomly assign clusters
    KL.perms<-c()
    for (m in 1:500){
      permclust<-c()
      permclust<-sample(finalclust[[1]],100,replace=FALSE)
      table2<-as.data.frame(table(permclust))
      newgroupdens<-matrix(c(NA),6,625)
      for (i in 1:6){
        names<-c()
        names<-tophundred[permclust==i]
        
        <<<<<<< HEAD
        double.player<-singles.of%>%filter(batter.name %in% names)
        =======
          double.player<-single.des[[z]]%>%filter(batter.name %in% names)
        >>>>>>> 30b21e72e22018f966b36c852219998b1ef96d7a
        if (nrow(double.player)==0){
          newgroupdens[i,]<-c(rep(0,625))
        } else {
          newgroupdens[i,]<-kde2d(double.player$x,double.player$z,lims = c(0,250,0,250),h=10)$z
        }
      }
      
      newgroupdens<-na.omit(newgroupdens)
      
      
      
      for (i in 1:nrow(newgroupdens)){
        for (j in 1:ncol(newgroupdens)){
          
          newgroupdens[i,j]<-ifelse(newgroupdens[i,j]==0,1e-300,newgroupdens[i,j])
          
        }}
      
      
      centerdist<-matrix(c(NA),nrow(newgroupdens),nrow(newgroupdens))
      for (i in 1:nrow(newgroupdens)){
        for (j in 1:nrow(newgroupdens)){
          centerdist[i,j]<-.5*(KL.plugin(newgroupdens[i,],newgroupdens[j,])+KL.plugin(newgroupdens[i,],newgroupdens[j,]))
        }}
      
      KL.perms[m]<-sum(centerdist)/2
    }
    pvals[z]<-sum(ifelse(KL.perms>double.KL,1,0))/500
  }
  
  
  
  doubles.p
  triples.p
  singles.p
  #permclust for positions
  single.des<-list(single.lf,single.rf,single.cf2,single.utm,single.lcf,single.rcf,single.ss,single.2b,single.3b,single.1b,single.tls,single.trs)
  double.des<-list(double.lf,double.rf,double.cf2,double.lcf,double.rcf,double.lfl,double.rfl)
  triple.des<-list(triple.rf,triple.cf2,triple.lcf,triple.rcf,triple.rfl)
  descriptions<-c(single.des,double.des,triple.des)
  
  <<<<<<< HEAD
  doubles.of.p
  triples.of.p
  singles.of.p
  =======
    >>>>>>> 30b21e72e22018f966b36c852219998b1ef96d7a