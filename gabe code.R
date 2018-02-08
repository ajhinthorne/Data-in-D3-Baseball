field <- readPNG("baseballfieldbnw.PNG")
innings<-c("1","2","3","4","5","6","7","8","9")
website<-c("http://www.sagehens.com/sports/bsb/2016-17/schedule")

require(XML)
require(RCurl)
require(stringr)
require(png)
find.box.scores <- function(website) {
  url <- getURL(website)
  boxscore <- gregexpr("boxscores", url)[[1]]
  href <- gregexpr("a href=", url)[[1]]
  good.href <- c()
  for (i in 1:length(boxscore))
    good.href[i] <- max((href<boxscore[i])*href) 
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
###code good up to here
  
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
### Sample Usage
####################

website <-"http://www.sagehens.com/sports/bsb/2016-17/schedule"
names <- c("A. Hinthorne","C. Hulse","B. Rogan","D. Skerrett")
playbyplays <- find.box.scores(website)


#look through playbyplays to find all inconsistencies in how names are written and fix them
#if playbyplays is not playbyplay information, alter findboxscores (lines 21-23)

for (i in 1:length(playbyplays)) {
  playbyplays[[i]] <- gsub("Dunn,Connor", "Dunn", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Dunn, Connor", "Dunn", playbyplays[[i]])
  playbyplays[[i]] <- gsub("DUNN, Connor", "Dunn", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Muramaru,Cal", "Muramaru", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Muramaru, Cal", "Muramaru", playbyplays[[i]])
  playbyplays[[i]] <- gsub("MURAMARU, Cal", "Muramaru", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Dejesus,James", "Dejesus", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Dejesus, James", "Dejesus", playbyplays[[i]])
  playbyplays[[i]] <- gsub("DEJESUS, James", "Dejesus", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Croney,Layne", "Croney", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Croney, Layne", "Croney", playbyplays[[i]])
  playbyplays[[i]] <- gsub("CRONEY, Layne", "Croney", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Hughes,Ian", "Hughes", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Hughes, Ian", "Hughes", playbyplays[[i]])
  playbyplays[[i]] <- gsub("HUGHES, Ian", "Hughes", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Moyes,Ryan", "Moyes", playbyplays[[i]])
  playbyplays[[i]] <- gsub("Moyes, Ryan", "Moyes", playbyplays[[i]])
  playbyplays[[i]] <- gsub("MOYES, Ryan", "Moyes", playbyplays[[i]])
}

stats.Adam<-get.table(names[1],playbyplays)
stats.Cade<-get.table(names[2],playbyplays)
stats.Bryce<-get.table(names[3],playbyplays)
stats.Duncan<-get.table(names[4],playbyplays)

for (w in 1:6) {
  name <- names[w]
  
  stats <- get.table(name, playbyplays)
}
  plot(c(-1,1), c(0,2), type='n', xlab="", ylab="", xaxt='n', yaxt='n')
  lim <- par()
 
  
  
  spots <- matrix(c(-.4, .48, -.4, .9, -.2, .69, 0, 1, .4, .48, .4, .9, .2, .69, 0, 1.3, -.6, 1.3, .6, 1.3, -.7, .6, .7, .6, -.35, 1.3, .35, 1.3, 0, .4), nrow=2)
  positions <- c('p', '1b', '2b', '3b', 'ss', 'lf', 'cf', 'rf', "to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "down the lf line", "down the rf line", "c ", "to center field")
  match <- c(15, 5, 7, 1, 3, 9, 8, 10, 9, 4, 2, 10, 6, 13, 14, 11, 12, 1, 3, 7, 5, 11, 12, 15, 8)
  actions.h <- c("singled", "doubled", "tripled", "homered")
  positions.h <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "down the lf line", "down the rf line", "to center field")
  actions.o <- c("grounded out to", "popped up to", "flied out to", "lined out to", "reached first on an error by", "lined into double play", "grounded into double play")
  positions.o <- c('p', 'c ', '1b', '2b', '3b', 'ss', 'lf', 'cf', 'rf')
  a <- seq(-1,1,length.out=30)
  arc <- dnorm(a)-dnorm(1)
  hit.scalers <- 1:4
  for (i in 1:nrow(stats)) {
    scaler <- 1
    ltype <- 1
    lwide <- 1
    added.var <- 0
    if (stats[i,3]>0) {
      
      
      
      
      
      color <- 'red'
      extrabase <- 0
      if (sum(which(stats[i,2]==positions.o)==c(7,8,9))>0 | sum(which(stats[i,2]==positions.h)==c(1,4,6,8,9))>0)
        added.var <- .06
      if (sum(stats[i,1]==actions.h)>0) {
        extrabase <- (which(stats[i,1]==actions.h)-1)/4
        lwide <- which(stats[i,1]==actions.h)
        color <- 'green'
        scaler <- hit.scalers[which(stats[i,1]==actions.h)]
        if (sum(which(stats[i,2]==positions.h)==c(2,3,5,10,11,12,13))==1) {
          scaler <- 0
          ltype <- 2
        }
      }
      if (sum(stats[i,1]==actions.h)==0) {
        if (sum(which(stats[i,1]==actions.o)==c(1,7))==1) {
          scaler <- 0
          ltype <- 2
        }
        if (sum(which(stats[i,1]==actions.o)==c(2,3))==1) {
          scaler <- 5
        }
        if (sum(which(stats[i,1]==actions.o)==c(4,6))==1) {
          lwide <- 2
          scaler <- 0
        }
      }
      for (j in 1:stats[i,3])
        lines(seq(0, spots[,match[which(stats[i,2]==positions)]][1]+rnorm(1,0,.03+added.var/3), length.out=30),seq(0, spots[,match[which(stats[i,2]==positions)]][2]+extrabase+rnorm(1,0,.03+added.var), length.out=30)+arc*scaler, lty=ltype, lwd=lwide, col=color)
      
    }
  }
  text(0,2, name ,col="white")
}











library(png)
library(dplyr)
library(ggplot2)
hitscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
hitscatter
outscatter<-spraycharts%>%filter(type=="O")%>%ggplot(aes(x=x,y=-y+250,color=Description))+geom_jitter()
field<-readPNG('BaseballOverlay.PNG')
field<-grid::rasterGrob(field, interpolate=TRUE)
field$height<-unit(1,"npc")
field$width<-unit(1,"npc")
hitscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()
outscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()

Left.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_density2d()
Left.den
Left.den+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density2d()

Right.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="R")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density_2d()+stat_density_2d(h=5)
Right.den+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density2d()

Right.den

