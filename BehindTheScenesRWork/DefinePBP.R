#Defining the Play by Play Locations

library(png)
library(dplyr)
library(ggplot2)
hitscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
outscatter<-spraycharts%>%filter(type=="O")%>%ggplot(aes(x=x,y=-y+250,color=Description))+geom_jitter()
field<-readPNG('BaseballOverlay.PNG')
field<-grid::rasterGrob(field, interpolate=TRUE)
field$height<-unit(1,"npc")
field$width<-unit(1,"npc")
hitscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()
outscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()

positions <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "to center field", "down the lf line", "down the rf line")

single.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Single")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
double.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Double")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
triple.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Triple")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()

double.scatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()


