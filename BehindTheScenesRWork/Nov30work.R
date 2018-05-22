

library(png)
library(dplyr)
library(ggplot2)
altscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(y>=25)%>%ggplot(aes(x=x,y=y,color=Description,ymin=25,ymax=245))+geom_jitter()
hitscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(y>=25)%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
hitscatter

hitscatter2<-hitscatter+geom_point(aes(x=125,y=35),color="black")+geom_abline(slope=-1,intercept=160)+geom_abline(slope=1,intercept=-90)
hitscatter3<-hitscatter2+geom_abline(slope=-1.25,intercept=191.25)+geom_abline(slope=1.25,intercept=-121.25)
hitscatter4<-hitscatter3+geom_abline(slope=-2.25,intercept=316.25)+geom_abline(slope=2.25,intercept=-246.25)+geom_abline(slope=-5.5,intercept=722.5)+geom_abline(slope=5.5,intercept=-652.5)
hitscatter4+geom_point(aes(x=125,y=200),color="black")+geom_point(aes(x=125,y=135),color="black")
hitscatter5<-hitscatter4+geom_abline(slope=-.5,intercept=207.5)+geom_abline(slope=.5,intercept=82.5)
hitscatter5

outscatter<-spraycharts%>%filter(type=="O")%>%ggplot(aes(x=x,y=-y+250,color=Description))+geom_jitter()
field<-readPNG('BaseballOverlay.PNG')
field<-grid::rasterGrob(field, interpolate=TRUE)
field$height<-unit(1,"npc")
field$width<-unit(1,"npc")
hitscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()
outscatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()

linescatter<-spraycharts%>%filter(Description=="Lineout")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_jitter()
linescatter
linescatter+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()

Dou.den<-spraycharts%>%filter(type=="H")%>%filter(Description=="Double")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_density_2d()
Dou.den+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density2d()


Left.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_density_2d()
Left.den
Left.den+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density2d()

Right.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="R")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density_2d()+stat_density_2d(h=.5)
Right.den+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density2d()

Right.den

Hits<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)
Hits.L<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%mutate(y2=-y+250)
Hits.R<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="R")%>%mutate(y2=-y+250)

x.sd.Hits<-sd(as.double(Hits$x))
x.IQR.Hits<-IQR(as.double(Hits$x))
x.bw.Hits<-((4/(3*length(Hits$x)))^.2)*x.sd.Hits

x.sd.Hits.R<-sd(as.double(Hits.R$x))
x.IQR.Hits.R<-IQR(as.double(Hits.R$x))
x.bw.Hits.R<-((4/(3*length(Hits.R$x)))^.2)*min(c(x.sd.Hits.R,x.IQR.Hits.R))

y.sd.Hits.R<-sd(as.double(Hits.R$y2))
y.IQR.Hits.R<-IQR(as.double(Hits.R$y2))
y.bw.Hits.R<-((4/(3*length(Hits.R$y2)))^.2)*min(c(y.sd.Hits.R,y.IQR.Hits.R))

                                               
x.sd.Hits.L<-sd(as.double(Hits.L$x))
x.IQR.Hits.L<-IQR(as.double(Hits.L$x))
x.bw.Hits.L<-((4/(3*length(Hits.L$x)))^.2)*min(c(x.sd.Hits.L,x.IQR.Hits.L))
x.bw.Hits.L

y.sd.Hits.L<-sd(as.double(Hits.L$y2))
y.IQR.Hits.L<-IQR(as.double(Hits.L$y2))
y.bw.Hits.L<-((4/(3*length(Hits.L$y2)))^.2)*min(c(y.sd.Hits.L,y.IQR.Hits.L))
y.bw.Hits.L


