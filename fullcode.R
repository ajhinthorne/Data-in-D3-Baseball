###full code

#Defining the Play by Play Locations
library(MASS)
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

seager.ggplot<-spraycharts%>%filter(batter.name=="Kyle Seager")%>%filter(Description!="Home Run")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=0,ymax=250,xmin=0,xmax=250))
seager.ggplot+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()+ggtitle("Kyle Seager 2014 Season")+xlab("x")+ylab("y")

seager.ggplot
seager.contour


annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_jitter()


positions <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "to center field", "down the lf line", "down the rf line")

single.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Single")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
double.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Double")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
triple.scatter<-spraycharts%>%filter(type=="H")%>%filter(Description=="Triple")%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
View(outscatter)

#Densities for Each Hit Type
library(dplyr)
library(ggplot2)
#Types of Hits/Outs
positions <- c("to left field", "up the middle", "through the left side", "to right field", "through the right side", "to left center", "to right center", "down the left field line", "down the right field line", "to third base", "to short stop", "to second base", "to first base", "to center field", "down the lf line", "down the rf line")
hitscatter<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(y>=25)%>%ggplot(aes(x=x,y=-y+250,color=Description,ymin=25,ymax=245))+geom_jitter()
hitscatter


hitscatter2<-hitscatter+geom_point(aes(x=125,y=35),color="black")+geom_abline(slope=-1,intercept=160)+geom_abline(slope=1,intercept=-90)
hitscatter3<-hitscatter2+geom_abline(slope=-1.25,intercept=191.25)+geom_abline(slope=1.25,intercept=-121.25)
hitscatter4<-hitscatter3+geom_abline(slope=-2.25,intercept=316.25)+geom_abline(slope=2.25,intercept=-246.25)+geom_abline(slope=-5.5,intercept=722.5)+geom_abline(slope=5.5,intercept=-652.5)
hitscatter4+geom_point(aes(x=125,y=200),color="black")+geom_point(aes(x=125,y=135),color="black")
hitscatter5<-hitscatter4+geom_abline(slope=-.5,intercept=207.5)+geom_abline(slope=.5,intercept=82.5)
hitscatter5


spraycharts2<-spraycharts%>%dplyr::select("Description","x","y","type")%>%mutate(z=-y+250)
View(spraycharts2)

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
popout.lf<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
popout.rf<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
popout.cf<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
popout.triangle1<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
popout.triangle2<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
popout.cf2<-bind_rows(popout.cf,popout.triangle1,popout.triangle2)
popout.utm<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
popout.lcf<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
popout.rcf<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
popout.lfl<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
popout.rfl<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
popout.ss<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
popout.2b<-spraycharts2%>%filter(Description=="Pop Out")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
popout.3b<-spraycharts2%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
popout.1b<-spraycharts2%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
popout.tls<-bind_rows(popout.3b,popout.ss)
popout.trs<-bind_rows(popout.2b,popout.1b)
popout.c<-spraycharts2%>%filter(Description=="Pop Out" | Description=="Bunt Pop Out")%>%filter(z<=60)

lineout.lf<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
lineout.rf<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
lineout.cf<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
lineout.triangle1<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
lineout.triangle2<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
lineout.cf2<-bind_rows(lineout.cf,lineout.triangle1,lineout.triangle2)
lineout.utm<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
lineout.lcf<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
lineout.rcf<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
lineout.lfl<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
lineout.rfl<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
lineout.ss<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
lineout.2b<-spraycharts2%>%filter(Description=="Lineout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
lineout.3b<-spraycharts2%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
lineout.1b<-spraycharts2%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
lineout.c<-spraycharts2%>%filter(Description=="Lineout" | Description=="Bunt Lineout")%>%filter(z<=60)

groundout.lf<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
groundout.rf<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
groundout.cf<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
groundout.triangle1<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
groundout.triangle2<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
groundout.cf2<-bind_rows(groundout.cf,groundout.triangle1,groundout.triangle2)
groundout.utm<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
groundout.lcf<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
groundout.rcf<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
groundout.lfl<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
groundout.rfl<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
groundout.ss<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
groundout.2b<-spraycharts2%>%filter(Description=="Groundout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
groundout.3b<-spraycharts2%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
groundout.1b<-spraycharts2%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
groundout.c<-spraycharts2%>%filter(Description=="Groundout" | Description=="Bunt Groundout")%>%filter(z<=55)

flyout.lf<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
flyout.rf<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
flyout.cf<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
flyout.triangle1<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
flyout.triangle2<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
flyout.cf2<-bind_rows(flyout.cf,flyout.triangle1,flyout.triangle2)
flyout.utm<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
flyout.lcf<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
flyout.rcf<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
flyout.lfl<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
flyout.rfl<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
flyout.ss<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
flyout.2b<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
flyout.3b<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
flyout.1b<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
flyout.c<-spraycharts2%>%filter(Description=="Flyout")%>%filter(z<=55)

###errors
error.lf<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
error.rf<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)
error.cf<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
error.triangle1<-spraycharts2%>%filter(type=="E")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
error.triangle2<-spraycharts2%>%filter(type=="E")%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z>=(.5*x)+82.5)
error.cf2<-bind_rows(error.cf,error.triangle1,error.triangle2)
error.utm<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(5.5*x)-652.5)%>%filter(z>=(-5.5*x)+722.5)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(.5*x)+82.5)
error.lcf<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z>=(.5*x)+82.5)
error.rcf<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z>=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
error.lfl<-spraycharts2%>%filter(type=="E")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(-1.25*x)+191.25)
error.rfl<-spraycharts2%>%filter(type=="E")%>%filter(z>=25)%>%filter(x>=5)%>%filter(z<=(1.25*x)-121.25)
error.ss<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z<=(-5.5*x)+722.5)%>%filter(z>=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
error.2b<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)%>%filter(z<=(5.5*x)-652.5)
error.3b<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
error.1b<-spraycharts2%>%filter(type=="E")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
error.c<-spraycharts2%>%filter(type=="E")%>%filter(z<=55)
error.leftfielder<-rbind(error.lf,error.lfl)
error.centerfielder<-rbind(error.cf,error.lcf,error.rcf)
error.rightfielder<-rbind(error.rf,error.rfl)

View(spraycharts2)

spraycharts2%>%filter(Description=="Bunt Lineout")%>%ggplot(aes(x=x,y=z,ymax=250,ymin=0,xmin=0,xmax=250))+geom_jitter()
rbind.Adam<-rbind(groundout.utm,groundout.c,groundout.1b,groundout.2b,groundout.3b,groundout.ss,popout.2b,flyout.cf,flyout.rf,lineout.2b,lineout.ss,lineout.cf,lineout.rf,error.2b,error.ss,single.lf,single.rf,single.lcf,single.3b,single.2b,single.cf,single.rfl,double.lf,double.lfl,double.rfl)
weights.Adam<-c(rep(3/76,length(groundout.utm$Description)),rep(1/76,length(groundout.c$Description)),rep(1/76,length(groundout.1b$Description)),rep(6/76,length(groundout.2b$Description)),rep(6/76,length(groundout.3b$Description)),rep(11/76,length(groundout.ss$Description)),rep(3/76,length(popout.2b$Description)),rep(3/76,length(flyout.cf$Description)),
                rep(7/76,length(flyout.rf$Description)),rep(2/76,length(lineout.2b$Description)),rep(1/76,length(lineout.ss$Description)),rep(1/76,length(lineout.cf$Description)),rep(1/76,length(lineout.rf$Description)),rep(2/76,length(error.2b$Description)),rep(1/76,length(error.ss$Description)),rep(5/76,length(single.lf$Description)),rep(9/76,length(single.rf$Description)),
                rep(1/76,length(single.lcf$Description)),rep(1/76,length(single.3b$Description)),rep(2/76,length(single.2b$Description)),rep(4/76,length(single.cf$Description)),rep(1/76,length(single.rfl$Description)),rep(1/76,length(double.lf$Description)),rep(1/76,length(double.lfl$Description)),rep(1/76,length(double.rfl$Description)))
rbind.Adam%>%ggplot(aes(x=x,y=z,weight=weights.Adam))+xlim(0,250)+ylim(0,250)+stat_density2d()
length(rbind.Adam$Description)
densitymat.Adam<-rbind.Adam%>%dplyr::select(x,z)%>%mutate(x1=as.numeric(x))%>%mutate(x2=as.numeric(z))
x.Adam<-c(densitymat.Adam$x1)
y.Adam<-c(densitymat.Adam$x2)
density.Adam<-ggtern::kde2d.weighted(x=x.Adam,y=y.Adam,w=weights.Adam)
contour(density.Adam$x,density.Adam$y,density.Adam$z)

###KLstuff
#Creating KL Distances

library(ggplot2)
library(dplyr)
View(diamonds)
ggplot(diamonds, aes(carat))+geom_density()

ggplot(diamonds, aes(carat))+geom_density(adjust=1/5)

ggplot(diamonds, aes(carat))+geom_density(adjust=5)

bw.nrd(diamonds$carat)

#check bandwidth of lefties

Left.den<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%ggplot(aes(x=x,y=-y+250,ymin=25,ymax=250))+geom_density_2d()
Left.den
Left.bwcheck<-spraycharts%>%filter(type=="H")%>%filter(Description!="Home Run")%>%filter(bats=="L")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
bw.nrd(Left.bwcheck$x)                                                                                         
bw.nrd(Left.bwcheck$y2)
bw.nrd0(Left.bwcheck$x)
bw.nrd0(Left.bwcheck$y2)
#Silverman's rueof thumb, more common variation

bandwidth.nrd(Left.bwcheck$x)
bandwidth.nrd(Left.bwcheck$y2)
#Via 'Normal Reference Distribution', calculated by taking the IQR of the vector, dividing by 1.34, then 4*1.06*min(sqrt(var(x)),h)*length(x^-1/5)
###calculate the bandwidth using this metric and then plot using this (test with Puig?)


###Choose which ones to actually use

names<-unique(spraycharts$batter.name)
topten<-names[1:10]
topfive<-names[1:5]


player.vector1<-spraycharts%>%filter(batter.name=="Yasiel Puig")%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
player.den1<-kde2d(player.vector1$x,player.vector1$y2)

player.vector2<-spraycharts%>%filter(batter.name=="Grady Sizemore")%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
player.den2<-kde2d(player.vector2$x,player.vector2$y2)

#Be sure to change matrix size
data.den<-matrix(data=NA,nrow=5,ncol=5)
#matches i and j down here
for(i in 1:5){
  for(j in 1:5){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==topfive[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==topfive[j])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2)
    
    KL.dist<-KL.plugin(player.den.a$z,player.den.b$z)+KL.plugin(player.den.b$z,player.den.a$z)
    
    data.den[i,j]<-KL.dist
    
  }
}

data.den
plot(x=seq(1,5,1),y=data.den[,2],ylim=c(0,5))

dist.Adam<-c()
for(i in 1:100){
  player.vector.a<-c()
  player.den.a<-c()
  player.vector.a<-spraycharts%>%filter(batter.name==tophundred[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
  player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
  
  KL.dist<-KL.plugin(player.den.a$z,density.Adam$z)+KL.plugin(density.Adam$z,player.den.a$z)
  dist.Adam[i]<-KL.dist
}
plot(seq(1,100,1),dist.Adam)
data.den2<-data.den2[1:100,1:100]

dist.Adam0<-c(dist.Adam,0)
data.den2<-cbind(data.den2,dist.Adam)
data.den2<-rbind(data.den2,dist.Adam0)
data.den

set.seed(47)
kmeans.topfive<-kmeans(data.den,2)
kmeans.topfive

kmeans.tophundred<-kmeans(data.den2,8)
kmeans.tophundred
###checking how the kmeans function did


spraycharts%>%filter(batter.name==topfive[1])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[3])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[5])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

spraycharts%>%filter(batter.name==topfive[2])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[4])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

#kmeans did ok, two batters clearly hit balls to left field, while the others hit more to right field

#now let's attempt to minimize the sum of least squares
bad <- c()
names<-c(unique(spraycharts$batter.name))
for (i in 1:length(names)) {
  if (sum(spraycharts$batter.name==names[i]) < 20)
    bad <- c(bad, i)
}
names <- names[-bad]
set.seed(47)
tophundred<-sample(names,100,replace=FALSE)
topfifty<-names[1:50]
library(MASS)
library(entropy)
#Be sure to change matrix size
data.den2<-matrix(data=NA,nrow=length(tophundred),ncol=length(tophundred))
#matches i and j down here
for(i in 1:length(tophundred)){
  for(j in 1:length(tophundred)){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==tophundred[i])%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==tophundred[j])%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2)
    
    KL.dist<-KL.plugin(player.den.a$z,player.den.b$z)+KL.plugin(player.den.b$z,player.den.a$z)
    
    data.den2[i,j]<-KL.dist
    
  }
}
data.den2

set.seed(47)
km.2<-kmeans(data.den2,7)
km.2

k.means<-as.data.frame(cbind(tophundred,km.2$cluster))
type1<-k.means%>%filter(km.2$cluster=="1")
type2<-k.means%>%filter(km.2$cluster=="2")
type3<-k.means%>%filter(km.2$cluster=="3")
type4<-k.means%>%filter(km.2$cluster=="4")
type5<-k.means%>%filter(km.2$cluster=="5")
type7<-k.means%>%filter(km.2$cluster=="7")

type1.spray<-spraycharts%>%filter(batter.name %in% type1$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
type2.spray<-spraycharts%>%filter(batter.name %in% type2$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
type3.spray<-spraycharts%>%filter(batter.name %in% type3$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
type4.spray<-spraycharts%>%filter(batter.name %in% type4$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
type5.spray<-spraycharts%>%filter(batter.name %in% type5$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
type7.spray<-spraycharts%>%filter(batter.name %in% type7$tophundred)%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)

full.spray<-rbind(type1.spray,type2.spray,type3.spray,type4.spray,type5.spray,type7.spray)

type1.den<-kde2d(type1.spray$x,type1.spray$y2)
type2.den<-kde2d(type2.spray$x,type2.spray$y2)
type3.den<-kde2d(type3.spray$x,type3.spray$y2)
type4.den<-kde2d(type4.spray$x,type4.spray$y2)
type5.den<-kde2d(type5.spray$x,type5.spray$y2)
type7.den<-kde2d(type7.spray$x,type7.spray$y2)

type1.KL<-KL.plugin(density.Adam$z,type1.den$z)+KL.plugin(type1.den$z,density.Adam$z)
type2.KL<-KL.plugin(density.Adam$z,type2.den$z)+KL.plugin(type2.den$z,density.Adam$z)
type3.KL<-KL.plugin(density.Adam$z,type3.den$z)+KL.plugin(type3.den$z,density.Adam$z)
type4.KL<-KL.plugin(density.Adam$z,type4.den$z)+KL.plugin(type4.den$z,density.Adam$z)
type5.KL<-KL.plugin(density.Adam$z,type5.den$z)+KL.plugin(type5.den$z,density.Adam$z)
type7.KL<-KL.plugin(density.Adam$z,type7.den$z)+KL.plugin(type7.den$z,density.Adam$z)

KL.vector<-c(type1.KL,type2.KL,type3.KL,type4.KL,type5.KL,type7.KL)
prob.vector.Adam<-(1/KL.vector)/sum(1/KL.vector)
length.vector<-c(nrow(type1.spray),nrow(type2.spray),nrow(type3.spray),nrow(type4.spray),nrow(type5.spray),nrow(type7.spray))

full.prob.vector<-c(rep(prob.vector.Adam[1],length.vector[1]),rep(prob.vector.Adam[2],length.vector[2]),rep(prob.vector.Adam[3],length.vector[3]),
                    rep(prob.vector.Adam[4],length.vector[4]),rep(prob.vector.Adam[5],length.vector[5]),rep(prob.vector.Adam[6],length.vector[6]))


iteration1.Adam<-kde2d.weighted(full.spray$x,full.spray$y2,w=full.prob.vector)
contour(iteration1.Adam)

type1b.KL<-KL.plugin(iteration1.Adam$z,type1.den$z)+KL.plugin(type1.den$z,iteration1.Adam$z)
type2b.KL<-KL.plugin(iteration1.Adam$z,type2.den$z)+KL.plugin(type2.den$z,iteration1.Adam$z)
type3b.KL<-KL.plugin(iteration1.Adam$z,type3.den$z)+KL.plugin(type3.den$z,iteration1.Adam$z)
type4b.KL<-KL.plugin(iteration1.Adam$z,type4.den$z)+KL.plugin(type4.den$z,iteration1.Adam$z)
type5b.KL<-KL.plugin(iteration1.Adam$z,type5.den$z)+KL.plugin(type5.den$z,iteration1.Adam$z)
type7b.KL<-KL.plugin(iteration1.Adam$z,type7.den$z)+KL.plugin(type7.den$z,iteration1.Adam$z)

KL.vectorb<-c(type1b.KL,type2b.KL,type3b.KL,type4b.KL,type5b.KL,type7b.KL)
prob.vectorb.Adam<-(1/KL.vectorb)/sum(1/KL.vectorb)
length.vector<-c(nrow(type1.spray),nrow(type2.spray),nrow(type3.spray),nrow(type4.spray),nrow(type5.spray),nrow(type7.spray))

full.prob.vector<-c(rep(prob.vectorb.Adam[1],length.vector[1]),rep(prob.vectorb.Adam[2],length.vector[2]),rep(prob.vectorb.Adam[3],length.vector[3]),
                    rep(prob.vectorb.Adam[4],length.vector[4]),rep(prob.vectorb.Adam[5],length.vector[5]),rep(prob.vectorb.Adam[6],length.vector[6]))

iteration2.Adam<-kde2d.weighted(full.spray$x,full.spray$y2,w=full.prob.vector)
contour(iteration2.Adam)

segment.data<-c(50,50)
ggplot(full.spray, aes(x,y2, weights=full.prob.vector,ymin=0,ymax=250,xmin=0,xmax=250))+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_density_2d()

#pool top hundred....

km.2
write your own kmeans 