###full code

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
popout.3b<-spraycharts2%>%filter(Description=="Pop Out" | Description="Bunt Pop Out")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
popout.1b<-spraycharts2%>%filter(Description=="Pop Out" | Description="Bunt Pop Out")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
popout.tls<-bind_rows(popout.3b,popout.ss)
popout.trs<-bind_rows(popout.2b,popout.1b)
popout.c<-spraycharts2%>%filter(Description=="Pop Out" | Description="Bunt Pop Out")%>%filter(z<=60)

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
lineout.3b<-spraycharts2%>%filter(Description=="Lineout" | Description="Bunt Lineout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
lineout.1b<-spraycharts2%>%filter(Description=="Lineout" | Description="Bunt Lineout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
lineout.c<-spraycharts2%>%filter(Description=="Lineout" | Description="Bunt Lineout")%>%filter(z<=60)

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
groundout.3b<-spraycharts2%>%filter(Description=="Groundout" | Description="Bunt Groundout")%>%filter(z<=235)%>%filter(x>=5)%>%filter(z>=(-1.25*x)+191.25)%>%filter(z<=(-2.25*x)+316.25)%>%filter(z<=(.5*x)+82.5)
groundout.1b<-spraycharts2%>%filter(Description=="Groundout" | Description="Bunt Groundout")%>%filter(z<=235)%>%filter(z>=(1.25*x)-121.25)%>%filter(z<=(2.25*x)-246.25)%>%filter(z<=(-.5*x)+207.5)
groundout.c<-spraycharts2%>%filter(Description=="Groundout" | Description="Bunt Groundout")%>%filter(z<=55)

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



spraycharts2%>%filter(Description=="Bunt Lineout")%>%ggplot(aes(x=x,y=z,ymax=250,ymin=0,xmin=0,xmax=250))+geom_jitter()



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

set.seed(47)
kmeans.topfive<-kmeans(data.den,2)
kmeans.topfive

###checking how the kmeans function did


spraycharts%>%filter(batter.name==topfive[1])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[3])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[5])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

spraycharts%>%filter(batter.name==topfive[2])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()
spraycharts%>%filter(batter.name==topfive[4])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%ggplot(aes(x,y2,ymin=25,ymax=250,xmin=0,xmax=250))+geom_density2d()

#kmeans did ok, two batters clearly hit balls to left field, while the others hit more to right field

#now let's attempt to minimize the sum of least squares
bad <- c()
for (i in 1:length(names)) {
  if (sum(spraycharts$batter.name==names[i]) < 20)
    bad <- c(bad, i)
}
names <- names[-bad]
tophundred<-names[1:100]
topfifty<-names[1:50]

#Be sure to change matrix size
data.den2<-matrix(data=NA,nrow=100,ncol=100)
#matches i and j down here
for(i in 1:100){
  for(j in 1:100){
    
    player.vector.a<-c()
    player.den.a<-c()
    player.vector.a<-spraycharts%>%filter(batter.name==tophundred[i])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.a<-kde2d(player.vector.a$x,player.vector.a$y2)
    
    player.vector.b<-c()
    player.den.b<-c()
    player.vector.b<-spraycharts%>%filter(batter.name==tophundred[j])%>%filter(type=="H")%>%filter(Description!="Home Run")%>%mutate(y2=-y+250)%>%dplyr::select(x,y2)
    player.den.b<-kde2d(player.vector.b$x,player.vector.b$y2)
    
    KL.dist<-KL.plugin(player.den.a$z,player.den.b$z)+KL.plugin(player.den.b$z,player.den.a$z)
    
    data.den2[i,j]<-KL.dist
    
  }
}
data.den2

set.seed(47)
km.2<-kmeans(data.den2,5,nstart=10)
km.3<-kmeans(data.den2,5)
km.3

#Gap Statistic/# of jumps
library(cstab)
clusters<-cDistance(data.den2,c(2:49),method="kmeans",kmIter=10)
clusters
plot(clusters$Jumps)

###kmeans stuff
