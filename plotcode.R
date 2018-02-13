#Plots
PP.names<-c("Bryce Rogan","Cade Hulse","Duncan Skerrett","Adam Hinthorne")
x.vals<-c(85,80,95,100)
y.vals<-c(180,175,190,130)
PP.doubleslcf<-as.data.frame(cbind(PP.names,as.numeric(x.vals),as.numeric(y.vals)))
PP.doubleslcf
plot(PP.doubleslcf$v2,PP.doubleslcf$v3,xlim=c(0,250),ylim=c(0,250))
PP.doubleslcf

#field<-readPNG('BaseballOverlay.PNG')
#field<-grid::rasterGrob(field, interpolate=TRUE)
#field$height<-unit(1,"npc")
#field$width<-unit(1,"npc")



#+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))

matrix<-matrix(c(85,80,95,100,190,185,200,140),4,2)
matrix

matrix<-as.data.frame(matrix)
matrix<-cbind(matrix,PP.names)
matrix%>%ggplot(aes(x=V1,y=V2,color=PP.names,xmin=0,xmax=250,ymin=0,ymax=250))+annotation_custom(field, xmin=-35, xmax=280, ymin=35, ymax=235)+theme(panel.ontop=TRUE,panel.background=element_rect(colour=NA,fill="transparent"))+geom_point()+ggtitle("Sagehen Doubles")+xlab("x")+ylab("y")

