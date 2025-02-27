
############Figure 2 . The relationship between annual Rs and MAT (a),
##monthly Rs and MMT (b). The black line represents the loess regression fit
##for all scatter points. NA indicates the altitude of the site is not available.

rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\")

library(ggplot2)
dat<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
colnames(dat)
dat1<-subset(dat,dat$MAT!="NaN")
nrow(dat1)
dat1$alt=as.factor(dat1$alt)
dat1$lat=as.factor(dat1$lat)
p3<-
ggplot(dat1, aes(x = MAT, y = flux))+
     geom_point(aes(colour = dat1$lat,shape=dat1$alt),size=1) +
geom_smooth( method = "loess",se=F,color="black",size=0.7)+
  labs(x = 'MAT(°C)')+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+theme_bw()+
theme(panel.grid = element_blank(),  legend.position=c(0.35,0.8),
panel.background = element_rect(fill = 'transparent'))+
annotate("text", x=25, y=3500, label= "(a)",size=5)+
 theme(legend.box = "horizontal")+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+                                                                    
theme(axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),#坐标轴标题ylab
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
scale_shape_manual(values=c(0, 2,3,16,4), name="Altitude(m)",labels=c("<1000","1000-2000","2000-3000",">3000","NA"))+
scale_color_manual(values=rainbow(5),name="Latitude(°N)",labels=c("<20","20-30","30-40","40-50",">50"))



mydata<-read.csv("Monthly Dataset.csv",encoding="UTF-8") 
colnames(mydata)
nrow(mydata)
mean(mydata$flux)
mydata<-subset(mydata,mydata$MMT!="")
mydata$alt=as.factor(mydata$alt)
mydata$lat=as.factor(mydata$lat)

p4<-
ggplot(mydata, aes(x = MMT, y = flux))+
     geom_point(aes(colour = mydata$lat,shape=mydata$alt),size=1) +
geom_smooth( method = "loess",se=F,color="black",size=0.7)+
  labs(x = 'MMT(°C)',y="")+theme_bw()+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent'))+
annotate("text", x=32, y=5270, label= "(b)",size=5)+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+                                                                    
theme(axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),#坐标轴标题ylab
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
scale_shape_manual(values=c(0, 2,3,16,4), name="Altitude(m)",labels=c("<1000","1000-2000","2000-3000",">3000","NA"))+
scale_color_manual(values=rainbow(5),name="Latitude(°N)",labels=c("<20","20-30","30-40","40-50",">50"))

library(cowplot)
plot_grid(p3,p4,ncol = 2,rel_widths=c(1,1))
