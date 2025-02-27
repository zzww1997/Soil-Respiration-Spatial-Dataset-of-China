#####Figure 6. Response curves for Rs versus MMT across six distinct sites.
rm(list=ls())
library(ggplot2)
library(ggsci)

library(RColorBrewer)
mydata<-read.csv("D:\\To do list\\111U型曲线.小论文\\组会建议修改\\所有生态系统.FIN\\空间变异数据.中国月.csv",encoding="UTF-8") 
setwd("C:\\Users\\User\\Desktop\\")
library(ggplot2)
mydata<-read.csv("Monthly Dataset.csv",encoding="UTF-8") 
colnames(dat)
mydat1<-subset(mydata,mydata$Lon==105.47 & mydata$Lat==31.27 & mydata$Eco=="Grassland")	
mydat2<-subset(mydata,mydata$Lon==102.6 & mydata$Lat==29.24)
mydat3<-subset(mydata,mydata$Lon==108.76 & mydata$Lat==18.38)
mydat4<-subset(mydata,mydata$Lon==105.3 & mydata$Lat==27.5)
mydat5<-subset(mydata,mydata$Lon==121.75 & mydata$Lat==50.75)
mydat6<-subset(mydata,mydata$Lon==117.25 & mydata$Lat==42.75)

mean(mydat1$MMT)
mean(mydat1$flux)
mean(mydat2$MMT)
mean(mydat2$flux)
mean(mydat3$MMT)
mean(mydat3$flux)
mean(mydat4$MMT)
mean(mydat4$flux)
mean(mydat5$MMT)
mean(mydat5$flux)
mean(mydat6$MMT)
mean(mydat6$flux)

mydat<-rbind(mydat1,mydat2,mydat3,mydat4,mydat5,mydat6)
write.csv(mydat,"new.csv")
#################
mydat<-read.csv("new.csv")
mydat$MT=factor(mydat$MT)
mydat$ZZ=factor(mydat$ZZ)

ggplot(data=mydat,aes(x=MMT,y=flux,color=MT),alpha=I(0.5))+
  geom_point(aes(shape=ZZ),size=3) +
 geom_smooth(method="loess",se = F)+
scale_color_manual(values=c("light blue","purple","red","blue", "cyan","green"), 
name="Mean temperature",labels=c("-4.16°C","3.04°C","7.89°C","12.05°C","16.76°C","24.19°C"))+
scale_shape_manual(values=c(13,16), 
name="Season",labels=c("GS","NGS"))+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+xlab("Monthly mean temperature(°C)")+  
theme_bw()+geom_point(aes(x=7.893327,y=1045.073),size=5,shape=0,
             color="red",alpha=1)+
geom_point(aes(x=16.76249,y=1319.892),size=5,shape=0,
             color="cyan",alpha=1)+
geom_point(aes(x=24.19142,y= 1012.285),size=5,shape=0,
             color="green",alpha=1)+
geom_point(aes(x=12.04916,y=1414.021),size=5,shape=0,
             color="blue",alpha=1)+
geom_point(aes(x=-4.158333,y=486.4842),size=5,shape=0,
             color="light blue",alpha=0.9)+
geom_point(aes(x=3.041667,y=564.655),size=5,shape=0,
             color="purple",alpha=1)+
geom_point(aes(x=7.893327,y=900),size=5,shape=24,
             color="red",alpha=0.9)+
geom_point(aes(x=16.76249,y=1300),size=5,shape=24,
             color="cyan",alpha=0.9)+
geom_point(aes(x=24.19142,y=1050),size=5,shape=24,
             color="green",alpha=0.9)+
geom_point(aes(x=12.04916,y=1100),size=5,shape=24,
             color="blue",alpha=0.9)+
geom_point(aes(x=-4.158333,y=160),size=5,shape=24,
             color="light blue",alpha=0.9)+
geom_point(aes(x=3.041667,y=150),size=5,shape=24,
             color="purple",alpha=0.9)+
theme(panel.grid = element_blank(), legend.position=c(0.2,0.75), panel.background = element_rect(fill = 'transparent', color = 'black'), 
 legend.key = element_blank()) +
guides(color = guide_legend(ncol = 2, 
    byrow = TRUE,
    reverse = T))+guides(shape = guide_legend(
    ncol = 2, byrow = TRUE,
    reverse = T))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+                                                                    
theme(axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),#坐标轴标题ylab
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))




