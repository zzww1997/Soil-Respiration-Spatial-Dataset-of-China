

####Figure S1. The relationship between annual soil respiration (Rs) 
####and mean annual temperature (MAT) across different regions.
rm(list=ls())				
library(ggplot2)									
dat<-read.csv("C:\\Users\\User\\Desktop\\Global annual spatial dataset.csv",encoding="UTF-8") 
colnames(dat)
head(dat)
			
ggplot(dat, aes(x = MAT, y = flux))+
     geom_point(aes(colour = dat$Region),size=1.5,alpha = 0.5) +
geom_smooth( method = "loess",se=F,color="black",size=1.2,linetype=2)+
  labs(x = 'MAT(°C)')+
geom_smooth(method="loess", se =F,size=1,data=subset(dat,dat$Region=="China"),color="red")+
geom_smooth(method="loess", se =F,size=1,data=subset(dat,dat$Region=="N-China"),color="blue")+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+theme_bw()+
theme(panel.grid = element_blank(),  legend.position=c(0.15,0.8),
panel.background = element_rect(fill = 'transparent'))+
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
scale_color_manual(values=c("red","blue"),name="Region")


		

							

