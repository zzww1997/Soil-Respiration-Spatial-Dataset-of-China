

####Figure 7. Comparison of Rs between annual and monthly scales.
rm(list=ls())
library(ggExtra)
library(ggpubr)
setwd("C:\\Users\\User\\Desktop\\")
dat<-read.csv("-10-25.csv",encoding="UTF-8") 
colnames(dat)
dat1<-subset(dat,dat$T==1)
dat2<-subset(dat,dat$T==2)
dat3<-subset(dat,dat$T==3)
dat4<-subset(dat,dat$T==4)
dat5<-subset(dat,dat$T==5)
dat6<-subset(dat,dat$T==6)

colnames(dat1)
dat$T=factor(dat$T)
dat$TT=factor(dat$TT)
dat$Scale=factor(dat$Scale)
nrow(dat1)

colsBlues <- display.brewer.pal(7, "Greens")
  
p1<-ggscatter(dat1, x = "MAT", y = "flux",palette = c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale",title="-10¡ÜObserved temperature<0")+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+
labs(x = '')+scale_y_continuous(expand = c(0, 0),limits=c(0,4440))+
scale_x_continuous(expand = c(0, 0),limits=c(-10,15))+
theme(legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -10, xmax =-5, ymin = 0, ymax =4440, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =4440, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax =4440, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =4440, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =4440, alpha = 0.5,fill="#41AB5D")+
theme(panel.grid = element_blank(), panel.background = element_rect( 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.background = element_rect(fill = "transparent", color = NA), 
legend.position=c(0.3,0.8))
P1<-ggMarginal(p1,type = 'histogram', groupColour = TRUE, groupFill = TRUE)
P1  


p2<-ggscatter(dat2, x = "MAT", y = "flux",palette = c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale" ,title="0¡ÜObserved temperature<5")+
ylab("")+
labs(x = '')+labs(x = '')+scale_y_continuous(expand = c(0, 0),limits=c(0,4000))+
scale_x_continuous(expand = c(0, 0),limits=c(-10,20))+
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.position="none")+
theme(legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -10, xmax =-5, ymin = 0, ymax =4000, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =4000, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax =4000, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =4000, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =4000, alpha = 0.5,fill="#41AB5D")+
annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax =4000, alpha = 0.5,fill="#238B45")
P2<-ggMarginal(p2,type = 'histogram', groupColour = TRUE, groupFill = TRUE)
P2

p3<-ggscatter(dat3, x = "MAT", y = "flux",palette = c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale",title="5¡ÜObserved temperature<10" )+
ylab("")+
labs(x = '')+labs(x = '')+labs(x = '')+scale_y_continuous(expand = c(0, 0),limits=c(0,3000))+
scale_x_continuous(expand = c(0, 0),limits=c(-5,20))+
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.position="none")+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =3000, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = 0, xmax =5, ymin = 0, ymax =3000, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =3000, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =3000, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax =3000, alpha = 0.5,fill="#41AB5D")
P3<-ggMarginal(p3,type = 'histogram', groupColour = TRUE, groupFill = TRUE)
P3

p4<-ggscatter(dat4, x = "MAT", y = "flux",palette = c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale",title="10¡ÜObserved temperature<13" )+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+
labs(x = 'Climatic conditions at sites(¡ãC)')+
scale_y_continuous(expand = c(0, 0),limits=c(0,3000))+
scale_x_continuous(expand = c(0, 0),limits=c(-5,20))+
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.position="none")+
theme(legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =3000, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = 0, xmax =5, ymin = 0, ymax =3000, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =3000, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =3000, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax =3000, alpha = 0.5,fill="#41AB5D")+
annotate("rect", xmin = 20, xmax = 25, ymin = 0, ymax =3000, alpha = 0.5,fill="#238B45")
P4<-ggMarginal(p4,type = 'histogram', groupColour = TRUE, groupFill = TRUE)


p5<-ggscatter(dat5, x = "MAT", y = "flux",palette = c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale" ,title="13¡ÜObserved temperature<20")+
ylab("")+scale_y_continuous(expand = c(0, 0),limits=c(0,4200))+
scale_x_continuous(expand = c(0, 0),limits=c(-5,20))+
labs(x = 'Climatic conditions at sites(¡ãC)')+
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.position="none")+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =4200, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = 0, xmax =5, ymin = 0, ymax =4200, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =4200, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =4200, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax =4200, alpha = 0.5,fill="#41AB5D")+
annotate("rect", xmin = 20, xmax = 25, ymin = 0, ymax =4200, alpha = 0.5,fill="#238B45")
P5<-ggMarginal(p5,type = 'histogram', groupColour = TRUE, groupFill = TRUE)

p6<-ggscatter(dat6, x = "MAT", y = "flux",palette =c("#FF0000", "#0000FF"),size=1,
          add = "loess",col="Scale",title="20¡ÜObserved temperature<25")+
ylab("")+scale_y_continuous(expand = c(0, 0),limits=c(0,6000))+
scale_x_continuous(expand = c(0, 0),limits=c(-5,25))+
labs(x = 'Climatic conditions at sites(¡ãC)')+
theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', 
color = 'black'), legend.title = element_blank(), legend.key = element_blank(), 
legend.position="none")+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 10,hjust = 0, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=12),
axis.text.y=element_text(color="black",size=12))+
annotate("rect", xmin = -5, xmax =0, ymin = 0, ymax =6000, alpha = 0.7,fill="#EDF8E9" ) +
annotate("rect", xmin = 0, xmax =5, ymin = 0, ymax =6000, alpha = 0.5,fill="#C7E9C0")+
annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax =6000, alpha = 0.5,fill="#A1D99B")+
annotate("rect", xmin = 10, xmax = 15, ymin = 0, ymax =6000, alpha = 0.5,fill="#74C476")+
annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax =6000, alpha = 0.5,fill="#41AB5D")+
annotate("rect", xmin = 20, xmax = 25, ymin = 0, ymax =6000, alpha = 0.5,fill="#238B45")
P6<-ggMarginal(p6,type = 'histogram', groupColour = TRUE, groupFill = TRUE)

P6
library(cowplot)
P7<-plot_grid(P1,P2,P3,P4,P5,P6,ncol=3,labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),label_size = 12)+
theme(plot.margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "cm"))

P8<-ggplot(dat,aes(x=T,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+
xlab("Observed temperature(¡ãC)")+
    scale_x_discrete(labels=c("-10-0","0-5","5-10","10-13","13-20","20-25"))+
  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(aes(group = Scale),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position=c(0.22,0.8),
panel.background = element_rect(fill = 'transparent'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+                                                                    
theme(axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))


blank_plot <- ggplot() + theme_void()
plot_grid(P7, plot_grid( blank_plot,P8, ncol = 1, rel_heights = c(0.3, 0.7)), 
          ncol = 2, rel_widths = c(2.8, 1))



