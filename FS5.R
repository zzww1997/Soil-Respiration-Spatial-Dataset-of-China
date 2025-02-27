
####Figure S5. Comparison of Rs across different time scales 
####and climatic conditions at sites with the same observed temperatures.

library(ggpubr)
library(ggplot2)
setwd("C:\\Users\\User\\Desktop\\")
dat<-read.csv("-10-25.csv")
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
my_comparisons <- list( c("1M", "1Y"), c("1Y", "2M"), c("1Y", "3M"), c("1Y", "4M"), c("1Y", "5M") )
p11<-ggplot(dat1,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+
  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+
labs(x = '',title="-10<=Observed temperature<0")+
    scale_x_discrete(labels=c("-10-0.M","-10-0.Y","0-5.M","5-10.M","10-13.M","13-20.M"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(2300,2300,2800,3300,3800),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))


my_comparisons <- list( c("1M", "2Y"), c("2Y", "2M"), c("2Y", "3M"), c("2Y", "4M"), c("2Y", "5M") )
p22<-ggplot(dat2,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+
ylab("")+  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
labs(x = '',title="0<=Observed temperature<5")+
    scale_x_discrete(labels=c("-10-0.M","0-5.M","0-5.Y","5-10.M","10-13.M","13-20.M"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(2000,2500,2000,2500,3000),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))

my_comparisons <- list( c("1M", "3Y"), c("3Y", "2M"), c("3Y", "3M"), c("3Y", "4M"), c("3Y", "5M") )
p33<-ggplot(dat3,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+guides(color=F)+
labs(x = '',title="5<=Observed temperature<10")+
    scale_x_discrete(labels=c("-10-0.M","0-5.M","5-10.M","5-10.Y","10-13.M","13-20.M"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(2000,1500,1000,1100,1600),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))

my_comparisons <- list( c("1M", "4Y"), c("4Y", "2M"), c("4Y", "3M"), c("4Y", "4M"), c("4Y", "5M"), c("4Y", "6M") )
p44<-ggplot(dat4,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
ylab("")+guides(color=F)+
labs(x = '',title="10<=Observed temperature<13")+
    scale_x_discrete(labels=c("-10-0.M","0-5.M","5-10.M","10-13.M","10-13.Y","13-20.M","20-25.M"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(2000,1700,1400,1100,1500,1800),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))

my_comparisons <- list( c("1M", "5Y"), c("5Y", "2M"), c("5Y", "3M"), c("5Y", "4M"), c("5Y", "5M"), c("5Y", "6M") )
p55<-ggplot(dat5,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+guides(color=F)+
labs(x = 'Climatic conditions at sites(°C)',title="13<=Observed temperature<20")+
    scale_x_discrete(labels=c("-10-0.M","0-5.M","5-10.M","10-13.M","13-20.M","13-20.Y","20-25.M"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(3500,3100,2700,2300,1500,2000),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))

my_comparisons <- list( c("1M", "6Y"), c("6Y", "2M"), c("6Y", "3M"), c("6Y", "4M"), c("6Y", "5M"), c("6Y", "6M") )
p66<-ggplot(dat6,aes(x=TT,y=flux,color=Scale))+
  geom_boxplot(aes(fill=Scale),
               alpha=0.7)+
ylab("")+guides(color=F)+  scale_color_manual(values = c("#cfcfe5", "#7262a0"))+
  scale_fill_manual(values =c("#cfcfe5", "#7262a0"))+
labs(x = 'Climatic conditions at sites(°C)',title="20<=Observed temperature<25")+
    scale_x_discrete(labels=c("-10-0.M","0-5.M","5-10.M","10-13.M","13-20.M","20-25.M","20-25.Y"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
stat_compare_means(comparisons = my_comparisons, label.y = c(4000,3600,3200,2800,2400,2000),
                        label="p.signif",
                       show.legend = F)+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
theme(legend.title = element_text(size=11),
legend.text = element_text(size = 11))+  
theme(plot.title = element_text( size = 12,hjust = 0.5, margin = margin(b =0)),
axis.line.x=element_line(linetype=1,color="black",size=0.3),
axis.line.y=element_line(linetype=1,color="black",size=0.3),#坐标轴线
axis.ticks.x=element_line(color="black",size=0.6,lineend = 10),
axis.ticks.y=element_line(color="black",size=0.6,lineend = 10),#刻度线
axis.title.y=element_text( color="black",size=13),
axis.title.x=element_text( color="black",size=12),#坐标轴标题ylab
axis.text.x=element_text(color="black",size=11),
axis.text.y=element_text(color="black",size=11))
library(cowplot)
plot_grid(p11,p22,p33,p44,p55,p66,ncol=2,labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"))

