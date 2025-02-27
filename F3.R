
#####Figure 3. Temperature dependence of Rs in the eight fitted functions.
##### Curve parameters are detailed in Table 1.


rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\")
##############Annual
dat<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
library(modelr)
#########Standard Exponential：
eq11 <- nls(flux~ a*exp(b*MAT),data=dat, start = list(a = 694, b = 0.014), trace=T)
summary(eq11)
rsquare(eq11,dat)
AIC(eq11)
###########Composite Exponential-Quadratic：
eq22 <- nls(flux~ a*exp(b*MAT+c*MAT^2),data=dat, 
 start = list(a = 694, b = -0.014,c=0.0007), trace=T)
summary(eq22)
rsquare(eq22,dat)
AIC(eq22)
#########Arctangent：
eq33 <- nls(flux~ a*atan(b+c*MAT)+d,data=dat, 
 start = list(a = 216, b = -4,c=0.25,d=898), trace=T)
summary(eq33)
rsquare(eq33,dat)
AIC(eq33)
nlc <- nls.control(maxiter = 1000)
###########Rate Decay Exponential：
eq44 <- nls(flux~ a*exp(-b/(MAT-c)),data=dat,  control = nlc,  
 start = list(a = 403,b=13.97,c=40), trace=T)
summary(eq44)
rsquare(eq44,dat)
AIC(eq44)

###############Monthly
dat<-read.csv("Monthly Dataset.csv",encoding="UTF-8") 
#########Standard Exponential：
eq11 <- nls(flux~ a*exp(b*MMT),data=dat, start = list(a = 694, b = 0.014), trace=T)
summary(eq11)
rsquare(eq11,dat)
AIC(eq11)
###########Composite Exponential-Quadratic：
eq22 <- nls(flux~ a*exp(b*MMT+c*MMT^2),data=dat, 
 start = list(a = 694, b = -0.014,c=0.0007), trace=T)
summary(eq22)
rsquare(eq22,dat)
AIC(eq22)
#########Arctangent：
eq33 <- nls(flux~ a*atan(b+c*MMT)+d,data=dat, 
 start = list(a = 216, b = -4,c=0.25,d=898), trace=T)
summary(eq33)
rsquare(eq33,dat)
AIC(eq33)
nlc <- nls.control(maxiter = 1000)
###########Rate Decay Exponential：
eq44 <- nls(flux~ a*exp(-13.72/(MMT-c)),data=dat,  control = nlc,  
 start = list(a = 403,c=40), trace=T)
summary(eq44)
rsquare(eq44,dat)
AIC(eq44)


#######Annual
x1 <- seq(-10,25,by=0.01)
y1 <- 525.41*exp(0.026*x1)
y2 <- 608.26*exp(-0.012*x1+0.0016*x1^2)
y3 <- 177.28*atan(0.43*x1-8.02)+875.90
y4<-394.28*exp(-13.97/(x1-37.03))

#######Monthly
x2 <- seq(-28,32,by=0.01)
y5 <- 366.86*exp(0.045*x2)
y6 <- 317.25*exp(0.066*x2-0.0006*x2^2)
y7 <- 580.95*atan(0.074*x2-1.35)+872.01
y8<-406.72*exp(-13.72/(x2-40.18))

library(ggplot2)
ggplot() +
geom_point(aes(x =x2, y = y5),alpha = 0.5, 
size =0.3, color = "cyan") +
ylab(bquote('Rs ('*g~'C'~ m^-2~yr^-1*')'))+
xlab("Mean Temperature(°C)")+
geom_line(aes(x =x1, y = y1), size = 1, color = "black") +
geom_line(aes(x =x1, y = y2), size = 1, color = "red") +
geom_line(aes(x =x1, y = y3), size = 1, color = "blue") +
geom_line(aes(x =x1, y = y4), size = 1, color = "pink") +
geom_line(aes(x =x2, y = y6), size = 1,  color = "green")+
geom_line(aes(x =x2, y = y7), size = 1,  color = "purple")+
geom_line(aes(x =x2, y = y8), size = 1,  color = "light blue")+
theme_bw()+
theme(panel.grid = element_blank(),  legend.position="none",
panel.background = element_rect(fill = 'transparent', color = 'black'))+
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

legend("topleft", c("Van’t Hoff_Annual","U-shape_Annual","Arctangent_Annual","Lloyd-Taylor_Annual",
"Van’t Hoff_Monthly","Gaussian_Monthly","Arctangent_Monthly","Lloyd-Taylor_Monthly"),
lty=1,cex=0.95,y.intersp=1.3, box.lty = 0, col=c("black","red","blue","pink", "cyan","green","purple"  ,"light blue")) 
     


      