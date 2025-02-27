####Figure 4. Influence of environmental factors on Rs.
####piecewiseSEM
rm(list=ls())
library(piecewiseSEM)
setwd("C:\\Users\\User\\Desktop\\")
#########Figure 4a
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
Rt<-subset(Rt,Rt$MAT<3.75 & Rt$SOC!=39.4 & Rt$flux!=1812)
model<-psem(
lm(flux ~LAI+SOC+SWC,  na.action = na.omit,data = Rt),
lm(SOC ~ LAI+MAT,  na.action = na.omit,data = Rt),
lm(LAI ~SWC+MAT, na.action = na.omit,data = Rt))
summary(model, .progressBar = F)
plot(model)


######Figure 4c
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8")
Rt<-subset(Rt,Rt$MAT>3.75 )
model<-psem(
lm(flux~LAI+MAT+SOC+SWC,  na.action = na.omit,data = Rt),
lm(SOC ~ MAT+LAI,  na.action = na.omit,data = Rt),
lm(LAI ~MAT+SWC, na.action = na.omit,data = Rt))
summary(model, .progressBar = F)
plot(model)
