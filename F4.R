####Figure 4. Influence of environmental factors on Rs.
####piecewiseSEM
rm(list=ls())
library(piecewiseSEM)
setwd("C:\\Users\\User\\Desktop\\")
#########Figure 4a
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
Rt<-subset(Rt,Rt$MAT<3.75 & Rt$SOC!=39.4 & Rt$flux!=1812)
nrow(Rt)
colnames(Rt)
mode_new <- psem(
  lm(flux ~ LAI+SWC , data = Rt, na.action = na.omit),
  lm(SOC ~ MAT, data = Rt, na.action = na.omit),
  lm(LAI ~ MAT+SWC, data = Rt, na.action = na.omit),
lm(SWC ~ MAT, data = Rt, na.action = na.omit),
  flux %~~% SOC)
summary(mode_new, .progressBar = F)
plot(mode_new)


######Figure 4c
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8")
Rt<-subset(Rt,Rt$MAT>3.75 )
nrow(Rt)
Rt_clean <- na.omit(Rt[, c("flux", "LAI", "SWC", "MAT", "SOC")])
mode_new <- psem(
  lm(flux ~ LAI + SWC + MAT, data = Rt_clean),
  lm(SOC ~ LAI, data = Rt_clean),
  lm(LAI ~ MAT + SWC, data = Rt_clean),
  lm(SWC ~ MAT, data = Rt_clean),
  flux %~~% SOC)
summary(mode_new, .progressBar = FALSE)
plot(mode_new)

