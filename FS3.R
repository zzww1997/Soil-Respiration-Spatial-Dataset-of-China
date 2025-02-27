
####Figure S3. The relationship between annual Rs and MAT.
rm(list=ls())
setwd("C:\\Users\\User\\Desktop\\")
dat1<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
dat1<-subset(dat1,dat1$MAT!="NaN")
library(dplyr)

dat1$MAT_rounded <- round(dat1$MAT)
dat_avg <- dat1 %>%
  group_by(MAT_rounded) %>%
  summarize(avg_flux = mean(flux, na.rm = TRUE))

head(dat_avg)
library(ggplot2)

p_avg <- ggplot(dat1, aes(x = MAT_rounded, y = flux)) +
  geom_point(color = "blue", size = 1, alpha = 0.5) +  
  geom_point(data = dat_avg, aes(x = MAT_rounded, y = avg_flux), size = 3, color = "red") +
  geom_smooth(data = dat_avg, aes(x = MAT_rounded, y = avg_flux), method = "loess", se = TRUE, color = "red", size = 0.7) +
  geom_line(data = dat1, aes(x = MAT_rounded, y = flux, group = MAT_rounded), color = "blue", size = 0.7, alpha = 0.5) +
  labs(x = 'MAT(¡ãC)', y = bquote('Rs ('*g~'C'~ m^-2~yr^-1*')')) +  
  theme_bw() +  theme(panel.grid = element_blank(),  legend.position="none",
    axis.line = element_line(linetype = 1, color = "black", size = 0.3),  
    axis.ticks = element_line(color = "black", size = 0.6, lineend = 10),  
    axis.title = element_text(color = "black", size = 13), 
    axis.text = element_text(color = "black", size = 12)  
  ) +
  scale_x_continuous(breaks = seq(min(dat_avg$MAT_rounded), max(dat_avg$MAT_rounded), by = 2)) 
p_avg
