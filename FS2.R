####Figure S2. Correlations between Rs and environmental factors.
####Figure S2a
setwd("C:\\Users\\User\\Desktop\\")
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8") 
Rt<-subset(Rt,Rt$MAT<3.75 & Rt$SOC!=39.4 & Rt$flux!=1812)
colnames(Rt)
mtcars <- Rt[,c(8:12)]
nrow(mtcars)
library(corrplot)
mtcars <-na.omit(mtcars)
class(mtcars)
#计算mtcars数据框的相关性系数;
cor<- cor(mtcars)
#自定义渐变颜色；
col2 <- colorRampPalette(c("#77C034","white" ,"#C388FE"),alpha = TRUE)
#显著性计算：
res <- cor.mtest(mtcars, conf.level = .95)
p <- res$p
#饼图添加显著性星号；
corrplot(cor, order = "hclust",col = col2(100),
         method = "pie",
         cl.length=5, type = "upper",diag = T,
         p.mat = p,sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",pch.col="grey20",pch.cex=1.4,
         tl.col="grey20",tl.cex = 1,cl.pos = "r",cl.ratio = 0.2)
corrplot(cor,add = TRUE,method = 'number',
         type = 'lower', col = col2(100),
         order = c('hclust'), diag = T, number.cex = 0.9,
         tl.pos = 'l', tl.col="grey20",cl.pos = 'n',
         p.mat = p,
         insig = "pch",pch.col="grey20",pch.cex=2)
####Figure S2b
Rt<-read.csv("Annual Dataset.csv",encoding="UTF-8")
Rt<-subset(Rt,Rt$MAT>3.75 )
mtcars <- Rt[,c(8:12)]
nrow(mtcars)
library(corrplot)
mtcars <-na.omit(mtcars)
class(mtcars)
#计算mtcars数据框的相关性系数;
cor<- cor(mtcars)
#自定义渐变颜色；
col2 <- colorRampPalette(c("#77C034","white" ,"#C388FE"),alpha = TRUE)
#显著性计算：
res <- cor.mtest(mtcars, conf.level = .95)
p <- res$p
#饼图添加显著性星号；
corrplot(cor, order = "hclust",col = col2(100),
         method = "pie",
         cl.length=5, type = "upper",diag = T,
         p.mat = p,sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",pch.col="grey20",pch.cex=1.4,
         tl.col="grey20",tl.cex = 1,cl.pos = "r",cl.ratio = 0.2)
corrplot(cor,add = TRUE,method = 'number',
         type = 'lower', col = col2(100),
         order = c('hclust'), diag = T, number.cex = 0.9,
         tl.pos = 'l', tl.col="grey20",cl.pos = 'n',
         p.mat = p,
         insig = "pch",pch.col="grey20",pch.cex=2)




