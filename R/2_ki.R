# so os comandos
rm(list=ls())
library(doBy)
#library(lattice)
path <- "D:\\Artigo_1\\"
arquivo <- "Dia_est.csv"
input <- paste(path,arquivo,sep="")
ki <- read.csv(input,na.strings="-99.90",header=T)
linhas <- complete.cases(ki)
ki2.dia <- ki[linhas,]
plot(ki2.dia[,"pdg"],ki2.dia[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")
reg1 <- lm(ki2reg[,"iag_ac"]~ki2reg[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")