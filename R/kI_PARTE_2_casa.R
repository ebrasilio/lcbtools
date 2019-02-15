#Parte dois 
#Comparar nossa série com as séries da esalq e iag.
rm(list=ls())       # clean all
# link para bibliotecas
library(doBy)
#library(lattice)

path <- "F:\\Artigo_1\\"
arquivo <- "Dia_est.csv"
input <- paste(path,arquivo,sep="")

#ler *.csv
ki <- read.csv(input,na.strings="-99.90",header=T)

#selecionar dias com dados dos tres sitios
linhas <- complete.cases(ki)

# ki que nao eh comum nos 3 coloca NA
#kicom <- ki
#kicom[!linhas,-c(1,2,3)] <- NA
#kicheck <- ki[!linhas,]

#which(is.na(ki[,8]) & is.na(ki[,9]))

# check, plots das séries de dados
#matplot(kicom[,-c(1,2,3,4)], type="l", lty=1)

#Plotar as series duas a duas
#pairs(kicom[,-c(1,2,3,4)])

ki2.dia <- ki[linhas,]
ki2reg <- ki[linhas,]

mypanel <- function(x,y,...){
   panel.xyplot(x,y,...)
   panel.lmline(x,y)
 }
#plots mes a mes
xyplot(iag_ac ~ pdg | mês,data=ki2reg,panel=mypanel) 
xyplot(iag_pir ~ pdg | mês,data=ki2reg,panel=mypanel) 
xyplot(ea_esalq ~ pdg | mês,data=ki2reg,panel=mypanel) 
xyplot(ec_esalq ~ pdg | mês,data=ki2reg,panel=mypanel) 


#plotar os dados e as regressões
#op <- par(mfrow=c(2,2))
#pdg e iag_ac
plot(ki2reg[,"pdg"],ki2reg[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")
reg1 <- lm(ki2reg[,"iag_ac"]~ki2reg[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")

#pdg e iag_pir
plot(ki2.dia[,"pdg"],ki2.dia[,"iag_pir"],pch=20,xlab="Cerrado-LCB",ylab="IAG_pir")
reg2 <- lm(ki2.dia[,"iag_pir"]~ki2.dia[,"pdg"])
cf <- coef(reg2)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg2)
abline(reg1,col="red")

# pdg e esalq
plot(ki2.dia[,"pdg"],ki2.dia[,"ec_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EC_ESALQ")
reg3 <- lm(ki2.dia[,"ec_esalq"]~ki2.dia[,"pdg"])
cf <- coef(reg3)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg3)
abline(reg2,col="red")

#esalq e iag_ac
#plot(ki2.dia[,"esalq"],ki2.dia[,"iag_ac"],pch=20,xlab="ESALQ",ylab="IAG_ac")
#reg4 <- lm(ki2.dia[,"iag_ac"]~ki2.dia[,"esalq"])
#cf <- coef(reg4)
#mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
#summary(reg4)
#abline(reg4,col="red")

# pdg e esalq_autoatico
plot(ki2.dia[,"pdg"],ki2.dia[,"ea_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EA_ESALQ")
reg5 <- lm(ki2.dia[,"ea_esalq"]~ki2.dia[,"pdg"])
cf <- coef(reg5)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg5)
abline(reg5,col="red")

