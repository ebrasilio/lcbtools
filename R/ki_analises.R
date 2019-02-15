#Parte dois 
#Comparar nossa série com as séries da esalq e iag.
# diretorio de trabalho
# clean all
rm(list=ls())
# link para bibliotecas
library(doBy)
library(lattice)

path <- "F:\\Artigo_1\\dados_para_analise\\"
arquivo <- "ki_dia_new_1.csv"
input <- paste(path,arquivo,sep="")
#ler *.csv
#ki.dia <- read.csv(input,na.strings="-99.90",header=T)
ki <- read.csv(input,na.strings="-99.90",header=T)

#labels
# names(ki.dia) <- c("ano","mes","dia","iag_ac","iag_pir","esalq_con","esalq_auto","pdg")

#selecionar dias com dados dos tres sitios
#linhas <- complete.cases(ki.dia)
linhas <- complete.cases(ki)
#ki2.dia <- ki.dia[linhas,]

# ki que nao eh comum nos 3 coloca NA
kicom <- ki
kicom[!linhas,-c(1,2,3)] <- NA
kicheck <- ki[!linhas,]

which(is.na(ki[,5]) & is.na(ki[,6]))

# check
#matplot(kicom[,-c(1,2,3)], type="l", lty=1)
#pairs(kicom[,-c(1,2,3)])

#ki2 <- ki[linhas,]
ki2reg <- ki[linhas,]

mypanel <- function(x,y,...){
   panel.xyplot(x,y,...)
   panel.lmline(x,y)
 }
xyplot(esalq_con ~ pdg | mês,data=ki2reg,panel=mypanel) 


#plotar os dados e as regressões
op <- par(mfrow=c(2,2))
#pdg e iag_ac
plot(ki2.dia[,"pdg"],ki2.dia[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")
reg1 <- lm(ki2.dia[,"iag_ac"]~ki2.dia[,"pdg"])
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
plot(ki2.dia[,"pdg"],ki2.dia[,"esalq"],pch=20,xlab="Cerrado-LCB",ylab="ESALQ")
reg3 <- lm(ki2.dia[,"esalq"]~ki2.dia[,"pdg"])
cf <- coef(reg3)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg3)
abline(reg2,col="red")

#esalq e iag_ac
plot(ki2.dia[,"esalq"],ki2.dia[,"iag_ac"],pch=20,xlab="ESALQ",ylab="IAG_ac")
reg4 <- lm(ki2.dia[,"iag_ac"]~ki2.dia[,"esalq"])
cf <- coef(reg4)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg4)
abline(reg4,col="red")


