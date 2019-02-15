rm(list=ls())
library(doBy)
#library(lattice)

path <- "F:\\Artigo_1\\DADOS\\"
arquivo <- "Dia_est.csv"
input <- paste(path,arquivo,sep="")

ki <- read.csv(input,na.strings="-99.90",header=T)

#labels
# names(ki.dia) <- c("ano","mes","dia","iag_ac","iag_pir","esalq_con","esalq_auto","pdg")

#selecionar dias com dados dos tres sitios
#linhas <- complete.cases(ki.dia)
linhas <- complete.cases(ki)
ki2 <- ki[linhas,]

# ki que nao eh comum nos 3 coloca NA
#kicom <- ki
#kicom[!linhas,-c(1,2,3)] <- NA
#kicheck <- ki[!linhas,]

#which(is.na(ki[,5]) & is.na(ki[,6]))

# check
#matplot(kicom[,-c(1,2,3)], type="l", lty=1)
#pairs(kicom[,-c(1,2,3)])

#ki2.dia <- ki[linhas,]
#ki2reg <- ki[linhas,]

#mypanel <- function(x,y,...){
#   panel.xyplot(x,y,...)
#   panel.lmline(x,y)
# }
#xyplot(esalq_con ~ pdg | mês,data=ki2reg,panel=mypanel) 


#plotar os dados e as regressões
op <- par(mfrow=c(2,2))
#pdg e iag_ac
plot(ki2[,"pdg"],ki2[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")
reg1 <- lm(ki2[,"iag_ac"]~ki2[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s \n R2 = 0.44",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")
abline(0,1,col="blue")


#pdg e iag_pir
plot(ki2[,"pdg"],ki2[,"iag_pir"],pch=20,xlab="Cerrado-LCB",ylab="IAG_pir")
reg2 <- lm(ki2[,"iag_pir"]~ki2[,"pdg"])
cf <- coef(reg2)
summary(reg2)
abline(reg2,col="red")
abline(0,1,col="blue")               #1:1
mtext(sprintf("%s x + %s\n R2 = 0.44",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)

# pdg e esalq
plot(ki2[,"pdg"],ki2[,"ea_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EA_ESALQ")
reg3 <- lm(ki2[,"ea_esalq"]~ki2[,"pdg"])
cf <- coef(reg3)
summary(reg3)
abline(reg3,col="red")
abline(0,1,col="blue")               #1:1
mtext(sprintf("%s x + %s\n R2 = 0.07",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)

# pdg e esalq
#plot(ki2[,"pdg"],ki2[,"ec_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EC_ESALQ")
plot(ki2[,"pdg"],ki2[,"ec_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EC_ESALQ",xlim=c(0,30),ylim=c(0,30))
reg4 <- lm(ki2[,"ec_esalq"]~ki2[,"pdg"])
cf <- coef(reg4)
summary(reg4)
abline(reg4)
#abline(reg4,col="red")
#abline(0,1,col="blue")               #1:1
mtext(sprintf("%s x + %s\n R2 = 0.64",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)


#esalq autoXconv
plot(ki2[,"ea_esalq"],ki2[,"ec_esalq"],pch=20,xlab="auto",ylab="conv")
reg5 <- lm(ki2[,"ec_esalq"]~ki2[,"ea_esalq"])
cf <- coef(reg5)
summary(reg5)
abline(0,1,col="blue")
abline(reg5,col="red")
mtext(sprintf("%s x + %s \n R2 = 0.11",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)

