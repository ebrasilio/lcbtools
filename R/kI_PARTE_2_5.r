###########################################################################
# Emilia M S Brasilio - LCB-IAG-USP - 31/Jul/2009                         #
# Script desenvolvido para comparar dados de ki do cerrado e dados de saté#
# lite gerando pêntadas. Baseado no script compara.R escrito por Tatsch   #
# J D.                                                                    #
#                                                                         #
###########################################################################
# DIRETÓRIO DE SCRIPTS
 dirfonte <- "F://R_STAT//SCRIPTS//"        # COURAGE NO WINDOWS
 source(paste(dirfonte,"E_inicio.r",sep=""))

# DIRETORIO DE DADOS DE ENTRADA E SAIDA
path <- "F:\\Artigo_1\\DADOS\\"
arquivo <- "Dia_est.csv"
input <- paste(path,arquivo,sep="")

ki <- read.csv(input,na.strings="-99.90",header=T)

linhas <- complete.cases(ki)
ki2 <- ki[linhas,]

#op <- par(mfrow=c(2,2))   Usar apenas para plotar a familia toda

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
#passar tudo para Wm2

x <- ki2[,"pdg"]*8.164
y <- ki2[,"ec_esalq"]*8.164

plot(x,y,pch=20,xlab="Cerrado-LCB (W/m²)",ylab="EC_ESALQ (W/m²)",xlim=c(0,300),ylim=c(0,300),
     cex.axis=1.2,cex.lab=1.2,las=1)
     addtickmarks(side=1,fator.tm=5)
     addtickmarks(side=2,fator.tm=5)
#plot(ki2[,"pdg"],ki2[,"ec_esalq"],pch=20,xlab="Cerrado-LCB",ylab="EC_ESALQ",xlim=c(0,30),ylim=c(0,30))
reg4 <- lm(y~x)
#reg4 <- lm(ki2[,"ec_esalq"]~ki2[,"pdg"])
eq <- paste("y =", paste(paste(round(coef(reg4)[[2]],2),"x +",sep=""),round(coef(reg4)[[1]],2),sep=" "),sep=" ")
  r2 <-
  abline(reg4,lwd=2,lty=2)
   segments(-10,-10,300,300,lwd=2, col=1)
 es <-emptyspace(na.exclude(x),na.exclude(x))
#    es <-emptyspace(na.exclude(ki2[,"pdg"]),na.exclude(ki2[,"pdg"]))
    text(es,eq,cex=2)

    out<-summary(reg4)
    text(es$x,es$y-2,paste("\n\nR² =",round(out$r.squared,2),sep=""),cex=2)
   box(lwd=2)

#cf <- coef(reg4)
#summary(reg4)
#abline(reg4)
#abline(reg4,col="red")
#abline(0,1,col="blue")               #1:1
#mtext(sprintf("%s x + %s\n R2 = 0.64",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)


#esalq autoXconv
#plot(ki2[,"ea_esalq"],ki2[,"ec_esalq"],pch=20,xlab="auto",ylab="conv")
##reg5 <- lm(ki2[,"ec_esalq"]~ki2[,"ea_esalq"])
#cf <- coef(reg5)
#summary(reg5)
#abline(0,1,col="blue")
#abline(reg5,col="red")
#mtext(sprintf("%s x + %s \n R2 = 0.11",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)

