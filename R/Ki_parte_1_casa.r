#Primeira Parte Médias dos nossos sítios e regrerssões
# clean all
rm(list=ls())

# Inicializando as bibliotecas
library(doBy)
library(lattice)

# Caminho do arquivo de entrada
path <- "D:\\Artigo_1\\"
arquivo <- "pdg_usr_vcp_ki_30m.dat"
input <- paste(path,arquivo,sep="")

# ler dados 30 minutos
min <- read.table(input,na.strings="-9999.00000000")
names(min) <- c("ano","doy","hh","mn","cerrado","cana","eucalipto")

#Separar linhas com dados válidos nos três sítios
linhas <- complete.cases(min)
min2 <- min[linhas,]

# fazer media diaria apenas com os dados diurnos entre 8 e 18 horas
min3 <- subset(min2,hh>=8 & hh<18)
dd.dia <- summaryBy(cerrado+cana+eucalipto~ano+doy,min3,FUN=mean)
names(dd.dia) <- c("ano","doy","cerrado","cana","eucalipto")

#Fazer as regressões dos sítios do LCB
#Cerrado e cana
plot(dd.dia[,"cerrado"],dd.dia[,"cana"],pch=20,xlab="Cerrado",ylab="Cana-de-açucar")
rg1 <- lm(dd.dia[,"cana"]~dd.dia[,"cerrado"])
cf <- coef(rg1)
mtext(sprintf("%sx + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
abline(rg1,col="red")
                           
#Cerrado e eucalipto
plot(dd.dia[,"cerrado"],dd.dia[,"eucalipto"],pch=20,xlab="Cerrado",ylab="Eucalipto")
rg2 <- lm(dd.dia[,"eucalipto"]~dd.dia[,"cerrado"])
cf <- coef(rg2)
mtext(sprintf("%sx + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
abline(rg2,col="red")

#Cana e eucalipto
#plot(dd.dia[,"cana"],dd.dia[,"eucalipto"],pch=20,xlab="Cana-de-açucar",ylab="Eucalipto")
#rg3 <- lm(dd.dia[,"eucalipto"]~dd.dia[,"cana"])
#cf <- coef(rg3)
#mtext(sprintf("%sx  %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
#abline(rg3,col="red")

#Eucalipto e Cana Ficou melhor
plot(dd.dia[,"eucalipto"],dd.dia[,"cana"],pch=20,ylab="Cana-de-açucar",xlab="Eucalipto")
rg4 <- lm(dd.dia[,"cana"]~dd.dia[,"eucalipto"])
cf <- coef(rg4)
mtext(sprintf("%sx + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
abline(rg4,col="red")

clima <- "climatologia.dat"
out_a <- paste(path,clima,sep="")

#Média diária para comparar com os outros dois sítios
clima.dia <- subset(min,hh>=8 & hh<18)
clima1.dia <- summaryBy(cerrado+cana+eucalipto~ano+doy,clima.dia,FUN=mean)
names(clima1.dia) <- c("ano","doy","cerrado","cana","eucalipto")
write.table(clima1.dia,file=out_a,append=FALSE,col.names=TRUE)

saida <- "pdg_MJ.dat"
output <- paste(path,saida,sep="")


#Transformando a media do cerrado de Wm^-2 em MJm-2
pdg <-clima1.dia["cerrado"]*0.0324
write.table(pdg,file=output,append=FALSE,col.names=TRUE)














#DAQUI EM DIANTE MUDOU TUDO!!!!!!!!!!!!!!!!
#ler *.csv
ki.dia <- read.csv("media_dia_sitios.csv",na.strings="-99.90")

#labels
 names(ki.dia) <- c("ano","doy","iag_ac","iag_pir","esalq","pdg")

#selecionar dias com dados dos tres sitios
linhas <- complete.cases(ki.dia)
ki2.dia <- ki.dia[linhas,]

#plotar os dados e as regressões
#pdg e iag_ac
plot(ki2.dia[,"pdg"],ki2.dia[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")
reg1 <- lm(ki2.dia[,"iag_ac"]~ki2.dia[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s",round(cf[2],2),round(cf[1],2)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")

#pdg e iag_pir
plot(ki2.dia[,"pdg"],ki2.dia[,"iag_pir"],pch=20,xlab="Cerrado-LCB",ylab="IAG_pir")
reg1 <- lm(ki2.dia[,"iag_pir"]~ki2.dia[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s",round(cf[2],2),round(cf[1],2)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")

# pdg e esalq
plot(ki2.dia[,"pdg"],ki2.dia[,"esalq"],pch=20,xlab="Cerrado-LCB",ylab="ESALQ")
reg2 <- lm(ki2.dia[,"esalq"]~ki2.dia[,"pdg"])
cf <- coef(reg2)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg2)
abline(reg2,col="red")

#plotar os dados e as regressões ????????
plot(ki2.dia[,"esalq"],ki2.dia[,"iag"],pch=20,xlab="ESALQ",ylab="IAG")
reg3 <- lm(ki2.dia[,"iag"]~ki2.dia[,"esalq"])
cf <- coef(reg3)
mtext(sprintf("%s x + %s",round(cf[2],3),round(cf[1],3)),side=3,line=1,font=2)
summary(reg3)
abline(reg3,col="red")

# As regressões mostram o cerrado com maior irradiancia que os demais, 
# vou refazer as medias do pdg com todos os dados.

min2 

dd3.dia <- summaryBy(cerrado~ano+doy,min2,FUN=mean)

