#ler *.csv
ki.dia <- read.csv("ki_dia.csv",na.strings="-99.9")

#labels
 names(ki.dia) <- c("ano","doy","iag","esalq","pdg")

#selecionar dias com dados dos tres sitios
linhas <- complete.cases(ki.dia)
ki.dia <- ki.dia[linhas,]

#plotar os dados e as regressões
plot(ki2.dia[,"pdg"],ki2.dia[,"iag"],pch=20,xlab="Cerrado-LCB",ylab="IAG")
reg1 <- lm(ki2.dia[,"iag"]~ki2.dia[,"pdg"])
cf <- coef(reg1)
mtext(sprintf("%s x + %s",round(cf[2],2),round(cf[1],2)),side=3,line=1,font=2)
summary(reg1)
abline(reg1,col="red")

# pdg e esalq
plot(ki2.dia[,"pdg"],ki2.dia[,"esalq"],pch=20,)
reg2 <- lm(ki2.dia[,"esalq"]~ki2.dia[,"pdg"])
cf <- coef(reg2)
mtext(sprintf("%s x + %s",round(cf[2],2),round(cf[1],2)),side=3,line=1,font=2)
summary(reg2)
abline(reg1,col="red")

#plotar os dados e as regressões
plot(ki2.dia[,"esalq"],ki2.dia[,"iag"],pch=20,xlab="ESALQ",ylab="IAG")
reg3 <- lm(ki2.dia[,"iag"]~ki2.dia[,"esalq"])
cf <- coef(reg3)
mtext(sprintf("%s x + %s",round(cf[2],2),round(cf[1],2)),side=3,line=1,font=2)
summary(reg3)
abline(reg1,col="red")

# As regressões mostram o cerrado com maior irradiancia que os demais, 
# vou refazer as medias do pdg com todos os dados.

min2 

dd3.dia <- summaryBy(cerrado~ano+doy,min2,FUN=mean)

