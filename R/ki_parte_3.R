#plot(dd[,"pdg"],type="l")  plotar a série

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
 path <- "F://Artigo_1//DADOS//"
 fsitios <- "Dia_est.csv"

# LEITURA DOS DADOS DIARIOS
 dd <- read.csv(paste(path,fsitios,sep=""),head=T,na.strings="-99.90")
 dd <- dd[-c(1:4),]
 dd1 <- subset(dd, ano <= 2006, select=c("ano","doy","mes","dia","pdg"))
 pos <- which(dd[,"dia"]==24 & dd[,"mes"]==12 & dd[,"ano"]==2006)
 dd <- dd[1:pos,]
 
# completando ano de 2009
#addoys <- 1:3
#
#tempo <-seq(as.Date("2007/1/1"), as.Date("2007/1/3"), "days")
#mes <- month.day.year(unclass(tempo))$month
#dia <- month.day.year(unclass(tempo))$day
#ano <- month.day.year(unclass(tempo))$year
#compldd <- data.frame(ano,addoys,mes,dia, matrix(NA,nrow=length(addoys),ncol=1))
#names(compldd) <- names(dd)
# juntando os dois conjunto de dados
#dd <- data.frame(abind(dd,compldd,along=1)) # se nao transforma p/ dataframe, fica como matriz
# liberando memoria
#rm(compldd, addoys, ano, mes,dia)

#check
#nrow(dd[c(F,F,F,F,T),c("dia","mes")])
#-------------------------------------------------------------------------------
# ler arquivo de dados radiacao satelite
fsat <- "47.6W_21.6S.txt"
 
# dados pentadas satelite, sem cabecalho!
dpsat <- read.table(paste(path,fsat,sep=""),head=F) 
 names(dpsat) <- c("year","month","day","Srad")

# so apartir de 2001 interessa
  dpsat <- subset(dpsat, year >= 2001)
# exclui a primeira pentada
  dpsat <- dpsat[-c(1),]
                                 

#-------------------------------------------------------------------------------
# DEFININDO FUNCOES
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# funcao para calcular media com criterio
# FUNCAO PARA FILTRAGEM DA MEDIA: CALCULA MEDIA SOMENTE SE HOUVER NDADOS DISPONIVEIS
# NDADOS EH O CRITERIO(tolerancia, tolcrt) PARA O CALCULO DA MEDIA
# tolcrt= criterio de tolerancia, eh o numero de dados acima do qual a media sera calculada
# tolcrt=24, valor default, calucla media se tiver pelo menos 24 dias de dados no mes
# oper= "media" ou "soma", default eh oper="media"
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

FiltAvg <- function(x,tolcrt=3,oper="media")
{ 
# ndados
  nd <-length(which(!is.na(x)))                                   
# calcula media somente para meses com mais de "tolcrt" dias de dados nao faltantes
   if(nd >= tolcrt) 
   {
    if(oper=="media") avg <- round(mean(x,na.rm=T),digits=4)  else avg <- round(sum(x,na.rm=T),digits=4)
   } else avg <-NA                  		         
  return(avg)
} # end FiltAvg
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# funcao para fazer media pentadas
# transforma o vetor de entrada em uma matriz (mat) com 5 linhas e k colunas(onde k = length(x)/5) 
# entao aplica a funcao FiltAvg em cada coluna de mat.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
PenAvg <- function(x)
{
 mat <- matrix(x, nrow=5)
 res <- apply(mat,2,FiltAvg)  # 1 para linhas, 2 para colunas
 return(res) 
} # end PenAvg
#=================================================================================
# check
# plot(PenAvg(dd[,5]),type="l")

# gerar serie pentadal de dd
cols <- 5  # se houvesse mais do q uma colunas colocaria 5:9, a conta abaixo funcionaria
#mp <- apply(dd[,cols], 2, PenAvg) # medias pentadas dos dados de radiacao dos sitios
mp <- PenAvg(dd[,"pdg"])

dpjuntos <- data.frame(dd[c(F,F,F,F,T),c("ano","mes","dia")],cerrado=mp, Srad=dpsat[,"Srad"])
 dpjuntos[dpjuntos[,5]==0, 5]<-NA #(o é dados faltantes no satelite)
  dpjuntos[,5] <- dpjuntos[,5]*(86400/10^6)

#      plot(dpjuntos[,"cerrado"],type="o",ylim=c(4,31),pch=20,col="red")
#      lines(dpjuntos[,"Srad"],col="blue",pch=20)

x <- dpjuntos[,"cerrado"]*8.164   # passei para Wm-2
y <- dpjuntos[,"Srad"] *8.164
#dpjuntos
#ddjun <- dpjuntos
#head(ddjun)
#head(dpjuntos)

x1 <- c(x[-c(1)],NA)
y1 <- c(y[-c(1)],NA)
#op <- par(mfrow=c(2,2))
# plot(x,y, pch=20, ylim=c(0,300), xlim=c(0,300),xlab="cerrado", ylab="satelite")
# plot(x,y, pch=20,  xlab="cerrado", ylab="satelite")
#  abline(lm(y~x))
# plot(x1,y, pch=20, ylim=c(5,31),xlab="cerrado", ylab="satelite")
# plot(x1,y, pch=20,ylim=c(0,300), xlim=c(0,300),xlab="cerrado", ylab="satelite")
#  abline(lm(y~x1))
# plot(x,y1, pch=20, ylim=c(5,31),xlab="cerrado", ylab="satelite")
# plot(x,y1, pch=20,ylim=c(0,300), xlim=c(0,300),xlab="cerrado", ylab="satelite")
#  abline(lm(y1~x))
#par(op)


# melhor resultado
op <- par(mfrow=c(2,2), mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(x1,y,pch=20, ylim=c(0,300),xlim=c(0,300),xlab="cerrado (W/m²)", ylab="GL12 (W/m²)",
      cex.axis=1.2,cex.lab=1.2,las=1)
      addtickmarks(side=1,fator.tm=5)
      addtickmarks(side=2,fator.tm=5)
  reg <- lm(y~x1)
  eq <- paste("y =", paste(paste(round(coef(reg)[[2]],2),"x +",sep=""),round(coef(reg)[[1]],2),sep=" "),sep=" ")
  r2 <- 
  abline(reg,lwd=2,lty=2)
   segments(-10,-10,300,300,lwd=2, col=1)
    es <-emptyspace(na.exclude(x),na.exclude(x))
    text(es, eq,cex=2)
    
    out<-summary(reg)
    text(es$x,es$y-2,paste("\n\nR² =",round(out$r.squared,2),sep=""),cex=2)
   box(lwd=2)

plot(x1 ,type="p",pch=21,bg=1,col="gray",ylim=range(x1,y,na.rm=T),xaxt="n",lty=1,lwd=2,cex.axis=1.2,cex.lab=1.2,las=1)
#points(x1,pch=21,col="gray")
 tempo<-seq(from=as.Date("2001/1/9"), length=length(x1)*5, by="days")[c(T,F,F,F,F)]
    axis(1, at=(1:length(x1))[c(rep(F,11),T)], labels=format(tempo[c(rep(F,11),T)],"%b/%y" ))
   lines(y,lty=1,lwd=2)
   box(lwd=2)

plot(density(na.exclude(x1)),xlim=range(x1,y,na.rm=T),lwd=2, main=" ",las=1);
lines(density(na.exclude(y)),lty=2,lwd=2)
box(lwd=2)   

boxplot(cbind(cerrado=x1,GL12=y),las=1)
box(lwd=2)
      
par(op)      



summary(reg)




x1.n <-(x1-mean(x1,na.rm=T))/sd(x1,na.rm=T)
y.n  <- (y-mean(y,na.rm=T))/sd(y,na.rm=T)

x11()
op<-par(mfrow=c(2,1))
plot(x1.n ,type="o",pch=21,bg=2,col="red",ylim=range(x1.n,y.n,na.rm=T),xaxt="n",lty=1,lwd=1,cex.axis=1.2,cex.lab=1.2,las=1)
#points(x1,pch=21,col="gray")
 tempo<-seq(from=as.Date("2001/1/9"), length=length(x1)*5, by="days")[c(T,F,F,F,F)]
    axis(1, at=(1:length(x1))[c(rep(F,11),T)], labels=format(tempo[c(rep(F,11),T)],"%b/%y" ))
   lines(y.n,lty=1,lwd=1)
   lines(x1.n-y.n,col="blue")
   box(lwd=2)
plot(density(na.exclude(x1.n)),xlim=range(x1.n,y.n,na.rm=T),lwd=2, main=" ",las=1);
lines(density(na.exclude(y.n)),lty=2,lwd=2)
box(lwd=2)    
par(op)

x11()
op<-par(mfrow=c(1,2),las=1)
plot(x1.n-y.n,type="l",col="blue")
        abline(h=0);box(lwd=2)
        hist(x1.n-y.n,col="blue")
        box(lwd=2)   
par(op)   


#plotar a regressão par ao paper

plot(x1,y,pch=20, ylim=c(0,300),xlim=c(0,300),xlab="cerrado (W/m²)", ylab="GL12 (W/m²)",
      cex.axis=1.2,cex.lab=1.2,las=1)
      addtickmarks(side=1,fator.tm=5)
      addtickmarks(side=2,fator.tm=5)
  reg <- lm(y~x1)
  eq <- paste("y =", paste(paste(round(coef(reg)[[2]],2),"x +",sep=""),round(coef(reg)[[1]],2),sep=" "),sep=" ")
  r2 <-
  abline(reg,lwd=2,lty=2)
   segments(-10,-10,310,310,lwd=2, col=1)  #arrumar isso
    es <-emptyspace(na.exclude(x),na.exclude(x))
    text(es, eq,cex=2)

    out<-summary(reg)
    text(es$x,es$y-2,paste("\n\nR² =",round(out$r.squared,2),sep=""),cex=2)
   box(lwd=2)
