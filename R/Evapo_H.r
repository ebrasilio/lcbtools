################################################################################
#  Emilia M S Brasilio - 01/12/2010
#  Gerando figuras de H e Le para trabalho do Tatsch
#
################################################################################

rm(list=ls())

library(doBy)
library(chron)
library(openair)

###############################################################################
# Funções 
# data.thresh
# The data capture threshold to use (%). A value of zero means that all available 
# data will be used in a particular period regardless if of the number of values 
# available. Conversely, a value of 100 will mean that all data will need to 
#be present for the average to be calculated, else it is recorded as NA.

timeAvg <- function(x, data.thresh = 50)
{
    ifelse( (sum(!is.na(x))/length(x) * 100) >= data.thresh, mean(x, na.rm=T), NA)
}



# cumAvg, faz médias com critério de dados ausentes.
# data.thresh é o limite de falhas em porcentagem.
cumAvg <- function(x, data.thresh = 50)
{
    ifelse( (sum(!is.na(x))/length(x) * 100) >= data.thresh, sum(x, na.rm=T), NA)
}
##################################################################################


hdata <- read.table("PDG_USR_VCP_2001-10.dat",header=F, stringsAsFactors = F, 
                    na.strings= c("-9999.00000000","-7777.00000000"))
names (hdata) <- c("ano","doy","hh","mn","Hpdg","Epdg","Hvcp","Evcp","Husr","Eusr")
date <- strptime(paste(hdata$ano, hdata$doy, hdata$hh, hdata$mn, sep=" "),format="%Y %j %H %M",tz="GMT")
hdata <- cbind(date,hdata)



timePlot(dia.d, pollutant=c("Hpdg","Epdg","Hvcp","Evcp","Husr","Eusr"),auto.text=F,percentile = 95)





media.mes <- summaryBy(Hpdg + Hvcp + Husr ~ mes,dia.d,FUN = timeAvg)

png("obs_H.png",bg="white")
  plot(media.mes$Hpdg.timeAvg,col=2,type="b",ylim=c(-10,150))
    lines(media.mes$Hvcp.timeAvg,col=3,type="b")
      lines(media.mes$Husr.timeAvg,col=4,type="b")

      legend("top",leg=c("PDG","VCP","USR"),col=c(2,3,4),lwd=1)
dev.off()

acumulado.mes <- summaryBy(Epdg + Evcp + Eusr ~ ano+mes,dia.d,FUN = cumAvg)

  plot(acumulado.mes$Epdg.cumAvg,col=2,type="b",ylim=c(0,500),xlim=c(1,40))
    lines(acumulado.mes$Evcp.cumAvg,col=3,type="b")
      lines(acumulado.mes$Eusr.cumAvg,col=4,type="b")

      legend("topright",leg=c("PDG","VCP","USR"),col=c(2,3,4),lwd=1)

media.acm <- summaryBy(Epdg.cumAvg + Evcp.cumAvg + Eusr.cumAvg ~ mes,acumulado.mes,FUN = timeAvg)
names(media.acm) <- c("mes","Epdg","Evcp","Eusr")

png("obs_E.png",bg="white")
  plot(media.acm$Epdg,col=2,type="b",ylim=c(0,500))
    lines(media.acm$Evcp,col=3,type="b")
      lines(media.acm$Eusr,col=4,type="b")

      legend("top",leg=c("PDG","VCP","USR"),col=c(2,3,4),lwd=1)
dev.off()




