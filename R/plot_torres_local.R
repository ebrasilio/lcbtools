#-----------------------------------------------------------------------#
# Emilia M S Brasilio - LCB-IAG-USP - 31/JUL/2009                       #
# Script do desespero para plotar a posição das torres..                #
# Versão 1.0                                                            #
# Tinn-R 1.17.2.4 2005                                                  #
#-----------------------------------------------------------------------#

 rm(list=ls())                             #Limpa a memória
#------------------------------------------------------------------

path <- "C:\\courage_backup\\Artigo_1\\Apresent\\"
input<- "lista.txt"

local <- read.table(paste(path,input,sep=""),header=T)

x <- local[,"Lon"]
y <- local[,"Lat"]
x11()
plot(x,y,type="p")

plot(local[,"Lon"],local[,"Lat"],type="b")
pch=20,xlim=c(-53.5,-44),ylim=c(-26.4,-44))

plot(ki2.dia[,"pdg"],ki2.dia[,"iag_ac"],pch=20,xlab="Cerrado-LCB",ylab="IAG_AC")

#path <- "D:\\Artigo_1\\"
#arquivo <- "Dia_est.csv"
# PacoteS                                      utilidade
library(doBy)                # muito boa para operacoes estatiscas basicas
library(plotrix)             # pre-requisito p/ drc e multiplos histogramas EMPTYSPACE
library(Hmisc)               # graficos de alto nivel- data.frame
#library(abind)              # cbind de matrizes e manipulacao arrays
#library(fields)             # funcao image.plot
#library(sp)                 # dados espaciais
#library(cluster)            # analise de cluster
#library(rgl)                # geração de superfícies de dados

#library(spam)               # pre-requisito p/ outro

#                             emptyspace
#library(alr3)               # pre-requisito p/ drc
#library(chron)              #gera sequencia de datas
#library(time)

#library(akima)              # interpolacao de ptos irregulares
#library(fCalendar)          # datas e calendarios
#library(its)                # irregular time series
#library(chron)              # gera sequencia de datas

#library(lattice)            # funcao levelplot

#library(tcltk)
#library(RNetCDF)            # NAO CONSEGUI INSTALAR - UDUNITS DEPENDENCIA!
#library(udunits)

#library(RHydro)             # geoprocessamento MNT
#library(RODBC)              # pre-requisito p/ outro


#library(Amelia)             # multiple imputation
#library(norm)
#library(Zelig)
#library(boot)

#library(climatol)
#library(seas)               # dados de estacao
#library(clim.pact)

#-------------------------------------------------------------------------------
# fUNÇÃO DO tATSCH

addtickmarks <- function(...,side,fator.tm) {
                         # fator.tm significa fator para tickmarks
                         # side=2 e um fator.tm=2, p.ex, duplica o numero de marcas default do R colocado no eixo y
                         # axis(side, at = pretty(axTicks(side), n = fator.tm *length(axTicks(side))),
                         #        labels = rep("",(fator.tm * length(axTicks(side)) - 1) )
                         #)

                          #res = resolução
                          res <- axTicks(side)[2]-axTicks(side)[1]
#                          posicoes <- seq(axTicks(side)[1],
#                                         axTicks(side)[length(axTicks(side))],
#                                          by = res/fator.tm)

                          posicoes <- seq(axTicks(side)[1]- 2*res/fator.tm,
                                         axTicks(side)[length(axTicks(side))] + 2*res/fator.tm,
                                          by = res/fator.tm)

                          axis(side, at = posicoes, labels = rep("", length(posicoes)) )
}
