#-----------------------------------------------------------------------#
# Emilia M S Brasilio - LCB-IAG-USP - 31/JUL/2009                       #
# Script de inicialização do R, onde as bibliotecas mais utilizadas e os#
# caminhos para os scripts e para os arquivos estão definidos           #
# Baseado do script Jinicia.R de Tatsch,JD.                             #
#                                                                       #
# Versão 1.0                                                            #
# Tinn-R 1.17.2.4 2005                                                  #
#-----------------------------------------------------------------------#
# graphics.off()
 rm(list=ls())                             #Limpa a memória
#------------------------------------------------------------------
# Pacotes instalados
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
