# -----------------------------------------------------------------------------
# Ordenar dados de Fluxo de Calor no solo
# cardcpnvert nao converte !!!!
# TODO CONVERTER SHF
#------------------------------------------------------------------------------
rm(list=ls())

library(openair)

# primeira parte da coleta, junto com fluxos apenas ano 2017
shf <- list.files("../../..", pattern = "50318.SHF", recursive = T, full.names = T)

tt = lapply(tt,
            function(.file) 
                read.csv(.file,skip=4, na.strings='\"NAN\"', header=F))

#------------------------------
# arquivos 1 e 2 com 87 colunas
tt1 <- list()
for(i in 1:2){
    tt1[[i]] <- tt[[i]]
} 
tt1 = do.call(rbind, tt1)
names(tt1) = unlist(strsplit(readLines(flux[1],2)[2],'\",\"'))
names(tt1)[1] = "date"
tt1$date = as.POSIXct(tt1$date, format="%Y-%m-%d %H:%M:%S", tz = "GMT")





