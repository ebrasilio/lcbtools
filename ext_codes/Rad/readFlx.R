# -----------------------------------------------------------------------------
# Ordenar dados de Saldo de Radiação
# de 
#------------------------------------------------------------------------------
rm(list=ls())

library(openair)

eddy = list.files("../../..", pattern = "eddypro_EXT", recursive = T, full.names = T)
eddy_18 <- eddy[substr(eddy, 10,13) =='2018']

# juntar apenas dados 2018 agora
tt = lapply(eddy_18, function(.file)
                    read.csv(.file,skip=3, na.strings='-9999.0',
                             header=F))
tt1 = do.call(rbind,tt)
names(tt1) = unlist(strsplit(readLines(eddy_18[1],2)[2],','))
tt1$date = as.POSIXct(paste(tt1$date, tt1$time), tz = "GMT")

timePlot(tt1, 'H')
