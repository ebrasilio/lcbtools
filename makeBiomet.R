#...................................................
# Criar Biomet para fluxos turbulentos
#
#...................................................

rm(list=ls())
library(openair)


inp <- read.csv('~/lcb_2021/EXT/data/2021/21231/EP_2/TOA5_EP2_21231_WXT_4.dat',
                skip=4, header=F,
                na.strings=c('NAN','NA'), stringsAsFactors = F)
inp$date <- as.POSIXct(inp$V1, tz='GMT')
inp_30 <- timeAverage(inp[, -1], avg.time= '30 min', data.thresh = 0,
                      start.date = paste0(substr(inp$date[1],1,14), '00:00'))
