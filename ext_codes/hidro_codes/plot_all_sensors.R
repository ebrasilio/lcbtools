# ---------------------------------------------------------
# plot básico dados umidade do solo
#
#----------------------------------------------------------

rm(list=ls())
setwd('/data1/DATA/LCB/EXT/STORAGE/codes/hidro_codes')
source('~/git/lcbtools/R/plots.R')

library(openair)
library(ggplot2)
library(reshape2)

inp = list.files('../../data/H/merge', pattern='merge.txt', full.names = T)
sta = substr(inp, nchar(inp)-12, nchar(inp)-10)
setAs('character','date', function(from) as.POSIXct(from, format="%Y-%m-%d %H:%M:%S",tz='GMT'))
clas = c('date', rep('numeric',7))

dt <- lapply(inp, function(.file){
              read.csv(.file, header = T, na.strings='NA',stringsAsFactors = F,
                       colClasses = clas)})

aws <- read.csv('../../data/C/merge_resample/C05clear_merge.txt', 
                colClasses=c('date', rep('numeric',8)))
names(aws)[1] = 'date' 
aws=timeAverage(aws, avg.time = '10 min', start.date=paste0(substr(aws$date[1],1,15),'0:00'))

#timePlot(aws, names(aws)[5])

for(i in 1:length(dt)){
  cat(sta[i],'\n')
  pdf(paste0('ext_',sta[i],'.pdf'),width = 10)
  aux1 = timeAverage(dt[[i]], avg.time = '10 min', start.date=paste0(substr(dt[[i]]$date[1],1,15),'0:00'))
  aux = merge(dt[[i]], aws[,c(1,5)], all=T)
  bla = seq.POSIXt(aux$date[1], aux$date[nrow(aux)], by='5 day')
  for(j in 2:length(bla)){
    aux2 = selectByDate(aux, bla[j-1], bla[j])
    if(nrow(aux2) >0 ){
      timePlot(aux2, names(aux)[-1], 
               y.relation = 'free',ylab="", key.columns = 2,
               main = paste(bla[j-1],'-',bla[j]))
    }
  }
  dev.off()
}




