# --------------------------------------------------------------------
# Plots diagnosticos
#---------------------------------------------------------------------

library(reshape2)
library(ggplot2)
library(openair)

#1-ler os dados
sta <- list.files('../../data/H/merge','merge.txt',full.names = T)
#sta <- '../../data/H/merge/H03_merge.txt'

for(i in 1:length(sta)){
  aux <- read.csv(sta[i])
  aux$date <- as.POSIXct(aux$date, tz='GMT')
  
  # juntar por nível
  tt <- list()
  for(l in 1:6)
  
  #2- plotar a série temporal
  aux <- melt(aux[,-2], id.vars = 'date')
  p <- 
    ggplot(aux, aes(x=date, y=value, col=variable)) +
    geom_point() + geom_line() + 
    theme(legend.position = 'bottom') + 
    ggtitle(substr(sta[i],20,22))

  jpeg(paste0(substr(sta[i],20,22),'.jpeg'), width = 1024)  
  print(p)
dev.off()
  # p <-
  #   ggplot(selectByDate(aux,year=2018), aes(x=date, y=value, col=variable)) +
  #   geom_point() + geom_line() +
  #   theme(legend.position = 'bottom') +
  #   ggtitle(substr(sta[i],20,22))
  # 
  # print(p)
  # dev.off()
  }

