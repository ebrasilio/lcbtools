# concatenar ano corrente
rm(list=ls())

#setwd('/data3/EXT/codes/')
setwd('~/lcb_2021/EXT/codes/')

# ---- Settings ----
library(stringr)
library(openair)
library(ggplot2)
library(reshape2)

source('climaFunctions.R')

#year <- 2018

for(year in 2014:2023){ #2020:2023){
  cat('*******  --> ', year, '\n')
  repo <- paste0('../data','/',year)
  
  #---- Checking Stations with New Data ----
  sta <- substr(list.dirs(repo), nchar(list.dirs(repo))-2, nchar(list.dirs(repo)))
  sta <- sta[substr(sta,1,1) == "C"]

  pdf(paste0('origin_',year,".pdf"), width=12)
  #---- Process  ----
  for(i in 1:length(sta)){
  # if(year == 2017)  sta <- sta[-18]
    cat("\n\nProcessando estação: ", sta[i],'\n')
    new <- checkNewData(repo, sta[i])
    dt <- new_dt(new, sta[i])
    if(!is.null(dt)){
      out <- checkRange(new, sta[i])
      # ..... LOGS.....
      write.table(sta[i], paste0('log_',year,'.txt'),  row.names = F, append=T)
      write.table(out, paste0('log_',year,'.txt'), row.names = F, append=T)
      timePlot(dt, names(dt[-c(1:3)]), y.relation = 'free', main=sta[i])
      # ..... print .....
      printOuts(dt, repo, sta[i])
    }
  }
  dev.off()
}
# .... Checando arquivo out Clima 

for(i in 1:length(sta)){
  cat ('\n',' ', sta[i], '\n')  #'../outExt/Clima',sta[i]
  if(file.exists( paste0('../outExt/Clima/', sta[i], 'clear_merge.txt')) ){
    t <- read.csv(paste0('../outExt/Clima/', sta[i], 'clear_merge.txt') )
    t$date <- as.POSIXct(t$date, tz='GMT')
    t$diff <- c(0, difftime(t$date[1:(nrow(t)-1)], t$date[2:nrow(t)]))
    pos <- which( t$diff != -2 ) 
    if(length(pos) > 1){
      cat("\n **** PROBLEMAS *****")
    }
  }
}

#-- ---------------------------------------------------------------------


sta <- c('C02','C03','C04','C05','C06','C07','C08','C09','C10','C11','C12',
         'C13','C14','C15','C16','C17','C18','C19','C20','C21','C22','C23',
         'C24','C25','C26','C27')

for(i in 1:length(sta)){
  f1 <- list.files('../data', pattern=paste0(sta[i],'clear_merge.txt'),  
                   recursive=T, full.names=T)
  test.data <- lapply(f1, 
                      function(.file) 
                        read.csv(.file, na.strings = c("-9999", "NA")))
  a <- do.call(rbind, test.data)
  a$date <- as.POSIXct(a$date, tz='GMT')
  
  min <- min(a$date)
  max <- max(a$date)
  
  ref <- check_ref(min, max, by='2 min')
  b <- merge(ref, a, by='date', all.x=TRUE)
  
  printOuts(b, '../outExt/Clima',sta[i])
}



#pdf('plot_allsta.pdf', width = 12)
for(i in 1:length(sta)){
  aux <- read.csv(paste0('../outExt/Clima/', sta[i], 'clear_merge.txt'), 
                  na.strings=c(NA, '-9999'), stringsAsFactors=FALSE)
  aux$date <- as.POSIXct(aux$date, tz="GMT")
 
  jpeg(paste0(sta[i],"_alldate.jpeg"), width = 960, height = 480 ) 
    timePlot(aux, names(aux)[5:8], y.relation='free', pch=c(20,20,20,20), 
             main = paste(sta[i], '\n', substr(aux$date[1],1,10),'-', 
                          substr(aux$date[nrow(aux)],1,10)),ylab="")
  dev.off()
  cat('plot ', sta[i], '\n')
}
#dev.off()


stt <- sta[c(2,4,6,12,20,23,25)]
ll <- list()
for( i in 1:length(stt) ){
  aux <- read.csv(paste0('../outExt/Clima/', stt[i], 'clear_merge.txt'), 
                  na.strings=c(NA, '-9999'), stringsAsFactors=FALSE)
  aux$date <- as.POSIXct(aux$date, tz="GMT")
  names(aux)[5] <- paste0(names(aux)[5],'_',stt[i])
  ll[[i]] <- aux[,c(1,5)] 
}

lapply(ll, head)

aux <- merge(ll[[1]], ll[[2]], all=T )
aux <- merge(aux, ll[[3]], all=T)
aux <- merge(aux, ll[[4]], all=T)
aux <- merge(aux, ll[[5]], all=T)
aux <- merge(aux, ll[[6]], all=T)
aux <- merge(aux, ll[[7]], all=T)

aa <- unique(aux)

timePlot(#aa, 
  selectByDate(aa,year=2021:2023),
  #pch=rep(20,7), 
         names(aa)[-1], group = F, ylab="", 
         date.format =  "%b-%Y", lwd=1.5)










