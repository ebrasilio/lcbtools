# perfil2
# 

library(stringr)
library(openair)
library(reshape2)
library(ggplot2)

setwd('~/lcb_2021/EXT/codes/')
in_files <- '~/lcb_2021/EXT/data/'
out <- '~/lcb_2021/EXT/outExt/'
source('functions-profile.R')   

year <- 2021:2023
wxt1 <- list()
wxt2 <- list()
wxt3 <- list()
wxt4 <- list()


for(i in 1:length(year)){
  inp <- list.files(paste0(in_files, year[i]), recursive=T, pattern="TOA5_EP_", full.names = T)
  inp1 <- inp[ which(!is.na(str_match(string = inp, pattern = 'WXT_1'))) ]
  inp2 <- inp[ which(!is.na(str_match(string = inp, pattern = 'WXT_2'))) ]
  inp3 <- inp[ which(!is.na(str_match(string = inp, pattern = 'WXT_3'))) ]
  inp4 <- inp[ which(!is.na(str_match(string = inp, pattern = 'WXT_4'))) ]
  # wxt1
  wxt1_a    <- unique(read_wxt(inp1, 1))
  ref       <- data.frame(date = seq.POSIXt(min(wxt1_a$date), max(wxt1_a$date), by='1 min'))
  wxt1[[i]] <- merge(ref, wxt1_a, all.x=T)
  # wxt_2
  wxt2_a <- unique(read_wxt(inp2, 2))
  ref    <- data.frame(date = seq.POSIXt(min(wxt2_a$date), max(wxt2_a$date), by='1 min'))
  wxt2[[i]]   <- merge(ref, wxt2_a, all.x=T)
  # wxt_3
  wxt3_a <- unique(read_wxt(inp3, 3))
  ref    <- data.frame(date = seq.POSIXt(min(wxt3_a$date), max(wxt3_a$date), by='1 min'))
  wxt3[[i]]   <- merge(ref, wxt3_a, all.x=T)
  #wxt4_a
  wxt4_a <- unique(read_wxt(inp4, 4))
  ref    <- data.frame(date = seq.POSIXt(min(wxt3_a$date), max(wxt3_a$date), by='1 min'))
  wxt4[[i]]   <- merge(ref, wxt4_a, all.x=T)
}


wxt1 <- do.call(rbind, wxt1)
wxt2 <- do.call(rbind, wxt2)
wxt3 <- do.call(rbind, wxt3)
wxt4 <- do.call(rbind, wxt4)

# write.csv(wxt1, paste0(out, 'Extrema_profile1_Torre2.csv'), row.names=F, quote=T)
# write.csv(wxt2, paste0(out, 'Extrema_profile2_Torre2.csv'), row.names=F, quote=T)
# write.csv(wxt3, paste0(out, 'Extrema_profile3_Torre2.csv'), row.names=F, quote=T)
# write.csv(wxt4, paste0(out, 'Extrema_profile4_Torre2.csv'), row.names=F, quote=T)


var <- c("Ta","Ua","Pa")
for(i in 1:length(var)){
  vars1 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars2 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars3 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars4 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  
  mydt <- merge(wxt1[,vars1], wxt2[,vars2])
  mydt <- merge(mydt, wxt3[,vars3])
  mydt <- merge(mydt, wxt4[,vars4])
  
  #mydt <- selectByDate(mydt,'2017-08-31', Sys.Date())
  
  # write.csv(mydt, paste0(out,var[i],'_Extrema_Perfil-Torre2_1min.csv'),
  #           row.names = F, quote = T)
  mydt_10m <- timeAverage(mydt, avg.time='10 min', data.thresh=0, 
                          start.date=paste0(substr(mydt$date[1],1,14),'00:00'))
  mydt_10m$date <- mydt_10m$date + 10*60
  
  
  # write.csv(mydt_10m, paste0(out,var[i],'_Extrema_Perfil-Torre2_10min.csv'),
  #           row.names = F, quote = T)
}

timePlot(mydt_10m, names(mydt_10m)[-1], main='Perfil1 \n 2021-2022', ylab="", 
         y.relation = 'free', date.format = "%b-%Y")




