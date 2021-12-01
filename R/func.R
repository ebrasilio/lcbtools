library(openair)
library(imputeTS)


#-------------------------------------------------------------------------------------------------
count_na_day <- function(mydata){
  dy = unique(substr(mydata$date,1,10))
  fail <- list()
  for(i in 1:length(dy)){
    aux <- selectByDate(mydata, dy[i], dy[i])
    fail[[i]] <- as.vector(apply(aux, 2, function(x){sum(is.na(x))}))
    do.call(cbind, fail)
  }
  fail <- data.frame(do.call(rbind, fail))
  names(fail) <- names(mydata)
  fail$date <- as.POSIXct(dy, tz = 'GMT')
  return(fail)
}
#---------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
fill_gap <- function(mydata){
  # menos que 10 gaps usar regressão linear
  # mais de 10 gaps replicar dado do dia anterior
  gap <- count_na_day(mydata)
  aux <- mydata
  
  for(i in 2:ncol(aux)){
    #cat(i,'\n')
    if(sum(gap[,i]) != 0 & (sum(gap[,i]) < (nrow(gap)*24))){
      aux[,i] <- as.vector(na.interpolation(dt[,i], option='spline'))
    }
  }
  return(aux)
}
#--------------------------------------------------------------------------------------- 

hh <- stringr::str_split_fixed(
  readLines('eddypro_18116_biomet_2019-11-12T121215_adv.csv',1), pattern = ',', 21)
a <- read.csv('eddypro_18116_biomet_2019-11-12T121215_adv.csv',skip=2, header=F,
              na.strings = '-9999', stringsAsFactors = F)
names(a) <- hh

dt <- data.frame(date=as.POSIXct(paste(a$date, a$time), format='%Y-%m-%d %H:%M', tz = 'GMT'),
                 a[,-c(1:2)])

new <- fill_gap(dt)
dt.out <- new
TIMESTAMP_1 <- substr(dt.out$date,1,10)
TIMESTAMP_2 <- paste0(substr(dt.out$date,12,13),substr(dt.out$date,15,16))
dt.out <- data.frame(TIMESTAMP_1, TIMESTAMP_2, dt.out[,-c(1)])
names(dt.out) <- c('Timestamp_1','Timestamp_2','Ta','Pa','RH','Rr','Rg',
                   #'Timestamp_1,Timestamp_2,  Ta,   Pa, RH,   Rr,  Rg,
                   #'yyyy-mm-dd, HHMM,         K,    Pa,  %,  W+1m-2,W+1m-2,
                   'Rn','PPFD', 'PPFDr',
                   #Rn, PPFD,    PPFDr,
                   #W+1m-2,umol+1m-2s-1,umol+1m-2s-1,
                   'P_rain','WS','WD','SHF','WF','H_WEBB','LE_WEBB',
                   #P_rain, MWS,  WD,      SHF,    WF,     H_WEBB, LE_WEBB,
                   #mm,    ms-1,  degrees, W+1m-2, m+3s-1, W+1m-2, W+1m-2, 
                   'FC_WEBB','Ts','SWC')
#                   FC_WEBB,  Ts,   SWC'))
#                   umol+1m-2s-1, K, m+3m-3'))
# ajuste de unidades
# dt.out$Ta <- dt.out$Ta + 273.15
# dt.out$Pa <- dt.out$Pa * 1000
# dt.out$Ts <- dt.out$Ts + 273.15

label1 <- data.frame(lab=c('Timestamp_1,Timestamp_2,Ta,Pa,RH,Rr,Rg,Rn,PPFD,PPFDr,P_rain,MWS,WD,SHF,WF,H_WEBB,LE_WEBB,FC_WEBB,Ts,SWC'))
label2 <- data.frame(lab=c('yyyy-mm-dd,HHMM,K,Pa,%,W+1m-2,W+1m-2,W+1m-2,umol+1m-2s-1,umol+1m-2s-1,mm,ms-1,degrees,W+1m-2,m+3s-1,W+1m-2, W+1m-2,umol+1m-2s-1, K, m+3m-3'))
out <- getwd()
write.table(label1, paste0(out,'/eddypro_18116_biomet_2019-11-12T121215_adv.csv'), append=FALSE,
            quote=FALSE,row.names=FALSE,col.names=FALSE)
write.table(label2,paste0(out,'/eddypro_18116_biomet_2019-11-12T121215_adv.csv'), append=TRUE, 
            quote=FALSE,col.names=FALSE,row.names=FALSE)
write.table(dt.out,paste0(out,'/eddypro_18116_biomet_2019-11-12T121215_adv.csv'),append=TRUE,
            quote=FALSE,sep=',',col.names = FALSE, row.names = FALSE, na = '-9999')

