#---------------------------------------------------------#
# Organizar dados de Radiação Global Inciente    
#  Old script use ki_ext.R
#---------------------------------------------------------#

rm(list=ls())

library(readr)

rad <- read.csv('../../../2017/10817/EK/EXT_Rad_2m.csv', na.strings = '-9999.0')
rad$date <- as.POSIXct(rad$date, tz='GMT')

if(file.exists('../../data/Radiation/Ex_GlobalRad_inc.dat')){
    rad <- read.csv('../../data/Radls iation/Ex_GlobalRad_inc.dat')
    log <- read.table('globalRad.log')
}

newfiles <- list.files('../../..', '^EX_FSA',recursive = T, full.names = T)
newfiles <- newfiles[!newfiles %in% log$V1]

ndata <- lapply(newfiles, function(x)
            read.table(newfiles, header = F, sep=','))
ndata <- do.call(cbind, ndata)
names(ndata) <- c('id','ano','doy','hhmn','Pira_397','Pira_369','Tlog','VLog')
ndata$doy[nchar(ndata$doy)==1] <- paste0('00',ndata$doy[nchar(ndata$doy)==1])
ndata$doy[nchar(ndata$doy)==2] <- paste0('0',ndata$doy[nchar(ndata$doy)==2])
ndata <- data.frame(date=as.POSIXct(paste0(ndata$ano, ndata$doy, ndata$hhmn),
                                    format='%Y %j %H%M', tz='GMT'), ndata)
ref <- data.frame(date=seq.POSIXt(ndata$date[1], ndata$date[nrow(ndata)], by = '2 min'))
    
# merge
# merge com dados antigos

write.table(newfiles, 'globalRad.log', append = T, row.names = F, col.names = F)



