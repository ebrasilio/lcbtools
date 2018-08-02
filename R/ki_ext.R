#----------------------------------------------------------------
# Read process file and check for new updates
# If hasn't finish
#----------------------------------------------------------------

rm(list=ls())

path = '/data1/DATA/LCB/EXT'
old <- readr::read_csv(paste0(path, '/Ki_data/EXT_Rad_2m.csv'), na='-9999.0')

last <- strftime(old$date[nrow(old)], format = "%Y-%m-%d")
files <- list.files(path, pattern = 'EX_FSArea1.dat', recursive = T)

dd <- strftime(as.POSIXct(unlist(stringr::str_split_fixed(files, '/',4))[,2],
                 format="%j%y"), format="%Y-%m-%d")

if(length(files[which(dd > last)]) > 0){
  files <- lapply(dd, function(.file){
                  read.csv(paste0(path,'/', .file), header=F)})
  files <- do.call(rbind, files)
  names(files) <- c("id","year","doy","hhmm","Pira_397","Pira_369","Tlogger","LoggerV")
  files$doy[nchar(files$doy) == 1] <- paste0('00',files$doy[nchar(files$doy) == 1])
  files$doy[nchar(files$doy) == 2] <- paste0('0',files$doy[nchar(files$doy) == 2])

  files$hhmm[nchar(files$hhmm) == 1] <- paste0('000',files$hhmm[nchar(files$hhmm) == 1])
  files$hhmm[nchar(files$hhmm) == 2] <- paste0('00',files$hhmm[nchar(files$hhmm) == 2])
  files$hhmm[nchar(files$hhmm) == 3] <- paste0('0',files$hhmm[nchar(files$hhmm) == 3])

  files <- data.frame(date=as.POSIXct(paste(files$year, files$doy, files$hhmm), 
                                      format = "%Y %j %H%M", tz = "GMT"), files)
  new <- data.frame(rbind(old, files))
  ref <- data.frame(date = seq.POSIXt(min(new$date), max(new$date), by='2 min'))
  new <- merge(ref, new, all.x = T)
  write.csv(new, file=paste0(path,'/Ki_data/EXT_Rad_2m.csv'),  row.names=F, na = "-9999.0")

  outh <- timeAverage(new, avg.time = "hour")
  write.csv(outh, file=paste0(path,'/Ki_data/EXT_Rad_hora.csv'),  row.names=F, na = "-9999.0")

  outdy <- timeAverage(new, avg.time = "day")
  write.csv(outdy, file=paste0(path,'/Ki_data/EXT_Rad_dia24h.csv'),  row.names=F, na = "-9999.0")


  system(paste0('rsync -Cravpz ', path, '/Ki_data/EXT_Rad* ', path, '/STORAGE/data/Radiation/'))

}else{
  stop("*** No New Data to Process ***")
}




