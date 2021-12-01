#
# Progama para organizar dados do PEFI
#
rm(list=ls())
library(openair)
library(stringr)

setwd()
#---- Funções ----
searchNewData <- function(path, pattern=NULL){
  if(is.null(pattern)){
    sfile <- list.files(path, full.names=T, recursive=T )  
  } else {
    sfile <- list.files(path, pattern, full.names=T, recursive=T )  
  }

  # arquivo Log
  if(file.exists(paste0(pattern,'_log.csv'))){
    lg <- readLines(paste0(pattern,'_log.csv'))
    return(sfile[!(sfile %in% lg)])
  } else {
    writeLines('# log',paste0(pattern,'_log.csv'))
    print('\n *** Primeiro rocessamento do sítio ***\n Log criado')
    return(sfile)
  }
}

check_ref <- function(min, max, by='30 min'){
  ref <- data.frame(date=seq.POSIXt(min, max, by))
  return(ref)
}

#---- Botânico ----
# dados novos
a <- searchNewData(path='data', pattern='TOA5_Bot')
if(length(a) == 0){
  cat('*** Nenhum arquivo novo a processar ***')
} else {
  # processando dados Botânico
  tt <- lapply(a, function(.file)readr::read_csv(.file, skip=4, na="NAN", 
                                                 col_names=F))
  cat('*** Existem ', length(a), ' arquivos novos a processar ***')
  # labels separado por periodos de datalogger
  # CR1000
  for(i in which(unlist(lapply(tt, ncol)) == 17) ){
    names(tt[[i]]) <- c("date","PTemp","Pira","batt","Dn","Dir","Dx","Sn","Vh","Sx","Tar",
                        "Tpainel","Ur","Press","Chuva_Acm","Vs","batt_Min")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  #CR300
  for(i in which(unlist(lapply(tt, ncol)) == 14) ){
    names(tt[[i]]) <- c("date", "RECORD", "batt", "Vh", "Dir", "Dir_SD", "Tar", "Ur",
                        "Press", "Chuva_Acm", "Vs", "TCG1", "Pyra_ref", "Pira")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  bot <- merge(tt[[1]], tt[[2]], all=T)
  for(i in 3:length(tt)){
    bot <- merge(bot, tt[[i]], all=T)
  }
  
  # atualizando o LOG
  write.table(a, paste0('TOA5_Bot','_log.csv'), append=T, col.names=F, row.names=F)
}

# juntar dados novos aos dados antigos
if(file.exists('out/botanico_all.csv')){
  old <- read.csv('out/botanico_all.csv')
  #merge
  #ref
} else {
  ref <- check_ref(min(bot$date), max(bot$date), by='1 min')
  bot1 <- merge(ref, unique(bot), all.x=T)
  bot1$diff <- c(NA, diff(bot1$date)
  
}








#---- Zoológico ----
# dados novos
a <- searchNewData(path='data', pattern='TOA5_Zoo')
if(length(a) == 0){
  cat('*** Nenhum arquivo novo a processar ***')
} else {
  # processando dados Botânico
  tt <- lapply(a, function(.file)readr::read_csv(.file, skip=4, na="NAN", 
                                                 col_names=F))
  cat('*** Existem ', length(a), ' arquivos novos a processar ***')
  
  # labels separado por periodos de datalogger
  # CR1000
  for(i in which(unlist(lapply(tt, ncol)) == 16) ){
    names(tt[[i]]) <- c("date","PTemp","batt","Dn","Dir","Dx","Sn","Vh","Sx","Tar",
                        "Tpainel","Ur","Press","Chuva_Acm","Vs","batt_Min")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  for(i in which(unlist(lapply(tt, ncol)) == 18) ){
    names(tt[[i]]) <- c("date","PTemp","batt","Pira_ref","Pira","Dn","Dir","Dx",
                        "Sn","Vh","Sx","Tar","Tpainel","Ur","Press","Chuva_Acm",
                        "Vs","bat_Min")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  #CR300
  for(i in which(unlist(lapply(tt, ncol)) == 14) ){
    names(tt[[i]]) <- c("date","RECORD","batt","Vh","Dir","Dir_SD","Tar",
                        "Ur","Press","Chuva_Acm","Vs","TCG2","Pira_ref","Pira")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  for(i in which(unlist(lapply(tt, ncol)) == 13) ){
    names(tt[[i]]) <- c("date","batt","Vh","Dir","Dir_SD","Tar",
                        "Ur","Press","Chuva_Acm","Vs","TCG2","Pira_ref","Pira")
    tt[[i]]$date <- as.POSIXct(tt[[i]]$date, format="%Y-%m-%d %H:%M%S", tz='GMT')
  }
  
  zoo <- merge(tt[[1]], tt[[2]], all=T)
  for(i in 3:length(tt)){
    zoo <- merge(zoo, tt[[i]], all=T)
  }
  
  # atualizando o LOG
  write.table(a, paste0('TOA5_Zoo','_log.csv'), append=T, col.names=F, row.names=F)
}



  ref <- data.frame(date=seq.POSIXt(min(b1$date), max(b1$date),by = '1 min'))

#---- Figuras ----
# antes de tudo plotar baterias para o Duda
bat <- merge(zoo[,c(1,3)], bot[,c(1,4)], by='date')
names(bat)[-1] <- c("Zoo","Bot")
final <- as.POSIXct(strftime(bat$date[nrow(bat)], format='%Y-%m-%d'), tz='GMT')
inicio <- final-(24*3600*60) # volta 60 dias
pdf('Baterias_2meses.pdf', width = 12)
timePlot(selectByDate(bat, inicio, final), names(bat)[-1], y.relation = 'free',
         main = paste('PEFI - Últimos 60 dias \n', inicio,' - ', final), ylab="Bateria(V)")
dev.off()
#


# calibração Tar e UR do Zoo
# Tcorr = 0.9919*TarZoo + 0.0841
# URcorr = 1.0127*URzoo + 1.7695

zoo$Tar_c = 0.9919 * zoo$Tar + 0.0841
zoo$Ur_c = 1.0127 * zoo$Ur + 1.7695

# conversão piranômetros:
zoo$Pira_ref_Wm <- zoo$Pira_ref * 14.771
zoo$Pira_Wm <- zoo$Pira * 15.151


#saida
# temperaturas, ur, vento do zoo.
out <- merge(bot[,c(1,11,13)], zoo[,c(1,19,20,5,8,13,14,21)], by="date")
#str(out)
names(out)[c(2:7,10)] <- c("Tar_Bot","UR_Bot","Tar_Zoo","UR_Zoo","wd","ws","Ki")

out10 <- timeAverage(out, avg.time = "10 min",vector.ws = TRUE)
out10$Chuva_Acm <- timeAverage(out, avg.time = "10 min",statistic = "sum")$Chuva_Acm
write.table(out, paste0(path,"/Dados_pefi.csv"), sep = ",", dec =".", row.names = FALSE, na="-9999.0")

# timePlot(zoo,names(zoo)[21:22], group=T)

#  pdf('pira_zoo.pdf', width = 12)
#  timePlot(zoo,names(zoo)[21:22], group=T, lty=1, ylab='W/m-2')
#  timePlot(zoo,names(zoo)[21:22], group=F, lty=1, ylab='W/m-2')
# scatterPlot(zoo, x='Pira_Wm',y='Pira_ref_Wm', pch=20, linear=T,ci.mod=T)
#  dev.off()
#Baterias
# pdf('baterias_pefi.pdf', width=12)
# timePlot(bot, names(bot)[c(4,16,17)], y.relation = "free",main="Botânico",
#          auto.text = FALSE)
# 
# timePlot(zoo, names(zoo)[c(3,15,16)], y.relation = "free",main="Zoológico",
#          auto.text = FALSE)
# dev.off()



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#--------------------------------------------------------------------
# 
#
#--------------------------------------------------------------------
rm(list=ls())

library(openair)


# dados botânico
path <- '/data1/DATA/LCB/PEFI'
a <- list.files(path, pattern='TOA5_50319', full.names=T,recursive=T)
tt <- lapply(a, function(.file)readr::read_csv(.file, skip=4, na="NAN", col_names=F))
bot <- do.call(rbind,tt)
names(bot) <- c("date","PTemp","Pira","batt","Dn","Dir","Dx","Sn","Vh","Sx","Tar",
                "Tpainel","Ur","Press","Chuva_Acm","Vs","batt_Min")
ref <- data.frame(date=seq.POSIXt(min(bot$date), max(bot$date),by = '1 min'))
bot <- merge(ref, bot, by="date", all.x=T)


# dados zoologico
labels <- c("date","PTemp","batt","Dn","Dir","Dx","Sn","Vh","Sx","Tar",
            "Tpainel","Ur","Press","Chuva_Acm","Vs","batt_Min")
b1 <- list.files(path, pattern='TOA5_60968', full.names=T,recursive=T)[1:2]
b1 <- lapply(b1, function(.file)readr::read_csv(.file, skip=4, na="NAN", 
                                                col_names=labels))
b1 <- do.call(rbind,b1)
b1 <- unique(b1)
ref <- data.frame(date=seq.POSIXt(min(b1$date), max(b1$date),by = '1 min'))
b1 <- merge(ref, b1, by="date", all.x=T)

labels <- c("date","PTemp","batt","Pira_ref","Pira","Dn","Dir","Dx","Sn","Vh","Sx",
            "Tar","Tpainel","Ur","Press","Chuva_Acm","Vs","batt_Min")
b2 <- list.files(path, pattern='TOA5_60968', full.names=T,recursive=T)[-c(1:2)]
b2 <- lapply(b2, function(.file)readr::read_csv(.file, skip=4, na="NAN", 
                                                col_names=labels))
b2 <- do.call(rbind,b2)
b2 <- unique(b2)
ref <- data.frame(date=seq.POSIXt(min(b2$date), max(b2$date),by = '1 min'))
b2 <- merge(ref, b2, by="date", all.x=T)
zoo <- merge(b1, b2, all=T)


# calibração Tar e UR do Zoo
# Tcorr = 0.9919*TarZoo + 0.0841
# URcorr = 1.0127*URzoo + 1.7695

zoo$Tar_c = 0.9919 * zoo$Tar + 0.0841
zoo$Ur_c = 1.0127 * zoo$Ur + 1.7695

# conversão piranômetros:
zoo$Pira_ref_Wm <- zoo$Pira_ref * 14.771
zoo$Pira_Wm <- zoo$Pira * 15.151


#saida
# temperaturas, ur, vento do zoo.
out <- merge(bot[,c(1,11,13)], zoo[,c(1,19,20,5,8,13,14,21)], by="date")
#str(out)
names(out)[c(2:7,10)] <- c("Tar_Bot","UR_Bot","Tar_Zoo","UR_Zoo","wd","ws","Ki")

# acertar o tempo para compatibilizar com os dados 
out10 <- timeAverage(out, avg.time = "10 min",vector.ws = TRUE)
out10$Chuva_Acm <- timeAverage(out, avg.time = "10 min",statistic = "sum")$Chuva_Acm
out10$date <- out10$date +10*60
write.table(out, paste0(path,"/Dados_pefi.csv"), sep = ",", dec =".", row.names = FALSE, na="-9999.0")

### teste da data - perfeito, como esperado
# tt <- out
# tt$mn <- floor(as.numeric(substr(tt$date,15,16))/10)*10
# tt$hr <- substr(tt$date,12,13)
# tt$dd <- substr(tt$date,9,10)
# tt$mm <- substr(tt$date,6,7)
# tt$yr <- substr(tt$date,1,4)
# library(doBy)
# t1 <- summaryBy(.~yr+mm+dd+hr+mn, data=tt)


