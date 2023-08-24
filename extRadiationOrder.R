#..................................................................................
# Input RScript RadExt TOA5_EP_yyddd.dat ==> para arquivo de radiação da torre 1
#
#..................................................................................

rm(list=ls())

library(openair)
library(stringi)
library(stringr)

#--- Inicio ----
args <- commandArgs(TRUE)
if (length(args) == 0) {
  stop("Informar o arquivo de entrada", call.=FALSE)
} else if (length(args)==1) {
  inpFile <- args[1]
}
cat(paste0('\n arquivo a processar, ', inpFile, '\n'))

#inpFile <- 'data/2021/21196/EP/TOA5_EP_21196.dat'
#---- identificar arquivo de entrada ---
#---- Torre 1: SHF ----
#==> só tem dados de shf
if( substr(inpFile, nchar(inpFile)-12, nchar(inpFile)-9) == '_EP_' )
{
  cat('Sistema Perfil 1 - Dados de calor no solo \n') 
  new <- read.csv(inpFile, header=F, skip=4, na.strings='NAN', stringsAsFactors=F)
  #new <- read.csv(paste0('data/2021/',inpFile), header=F, skip=4, na.strings='NAN', stringsAsFactors=F)
  new$V1 <- as.POSIXct(new$V1, tz='GMT')
  new$G <- apply(new[,c(4:6)],1,function(x)mean(x, na.rm=T))
  old <- read.csv('outExt/Radiação_torre1/extrema_shf_Torre1_15min.csv', header=T,
                  na.strings='-9999.0000', stringsAsFactors=F)
  old$date <- as.POSIXct(old$date, tz='GMT')
  old$G <-  apply(old[,c(4:6)],1,function(x)mean(x, na.rm=T))
  pos <- which(apply(old[,2:6], 1, function(x)sum(is.na(x))==5))
  old <- old[-pos, ]
  
  if(ncol(new) == ncol(old))
  {
    cat('\n Numero de colunas compatível, iniciando a concatenação de arquivos \n')
    names(new) <- names(old)
    aux <- unique(merge(old, new, all=T))
    ref <- data.frame(date= seq.POSIXt(from=min(aux$date), to=max(aux$date), by='15 min'))
    aux <- merge(ref, aux, by='date', all.x=T)
  }
  
  if(dim(aux)[1] == dim(ref)[1])
  {
    cat("\n Merge realizado corretamente \n")
  }
  
  aux30 <- timeAverage(aux, avg.time='30 min', start.date=paste0(substr(aux$date[1], 1,14),'00:00'))
  out <-   data.frame(date=aux$date, apply(aux[,-1], 2, function(x)round(x,4)))
  out30 <- data.frame(date=aux30$date, apply(aux30[,-1], 2, function(x)round(x,4)))
  write.csv(out,   'outExt/Radiação_torre1/extrema_shf_Torre1_15min.csv', row.names = F)
  write.csv(out30, 'outExt/Radiação_torre1/extrema_shf_Torre1_30min.csv', row.names = F)
  
  
  #---- Torre 2: RAD-SHF ----
  # ==> Radiação e dados de solo
} else if( substr(inpFile, nchar(inpFile)-12,nchar(inpFile)-9) == 'EP2_' )
{
  cat('Sistema Perfil 2\n')
  new <- read.csv(inpFile, header=F, skip=4, na.strings='NAN', stringsAsFactors=F)
  # new <-  read.csv(' data/2021/21196/EP_2/TOA5_EP2_21196.dat', header=F, skip=4, na.strings=c('NAN',"NA), stringsAsFactors=F)
  new$V1 <- as.POSIXct(new$V1, tz='GMT')
  new$G <-  apply(new[,c(4:6)],1,function(x)mean(x, na.rm=T))
  
  old <- read.csv('outExt/Radiação_torre2/extrema_rad_Torre2_15min.csv', header=T,
                  na.strings=c('-9999.0000','NA','NAN'), stringsAsFactors=F)
  old$date <- as.POSIXct(old$date, tz='GMT')
  old$G <-  apply(old[,c(4:6)],1,function(x)mean(x, na.rm=T))
  pos <- which(apply(old[,2:11], 1, function(x)sum(is.na(x)) ==11))
  
  if(length(pos) >0 )old <- old[-pos, ]
  
  if(ncol(new) == ncol(old))
  {
    cat('\n Numero de colunas compatível, iniciando a concatenação de arquivos \n')
    names(new) <- names(old)
    aux <- unique(merge(old, new, all=T))
    ref <- data.frame(date= seq.POSIXt(from=min(aux$date), to=max(aux$date), by='15 min'))
    aux <- merge(ref, aux, by='date', all.x=T)
  }
  if(dim(aux)[1] == dim(ref)[1])
  {
    cat("\n Merge realizado corretamente \n")
  }
  aux30 <- timeAverage(aux, avg.time='30 min')
  out <-   data.frame(date=aux$date, apply(aux[,-1], 2, function(x)round(x,4)))
  out30 <- data.frame(date=aux30$date, apply(aux30[,-1], 2, function(x)round(x,4)))
  write.csv(out, 'outExt/Radiação_torre2/extrema_rad_Torre2_15min.csv', row.names = F)
  write.csv(out30, 'outExt/Radiação_torre2/extrema_rad_Torre2_30min.csv', row.names = F)
} else
{
  cat('Nenhum sistema a detectar')
}









#---------------------------------------------------------------------------------------------
# Ki 
# arquivos já organizados - 31/01/2018

# ki_old  <- read.csv('/data1/DATA/LCB/EXT/Ki_data/EXT_Rad_2m.csv', na.strings = '-9999.0')
# ki_old$date <- as.POSIXct(ki_old$date, tz="GMT")
# names(ki_old)[6] <- 'Pyra_I_Avg'

#..................................................................................
# Constinuação dos dados
# inp <- list.files('/data1/DATA/LCB/EXT/data', pattern='EX_FSArea1.dat', 
#                   recursive=T, full.names=T)[-c(1:4)]
# aux = lapply(inp, function(.file)  read.csv(.file, header=F))
# aux <- do.call(rbind, aux)
# names(aux) <- c('id','year','doy','hhmm','Pyra_I_Avg','Pira_369','Tlogger','LoggerV')
# 
# aux$hhmm[nchar(aux$hhmm)==1] <- paste0('000',aux$hhmm[nchar(aux$hhmm)==1])
# aux$hhmm[nchar(aux$hhmm)==2] <- paste0('00',aux$hhmm[nchar(aux$hhmm)==2])
# aux$hhmm[nchar(aux$hhmm)==3] <- paste0('0',aux$hhmm[nchar(aux$hhmm)==3])
# 
# aux <- data.frame( date = as.POSIXlt(paste(aux$year, aux$doy, aux$hhmm), 
#                                    format="%Y %j %H%M", tz ="GMT"), aux)
# ki <- merge(ki_old, aux, all=T)
# 
# # 397 tem menos problema mas no envelope superior ambos são muito parecidos
# ki_10min <- timeAverage(ki, avg.time = '10 min', data.thresh=0,
#               start.date = paste0(substr(ki_old$date[1],1,14), '00:00'))
# ki_10min$date <- ki_10min$date + 10*60 # ajuste
# ref <- data.frame(date = seq.POSIXt(ki_10min$date[1], ki_10min$date[nrow(ki_10min)], by='10 min'))
# 
# 
# ki <- merge(ref, ki_10min, all.x=T)


#timePlot(ki, names(ki)[5])


#---------------------------------------------------------------------------------------------
# Net até 2019-115, dados junto com fluxos médios 
# Dados já processados:
# net_old <- read.csv('/data1/DATA/LCB/EXT/STORAGE/data/I/bk/Ext_Radiations_raw.csv',
#                     na.strings = '-9999')
# net_old$date <- as.POSIXct(net_old$date, tz='GMT')

#----------------------------------------
# Corrigir dados com multiplicador errado
# entre nov/2016 e abril/2018
# ref <- data.frame(date =
#             seq.POSIXt(net_old$date[1], as.POSIXct('2017-03-29 13:30:00', tz="GMT"), by='30 min'))
# pos <- which(net_old$date %in% ref$date)
# net_old$Net_Avg[pos] <- (net_old$Net_Avg[pos] -77.5194) * 77.5194

# timePlot(selectByDate(net_old, '2016-12-01','2017-03-28'), names(net_old)[2], pch = 20)


# Verificar se há dados novos a processar

#=========================================================================================
#---- Torre 1 ----
#SHF

#raw_data <- "/data1/DATA/LCB/EXT/data"
# inpD <- '/media/emilia/Seagate Expansion Drive/Ext_2020/data/2021'
# 
# # flux novo arquivo 2018 e 2019
# #inp <- list.files(raw_data, pattern = "TOA5_XF", recursive = T, full.names = T)
# inp <- list.files(inpD, pattern = "TOA5_EP_", recursive = T, full.names = T)
# inp <- inp[!str_detect(inp, pattern='WXT')]
# 
# #flux1 <- flux1[!(substr(flux1, 26, 29) %in% c("2014","2015","2016", "2017"))]
# dir_out <- '/data1/DATA/LCB/EXT/STORAGE/data/I'
# 
# 
# # falta colocar a condicional aqui:
# # Lendo arquivo antigo e concatenando colet anova
# cat("\n *** Lendo arquivo antigo torre 1 ***")
# old <- read.csv(paste0(dir_out, '/Ext_Radiation_10min.csv'), header=T, na.strings='-9999', stringsAsFactors=F)
# old$date <- as.POSIXct(old$date, tz='GMT')
# last <-old$date[nrow(old)]
# # verificar apenas arquivos deste ano e do ano anterio
# # tt <- as.POSIXct(substr(inp, nchar(inp)-11, nchar(inp)-7), format='%y%j', tz='GMT')
# # tt[1:(length(tt)-5)] <- NA
# # nn <- inp[which(last<tt)]
# 
# tt <- as.POSIXct(substr(inp, 31,35), format="%y%j", tz="GMT")
# nn <- inp[tt > last & substr(tt,1,4) == '2020' & !is.na(tt > last)]
# 
# if(length(nn) == 0){
#   cat("\n *** Última coleta em :", as.character(last), '***')
#   cat("\n *** Nada para processar da Torre 1 ***\n")
# return()
# }else{
#   dt <- lapply(nn, function(.file){
#         read.csv(.file, skip=4, na.strings =c('NAN'),
#         stringsAsFactors = F, header=F)})
#   flx <- do.call(rbind,dt)
#   flx <- data.frame(date=as.POSIXct(flx$V1, tz = "GMT"), flx[,c(2,5,6,3,4)])
#   aux <- flx[order(flx$date),]
#   flx2 <- timeAverage(aux, avg.time='1 min', start.date=paste0(substr(min(aux$date),1,14),'00:00'))
#   flx2 <- timeAverage(flx2, avg.time='5 min', start.date=paste0(substr(min(flx2$date),1,14),'00:00'))
#   dt <- flx2
#   names(dt) <- names(old)
#   ref <- data.frame(date = seq.POSIXt( min(old$date), max(dt$date), by='5 min'))
#   old <- rbind(old, dt) # dado old + dado novo
#   dt <- merge(ref, old, all.x=TRUE)
# }
# 
# 
# 
# 
# # dt est em media de 10 min
# write.csv(dt, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiation_10m.csv", 
#           row.names = F, quote = T)
# 
# dt30min <- timeAverage(dt,data.thresh=0, avg.time='30 min') 
# dt30min$date <- dt30min$date + 30*60
# out <- data.frame(date=dt30min$date, apply(dt30min[,-1], 2, function(x)round(x,4)))
# 
# write.csv(out, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiation_30m.csv", 
#           row.names = F, quote = T, na='NA')
# 
# 
# # #---------------------------------------------------------------------------------------------
# # # lista de arquivos novos a sincronizar
# # 
# # #if(length(flux) > 0){
# #     #a <- c("\"TIMESTAMP","Net_Avg","Par_I_Avg","Pyra_I_Avg","Pyra_R_Avg","BattV_Avg","PTemp_C_Avg\"" )
# #     # informação das colunas
# #     # labels_tt <- lapply(flux1,
# #     #                     function(.file)
# #     #                         #which(
# #     #                           unlist(stri_split_fixed(readLines(.file, n=2)[2], '\",\"')))#)
# #     #                               #%in% a))
# #     
# #     # dados e renomeação das colunas
# #     # tt = lapply(flux1,
# #     #             function(.file)
# #     #                 read.csv(.file,skip=4, na.strings='\"NAN\"', header=F))
# #     # 
# #     # # for (i in 1:length(tt)){
# #     #     names(tt[[i]]) <- unlist(stri_split_fixed(readLines(flux1[[i]], n=2)[2], '\",\"'))
# #     # }
# # 
# # # dados a separar - 
# # # TODO INCLUIR UM IF PARA NCOL == 4
# #     colunas <-  unique(unlist(lapply(tt, ncol)))[order(unique(unlist(lapply(tt, ncol))))]
# #     
# #     saida <- list()
# #     for(l in 1:length(colunas)){
# #       pos <- which(unlist(lapply(tt, ncol)) == colunas[l])
# #       aux <- list()
# #         for(m in pos){
# #           aux[[m]] <- tt[[m]]
# #         }
# #     saida[[l]] <- do.call(rbind, aux)  
# #     if(l == 1){
# #       names(saida[[l]]) <- c("TIMESTAMP","Net_Avg","BattV_Avg","PTemp_C_Avg")
# #     }else if(l == 2){
# #       names(saida[[l]]) <- c("TIMESTAMP","Net_Avg","Par_I_Avg","Par_R_Avg","Pyra_I_Avg","Pyra_R_Avg",
# #                              "BattV_Avg","PTemp_C_Avg")
# #     }else if(l == 3){
# #       names(saida[[l]])[c(1,85)] <- c("TIMESTAMP","Net_Avg")
# #     }else if(l == 4){
# #       names(saida[[l]])[c(1,86)] <- c("TIMESTAMP","Net_Avg")
# #     }else{
# #       cat("** Arquivo novo **")
# #     }
# #     }
# #     
# # #        
# #     net <- merge(data.frame(saida[[1]]), data.frame(saida[[2]]), all=T)
# #     net <- merge(net, data.frame(saida[[3]][,c(1,85)]), all=T)
# #     net <- merge(net, data.frame(saida[[4]][,c(1,86)]), all=T)
# # 
# #     names(net)[1] <- 'date'    
# #     net$date <- as.POSIXct(net$date, tz='GMT') 
# #     ref <- data.frame(date = seq.POSIXt(min(net$date), max(net$date), by='10 min'))
# #     net2 <- merge(ref, net, all.x=T)   
# #     
# #     #----------------------------------------
# #     # Corrigir dados com multiplicador erraPyra_I_Avgdo
# #     # entre nov/2016 e abril/2018
# #     ref <- data.frame(date =
# #                 seq.POSIXt(net2$date[1], as.POSIXct('2017-03-29 13:30:00', tz="GMT"), by='30 min'))
# #     pos <- which(net2$date %in% ref$date)
# #     net2$Net_Avg[pos] <- (net2$Net_Avg[pos] -77.5194) * 77.5194
# #     
# #     # juntar dados de net com ki antigo
# #     # merge só em Ki
# #     aux <- merge(net2, ki[,c(1,5)], all=T) #duplica linhas
# #     aux <- merge(net2, ki[,c(1,5)],by='date', all=T) # não duplica, mas não dá merge no ki
# #     aux$Pyra_I_Avg = aux$Pyra_I_Avg.y
# #     aux$Pyra_I_Avg[which(!is.na(aux$Pyra_I_Avg.x))] <- aux$Pyra_I_Avg.x[which(!is.na(aux$Pyra_I_Avg.x))]
# #     
# #     rad_10m <- aux[,c(1,2,10,8,5,6)]
# #     #timePlot(aux, names(aux)[c(2,7,9)], y.relation='free')
# #     
# #     
# #     names(out)[-1] <- c('Net', 'Ki', 'Kr', 'Par_i', 'Par_r')
# #     write.csv(out, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiation_10min.csv", 
# #              row.names = F, quote = T, na='-9999')
#     
#     # out30 <- timeAverage(out, avg.time = '30 min', data.thresh = 0,
#     #                      start.date = paste0(substr(out$date[1],1,14), '00:00'))
#     # out30$date <- out30$date + 30*60
#     # 
#     # write.csv(out30, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiation_30m.csv", 
#     #           row.names = F, quote = T, na='-9999')
#     # 
#      
#     
#     
#     
#     
#     
#     
#     
# 
