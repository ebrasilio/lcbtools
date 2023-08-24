#------------------------------------------------------------------------------------------------------------
# LCB - 31/05/2017 - Emilia M S Brasilio                                
                                           
# 1. Leitura do arquivo anterior (caso exista)                          
# 2. ler dados na pasta raw # RE melhorar                               
#-----------------------------------------------------------------------------------------------------------
#rm(list=ls())

#library(openair)
library(stringr)
#library(dplyr)

# paths
old <- '/home/emilia/lcb_2021/EXT/outExt/Umidade_Solo/'
inp <- '/home/emilia/lcb_2021/EXT/data/'
# infiles <- "/data1/DATA/LCB/EXT/origin"
# merge   <- "/data1/DATA/LCB/EXT/STORAGE/data/H/merge"
# temp    <- "/data1/DATA/LCB/EXT/STORAGE/data/H/temp"
source('climaFunctions.R')


year <- 2022
inpf <- list.files(paste0(inp, year), pattern = 'H')
sta <- unique(substr(inpf, 1, 3) [substr(inpf, 1, 1) == 'H'])

for( i in 1:length(sta)){
  cat('\n Processando: ', sta[i], '\n')
  
  # ler todos os arquivos novos da estação
  files <- list.files(paste0(inp, year, '/', sta[i]), full.names = T) 
  dt <- lapply(files, function(x) readLines(x))
  
  label <- c("date","Bat.mV","S_10cm","S_20cm","S_30cm","S_40cm","S_60cm","S_100cm")
  dt <- lapply(dt, function(x){
    x <- x[!str_detect(x, '[:alpha:]')]
    x <- x[ str_count(x, ',') == 7 ]
      if(length(x) == 1)x <- c(x,x)
      x <- data.frame(str_split_fixed(x, ",", 8))
      x <- data.frame(date = x$X1, apply(x[,-1], 2, as.numeric))
  })

  dt1 <- plyr::ldply(Filter(NROW, dt), rbind)
  
  if(nrow(dt1) == 0){
    cat('\n Arquivo(s) sem dados válidos \n')
  } else {
    if( length(label) != ncol(dt1)) {
      stop(cat('\n Numero de labels e coluna não conferem !! \n'))
    } else {
      names(dt1) <- label 
      
      # acertar formato da data
      aux <- matrix(unlist(strsplit(substr(dt1$date,1,10), "-")), ncol=3,byrow = T)
      if(unique(nchar(aux))[1] == 2 & unique(nchar(aux))[3] == 4){
        fmt = "%d-%m-%Y %H:%M"
      } else {
        fmt = "%Y-%m-%d %H:%M"
      }
       dt1$date <- as.POSIXct(dt1$date, format=fmt, tz='GMT')
     
      # concatenar, limpar, escrever
      ref <- check_ref(dt1$date[1], dt1$date[nrow(dt1)], '5 min')
      dt2 <- unique(merge(ref, dt1, by.x = TRUE) )
      write.table(dt2, paste0(inp, year, '/', sta[i], 'clear_merge.txt'), row.names = F)
    }
  }
}

 
# Ler lista de arquivos da base
# filesToProcess = checkFiles(
#     list.files(infiles, pattern = "^H", recursive = T,  full.names = T))

# Verificar log de arquivos já processados
# if(!file.exists("Log_raw_data")){
#   cat("Log não encontrado, processando todos os dados\n")
#   lista = as.character(filesToProcess)
# }else{
#   cat("Log encontrado, verificando se existem arquivos novos a processar\n\n")
#    logRaw <- read.csv("Log_raw_data", header = F, stringsAsFactors = F)
#    fil <-substr(filesToProcess, nchar(filesToProcess)-11, nchar(filesToProcess))
#    lista <-  as.character(filesToProcess[!(fil %in% logRaw$V1)])
#    if(length(lista) == 0)  stop('\n **** Nada para processar **** \n',call. = FALSE)
# }

# estações com dados a processar
# sta = sort(unique(substr(as.character(lista), nchar(as.character(lista))-11, 
#                          nchar(as.character(lista))-9)))

# Processando por estação
# for(j in 1:length(sta)){
#   lista_sta = lista[substr(lista, nchar(lista)-11, nchar(lista)-9) == sta[j]]
#   data_sta = list()
#   cat("Processando estação:", sta[j], "\n")
  
  # for(i in 1:length(lista_sta)){
  #   out = unlist(strsplit(lista_sta[i], "/"))[unlist(lapply(strsplit(lista_sta[i], "/"), length))]
  #   write.table(out, file = "Log_raw_data", append = TRUE, row.names = F,
  #              col.names= F, quote = F)
  #   cat("Processando arquivos novos:", i, out, "\n")
    
  #   aux = readLines(lista_sta[i])
  #   data_sta [[i]] <- cleanFiles(aux)
  # }

  # merging new data 
  # test = do.call(rbind,data_sta)
  # test = unique(test)
  # test = data.frame(date = test$date, apply(test[,-1], 2, as.numeric))
  # test$date = as.POSIXct(test$date, tz ="GMT")
  
  
  # if(sta[j] %in% c("H06", "H08", "H09")){    # duas séries de 2 e 5 min
  #   pos <- which(diff(test$date) %in% 120)+1
  #   ref1 <- test[1:pos[length(pos)],]
  #   ref1 <- ref1[!is.na(ref1$date),]

    # ref2 <- test[pos[length(pos)]+1:nrow(test),]
    # ref2 <- ref2[!is.na(ref2$date),]

    # tt <-  sort(as.POSIXlt(ref1$date, tz="GMT"), descending = TRUE)
    # ini <- as.POSIXct(ref1$date[1], tz = "GMT")
    # end <- as.POSIXct(ref1$date[length(tt)], tz = "GMT")
    # rr1 <- data.frame(date=seq.POSIXt(ini, end, by = "2 min"))
    # dt1 <- merge(rr1, ref1, all.x = T)

 #    tt =  sort(as.POSIXlt(ref2$date, tz="GMT"), descending = TRUE)
 #    ini = as.POSIXct(ref2$date[1], tz = "GMT")
 #    end = as.POSIXct(ref2$date[length(tt)], tz = "GMT")
 #    rr2 <- data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
 #    dt2 <- merge(rr2, ref2, all.x = T)
 #    dt <- rbind(dt1, dt2)
 #  }else{
 #    tt <-  sort(as.POSIXlt(test$date, tz="GMT"), descending = TRUE)
 #    ini = as.POSIXct(tt[1], tz = "GMT")
 #    end = as.POSIXct(tt[length(tt)], tz = "GMT")
 #    ref = data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
 #    dt = merge(ref, test,all.x=T, by = "date")
 #    dt = dt[which(!is.na(dt$date)),]
 # }

  
  
  # se existe dado anterior concatenar
  # old = list.files(merge, pattern=paste0(sta[j],"_merge.txt"))
  # if(length(old) == 1){
  #   old = read.csv(paste0(merge,'/',old))
  #   dt = merge(dt, old, all=T)
  # }else{
  #   cat("Primeiros dados da estação: ",sta[j],'\n')
  # }
  
#---------------------------------------------------------------------------
# garantir que não haja dados duplicados
#---------------------------------------------------------------------------
   # dt1 <- unique(dt)
   # dt2 <- dt1[!duplicated(dt1$date),]

   # conferir se há inconsistencia entre dado anterior e atual:
#    ini = as.POSIXct(dt2$date[1], tz = "GMT")
#    end = as.POSIXct(dt2$date[length(dt2$date)], tz = "GMT")
#    ref = data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
# #   dt = merge(ref, dt2, all.x=T, by = "date")
#   timePlot(dt2, names(dt2)[2])

#------------------------------------------------------------------------
# escrever dado bruto 
#------------------------------------------------------------------------
#   write.csv(dt2, paste0(merge, "/",sta[j],"_merge.txt"), row.names = F)
# }

# cDoubleLines(paste0(merge,'/H04_merge.txt'))
# cDoubleLines(paste0(merge,'/H05_merge.txt'))
# cDoubleLines(paste0(merge,'/H06_merge.txt'))
# cDoubleLines(paste0(merge,'/H07_merge.txt'))
# cDoubleLines(paste0(merge,'/H08_merge.txt'))
# cDoubleLines(paste0(merge,'/H09_merge.txt'))
# cDoubleLines(paste0(merge,'/H10_merge.txt'))
# cDoubleLines(paste0(merge,'/H11_merge.txt'))
# cDoubleLines(paste0(merge,'/H21_merge.txt'))
# cDoubleLines(paste0(merge,'/H23_merge.txt'))

