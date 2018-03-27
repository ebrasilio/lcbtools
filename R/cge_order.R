#-------------------------------------------------------------------------#
# Ordenar dados do CGE                                                    #  
# Emilia M S Brasilio - Mar2018                                           #
#-------------------------------------------------------------------------#
rm(list=ls())

library(stringr)
library(openair)

options(digits = 16)

# função para acertar colunas 
corrige_labels <- function(mylist){
  
  for(i in 1:length(mylist)){
    aux <- mylist[[i]]
    if(ncol(aux) == 8){
      #cat(i, ncol(aux),'\n')
      head(aux,1)
      aux <- data.frame(date = as.POSIXct(aux$DATA, tz='GMT'), aux[,-c(1:2,8)])
      names(aux) [-1] <- c('Plu_mm','Tar_C','UR_%','Press_mb','BatV')
      mylist[[i]] <- aux
      
    } else if(ncol(aux) == 9){
      #cat(i, ncol(aux),'\n')
      head(aux,1)
      aux <- data.frame(date = as.POSIXct(aux$DATA, tz='GMT'), aux[,-c(1:2,9)])
      names(aux) [-1] <- c('Plu_mm','ws_ms','wd_o','Tar_C','UR_%','Press_mb')
      mylist[[i]] <- aux
      
    } else if(ncol(aux) == 10){
      #cat(i, ncol(aux),'\n')
      head(aux,1)
      aux <- data.frame(date = as.POSIXct(aux$DATA, tz='GMT'), aux[,-c(1:2,10)])
      names(aux) [-1] <- c('Plu_mm','ws_ms','wd_o','Tar_C','UR_%','Press_mb','Rajada_ms')
      mylist[[i]] <- aux
      
    } else if(ncol(aux) == 11){
      #cat(i, ncol(aux),'\n')
      head(aux,1)
      aux <- data.frame(date = as.POSIXct(aux$DATA, tz='GMT'), aux[,-c(1:2,11)])
      names(aux) [-1] <- c('Plu_mm','ws_ms','wd_o','Tar_C','UR_%','Press_mb','BatV','Rajada_ms')
      mylist[[i]] <- aux
      
    } else if(ncol(aux) == 12){
      #cat(i, ncol(aux),'\n')
      head(aux,1)
      aux <- data.frame(date = as.POSIXct(aux$DATA, tz='GMT'), aux[,-c(1:2,12)])
      names(aux) [-1] <- c('Plu_mm','ws_ms','wd_o','Tar_C','UR_%','Press_mb','BatV','SensTer_C','Rajada_ms')
      mylist[[i]] <- aux
      
    } else {
      cat(i, ncol(aux),' PROBLEMAS COM LABELS \n')
      head(aux,1)
      stop()
    }
  }
  return(mylist)
}


# acertando os metadatas
metadata <- read.table('../cge_anaCarolina_version/estaçoes_CGE_LAT_LONG.csv',
                       sep = ';', stringsAsFactors = F, na.strings = c('-9999', "NA"),
                       header=T)
metadata$lon <- NA
metadata$lat <- NA
metadata$Sta <- NA
metadata$Alt <- NA
metadata$Nome <- NA
  lon <- as.numeric(unlist(str_split(metadata$wkt_geom, pattern = "-", n = 4))[c(F,T,F)]) * -1
  lat <- unlist(str_split(metadata$wkt_geom, pattern = "-", n = 4))[c(F,F,T)]
  lat <- as.numeric(substr(lat, 1, nchar(lat)-1)) * -1
metadata$lon[1:length(lon)] <- lon
metadata$lat[1:length(lat)] <- lat
  Sta <- unlist(str_split(metadata$Estacao, pattern = "_", n = 2))[c(T,F)]
  Nome <- unlist(str_split(metadata$Estacao, pattern = "_", n = 2))[c(F,T)]
metadata$Sta[1:length(Sta)] <- Sta
metadata$Nome[1:length(Nome)] <- Nome
  Alt <- as.numeric(substr(metadata$Descriptio, nchar(metadata$Descriptio)-4, nchar(metadata$Descriptio)-1))
metadata$Alt[1:length(Alt)] <- Alt

metadata <- metadata[,c(10,12,9,8,11)]

rm(list=c("lon","lat", "Sta","Nome","Alt"))


# leitura dos dados antigos e novos, correção de labels e merging dos arquivos
inp1 <- list.files(pattern="csv")
inp2 <- list.files("2015 a 17_dados/", pattern = "csv")

sta1 <- str_to_lower(unlist(str_split(inp1, "_",3))[c(F,T,F)])
sta2 <- str_to_lower(unlist(str_split(inp2, "_",3))[c(F,T,F)])

# ler parte1 e parte 2
ini1 <- lapply(inp1, function(.file)read.csv(.file))
ini2 <- lapply(inp2, function(.file)read.csv(paste0("2015 a 17_dados/",.file)))

# arrumar colunas
ini12 <- corrige_labels(ini1)
ini22 <- corrige_labels(ini2)

# merging por estação e saida

sta <- unique(c(sta1, sta2))
for(i in 1:length(sta)){
  if(sum(sta1 %in% sta[i]) == 1 && sum(sta2 %in% sta[i]) == 1){
    auxmerge <- merge(ini12[[which(sta1 %in%sta[i])]],
                      ini22[[which(sta2 %in%sta[i])]], all=T)
    ini <- as.POSIXct(paste0(substr(min(auxmerge$date),1,14),"00:00"),tz='GMT')
    end <- as.POSIXct(paste0(substr(max(auxmerge$date),1,14),"50:00"),tz='GMT')
    ref <- data.frame(date = seq.POSIXt(ini, end, by='10 min'))
    out <- merge(ref, auxmerge, by='date', all.x = T)
    nn <- metadata$Nome[metadata$Sta == sta[i]]
    cat(sta[i],': ',nn, '\n')
    jpeg(paste0('data/',nn,'.jpeg'))
      timePlot(out, names(out)[-1], y.relation = 'free')
    dev.off()
    
    #separar por ano
    year = unique(substr(out$date,1,4))
    for(j in 1:length(year)){
      dir.out <- paste0('data/',year[j])
      if(!dir.exists(dir.out)){
        dir.create(dir.out)
      }
      write.table(selectByDate(out, year = year[j]),
                  paste0(dir.out,'/', nn,"_",year[j],'.csv'),
                  sep=",",row.names = F, na = '-9999', quote = F)
    }
    
  } else if(sum(sta1 %in% sta[i]) == 1 && sum(sta2 %in% sta[i]) == 0) {
    auxmerge <- ini12[[which(sta1 %in%sta[i])]]
    ini <- as.POSIXct(paste0(substr(min(auxmerge$date),1,14),"00:00"),tz='GMT')
    end <- as.POSIXct(paste0(substr(max(auxmerge$date),1,14),"50:00"),tz='GMT')
    ref <- data.frame(date = seq.POSIXt(ini, end, by='10 min'))
    out <- merge(ref, auxmerge, by='date', all.x = T)
    nn <- metadata$Nome[metadata$Sta == sta[i]]
    cat(sta[i],': ',nn, '\n')
    jpeg(paste0('data/',nn,'.jpeg'))
      timePlot(out, names(out)[-1], y.relation = 'free')
    dev.off()
    
    year = unique(substr(out$date,1,4))
    for(j in 1:length(year)){
#      cat("\n Year: ", year[j])
      dir.out <- paste0('data/',year[j])
      if(!dir.exists(dir.out)){
        dir.create(dir.out)
      }
      write.table(selectByDate(out, year = year[j]),
                  paste0(dir.out,'/', nn,"_",year[j],'.csv'),
                  sep=",",row.names = F, na = '-9999', quote = F)
    }
    
  } else if(sum(sta1 %in% sta[i]) == 0 && sum(sta2 %in% sta[i]) == 1){
    auxmerge <- ini22[[which(sta2 %in%sta[i])]]
    ini <- as.POSIXct(paste0(substr(min(auxmerge$date),1,14),"00:00"),tz='GMT')
    end <- as.POSIXct(paste0(substr(max(auxmerge$date),1,14),"50:00"),tz='GMT')
    ref <- data.frame(date = seq.POSIXt(ini, end, by='10 min'))
    out <- merge(ref, auxmerge, by='date', all.x = T)
    nn <- metadata$Nome[metadata$Sta == sta[i]]
    cat(sta[i],': ',nn, '\n')
    jpeg(paste0('data/',nn,'.jpeg'))
      timePlot(out, names(out)[-1], y.relation = 'free')
    dev.off()
    
    #separar por ano
    year = unique(substr(out$date,1,4))
    for(j in 1:length(year)){
#      cat("\n Year: ", year[j])
      dir.out <- paste0('data/',year[j])
      if(!dir.exists(dir.out)){
        dir.create(dir.out)
      }
      write.table(selectByDate(out, year = year[j]),
                  paste0(dir.out,'/', nn, "_",year[j],'.csv'),
                  sep=",",row.names = F, na = '-9999', quote = F)
    }
  } else {
    cat(sta[i],'\n')
    cat(i,'fudeu \n')
    stop()
  }
} 








