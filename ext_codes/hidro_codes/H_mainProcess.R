#-----------------------------------------------------------------------
# LCB - 31/05/2017 - Emilia M S Brasilio                                
#                                                                       
# 1. Leitura do arquivo anterior (caso exista)                          
# 2. ler dados na pasta raw # RE melhorar                               
#-----------------------------------------------------------------------
rm(list=ls())

library(openair)
library(stringr)
library(dplyr)

# paths
infiles <- "/data1/DATA/LCB/EXT/origin"
merge   <- "/data1/DATA/LCB/EXT/STORAGE/data/H/merge"
source('functions/functions.R')

#------------------------------------------------
# 1.1 Ler lista de arquivos da base, verificá-los
# Tá pegando tudo
#------------------------------------------------

filesToProcess = list.files(infiles, pattern = "^H", recursive = T,  full.names = T)
#filesToProcess = filesToProcess[substr(filesToProcess, 28, 31) %in% as.character(c(2015))]

# às vezes aparecem dados de outros formatos, precisa verificar
#csv e xlsx

csv = which(substr(filesToProcess, nchar(filesToProcess)-3, nchar(filesToProcess)) =='.csv')
if(length(csv) > 0){
  cat("\n Arquivos extranhos:\n")
  for(z in 1:length(csv)) {cat(filesToProcess[csv[z]],'\n')}
  filesToProcess <- filesToProcess[-csv]
}

xls = which(substr(filesToProcess, nchar(filesToProcess)-3, nchar(filesToProcess)) =='.xls' |
            substr(filesToProcess, nchar(filesToProcess)-4, nchar(filesToProcess)) =='.xlsx')
if(length(xls) > 0){
  cat("\n Arquivos extranhos:\n")
  for(z in 1:length(xls)) {cat(filesToProcess[xls[z]],'\n')}
  filesToProcess <- filesToProcess[-xls]
}

#-------------------------------------------------------------------------------------------
# 2.1 se existe log, lê dados antigos + dados novos por estação, concatena, média e escreve.
#-------------------------------------------------------------------------------------------
if(!file.exists("Log_raw_data")){
  cat("Log não encontrado, processando todos os dados\n")
  lista = as.character(filesToProcess)
}else{
  cat("Log encontrado, verificando se existem arquivos novos a processar\n\n")
   logRaw <- read.csv("Log_raw_data", header = F, stringsAsFactors = F)
   fil <-substr(filesToProcess, nchar(filesToProcess)-11, nchar(filesToProcess))
   lista <-  as.character(filesToProcess[!(fil %in% logRaw$V1)])
   if(length(lista) == 0)  stop('\n **** Nada para processar **** \n',call. = FALSE)
}

#-----------------------------------------------
# lista de estações com dados a processar
#-----------------------------------------------
sta = unique(substr(as.character(lista), nchar(as.character(lista))-11, nchar(as.character(lista))-9))

#---------------------------------------------------------------------------------------
# 3- Ler arquivos por estação e processar, aplicar limpeza e critérios 
#    básicos de qualidade e copiá-los para a pasta temp.
#---------------------------------------------------------------------------------------
for(j in 1:length(sta)){
  lista_sta = lista[substr(lista, nchar(lista)-11, nchar(lista)-9) == sta[j]]
  data_sta = list()
  cat("Processando estação:", sta[j], "\n")
  
  for(i in 1:length(lista_sta)){
    cat("Processando arquivos novos:", i, lista_sta[i], "\n")
    out = unlist(strsplit(lista_sta[i], "/"))[unlist(lapply(strsplit(lista_sta[i], "/"), length))]
    write.table(out, file = "Log_raw_data", append = TRUE, row.names = F,
               col.names= F, quote = F)
    
    aux = readLines(lista_sta[i])
    label = gsub(",,","",aux[substr(aux,1,4) == "T_St"][1])
    if(is.null(label)|is.na(label)){
      label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    } else if(label != "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"){
      label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    }

    if(length(aux) != 0){
      # 3.1.1 remover dupla vírgula dos dados
      aux = gsub(",,", "", aux)

      # 3.1.2 trocar '/' por '-' na data quando aparecer
      aux = gsub("/", "-", aux)

      # 3.2 remover todas as linha começando por caracter
      pos_grep = grep("[A-z]", substr(aux,1,1))
      if(length(pos_grep) > 0) aux = aux[-pos_grep]
      
      # 3.3 remover linhas com "bateria descarregada"
      aux <- aux[which(is.na(str_match(aux, "Bateria descarregada")))]
      
      # 3.4 verificar linhas sem oito colunas
      aux <- aux[str_count(aux,',') == 7]
      
      if(length(aux) != 0){
        # 3.3 checar time-stamp
        p1 = which(str_locate(aux, ",")[,1] != 17)  # campo da data incompleto
        if(length(p1) > 0) aux = aux[-p1] # remover linha
        ts = unlist(lapply(strsplit(aux, ","), function(x){return(x[1])}))
        data = matrix(unlist(strsplit(substr(ts,1,10), "-")), ncol=3,byrow = T)

        # onde tem  ano trocado com dia precisa acertar.....
        a = apply(data, 2, function(x)unique(nchar(x)))
        if(length(unique(unlist(lapply(a, length)))) == 1){
          yr_pos = which(apply(data, 2, function(x)unique(nchar(x))) == 4)
          if(yr_pos == 1){
            fmt = "%Y-%m-%d %H:%M"
          }else{
            aux = paste0(
              substr(aux,7,10), substr(aux,3,6), substr(aux,1,2),
              substr(aux,11,nchar(aux)))
            fmt = "%Y-%m-%d %H:%M"
          }
        }else{
        # procurar onde está o problema e dazer um substr nele
          pos2 = which(nchar(data[,1]) == 2)
          aux[pos2] = paste0(
            substr(aux[pos2],7,10),substr(aux[pos2],3,6), substr(aux[pos2],1,2),
            substr(aux[pos2],11,nchar(aux[pos2])))
          fmt = "%Y-%m-%d %H:%M"
          ts = unlist(lapply(strsplit(aux, ","), function(x){return(x[1])}))
        }

        ts = !is.na(as.POSIXct(ts, format= fmt, tz ="GMT"))
        if(length(ts) != length(aux)){
          cat("Problema com data no arquivo ",lista[i], "\n")
          aux = aux[ts]
        }

        # separar em colunas, arrumar labels
        data_sta[[i]] = data.frame(matrix(ncol = 8, nrow = length(aux), 
                                          unlist(strsplit(aux, ",")), byrow = T))
        names(data_sta[[i]]) =  c("date",unlist(strsplit(label,","))[-1])
      } else{
        cat("*** ARQUIVO APENAS COM CABEÇALHO *** !!!\n")
      }
    }else{
      cat("*** ARQUIVO VAZIO *** !!!\n")
    }
  
  }

  # @@@
  # merging new data 
  test = do.call(rbind,data_sta)
  test = data.frame(date = test$date, apply(test[,-1], 2, as.numeric))
  
  # formatar data e verificar consistência
  test$date = as.POSIXct(test$date, format= fmt, tz ="GMT")

  if(sum(diff(test$date) %in% 120)){    # duas séries
    pos <- which(diff(test$date) %in% 120)+1
    ref1 <- test[1:pos[length(pos)],]
    ref1 <- ref1[!is.na(ref1$date),]
    
    ref2 <- test[pos[length(pos)]+1:nrow(test),]
    ref2 <- ref2[!is.na(ref2$date),]
    
    tt <-  sort(as.POSIXlt(ref1$date, tz="GMT"), descending = TRUE)
    ini <- as.POSIXct(ref1$date[1], tz = "GMT")
    end <- as.POSIXct(ref1$date[length(tt)], tz = "GMT")
    rr1 <- data.frame(date=seq.POSIXt(ini, end, by = "2 min"))
    dt1 <- merge(rr1, ref1, all.x = T)
    
    tt =  sort(as.POSIXlt(ref2$date, tz="GMT"), descending = TRUE)
    ini = as.POSIXct(ref2$date[1], tz = "GMT")
    end = as.POSIXct(ref2$date[length(tt)], tz = "GMT")
    rr2 <- data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
    dt2 <- merge(rr2, ref2, all.x = T)
    dt <- rbind(dt1, dt2)
  }else{
    tt <-  sort(as.POSIXlt(test$date, tz="GMT"), descending = TRUE)
    ini = as.POSIXct(tt[1], tz = "GMT")
    end = as.POSIXct(tt[length(tt)], tz = "GMT")
    ref = data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
    dt = merge(ref, test,all.x=T, by = "date")
    dt = dt[which(!is.na(dt$date)),]
  }
  
  
  # se existe dado anterior concatenar
  old = list.files(merge, pattern=paste0(sta[j],"_merge.txt"))
  if(length(old) == 1){
    old = read.csv(paste0(merge,'/',old))
    dt = merge(dt, old, all=T)
  }else{
    cat("Primeiros dados da estação: ",sta[j],'\n')
  }
  
#---------------------------------------------------------------------------
# garantir que não haja dados duplicados
#---------------------------------------------------------------------------
   dt1 <- unique(dt)
   dt2 <- dt1[!duplicated(dt1$date),]
  
#------------------------------------------------------------------------
# escrever dado bruto 
#------------------------------------------------------------------------
  write.csv(dt2, paste0(merge, "/",sta[j],"_merge.txt"), row.names = F)
}

cDoubleLines(paste0(merge,'/H04_merge.txt'))
cDoubleLines(paste0(merge,'/H05_merge.txt'))
cDoubleLines(paste0(merge,'/H06_merge.txt'))
cDoubleLines(paste0(merge,'/H07_merge.txt'))
cDoubleLines(paste0(merge,'/H08_merge.txt'))
cDoubleLines(paste0(merge,'/H09_merge.txt'))
cDoubleLines(paste0(merge,'/H10_merge.txt'))
cDoubleLines(paste0(merge,'/H11_merge.txt'))
cDoubleLines(paste0(merge,'/H21_merge.txt'))
cDoubleLines(paste0(merge,'/H23_merge.txt'))

