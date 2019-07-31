#-------------------------------------------------------------------------------#
# LCB - 31/05/2017 - Emilia M S Brasilio                                        #
#                                                                               #
# 1. Leitura do arquivo anterior (caso exista)                                  #
# 2. ler dados na pasta raw # RE melhorar                                       #
#3. ler, corrigir, converter e completar tabela lida                            #
#4. calcular médias                                                             #
#5. escrever saidas                                                             #
#6. sincronizar na storage                                                      #
#                                                                               #
#-------------------------------------------------------------------------------#
rm(list=ls())

library(openair)
library(stringr)

source("~/git/lcbtools/R/soilHumidityFunctions.R")

mergePath <- "/data1/DATA/LCB/EXT/STORAGE/data/H/merge"
tempPath  <- "/data1/DATA/LCB/EXT/STORAGE/data/H/temp"

# 1.1 Ler lista de arquivos da base, verificá-los 
    allFiles = list.files("../../..", pattern = "H........TXT", recursive=T, full.names = T)
    allFiles = allFiles[substr(allFiles, 10,13) %in% as.character(2014:2017)]

# 1.1 arquivos com mesmo nome em mais de uma pasta?
    tb = unique(table(substr(allFiles, nchar(allFiles)-11, nchar(allFiles)-4)))
    if(length(tb) > 1){
        cat("**** Arquivo duplicado ****\n")
        rep = table(substr(allFiles, nchar(allFiles)-11, nchar(allFiles)-4))[
            table(substr(allFiles, nchar(allFiles)-11, nchar(allFiles)-4)) != 1]
        repFull = allFiles[substr(allFiles, nchar(allFiles)-11, nchar(allFiles)-4) %in% unlist(dimnames(rep))]
        for(i in 1:length(repFull)){
            cat(paste0("Doppelganger: ", i, "\t", repFull[i]), "\n")
        }
    #stop("**** Abortando execução - Verifique erros ****", call. = F)
    }

    
# 2.1 se existe log, lê dados antigos + dados novos por estação, concatena, média e escreve.
    if(file.exists("Log_raw_data")){
        cat("Log encontrado, verificando arquivos novos a processar\n\n")
        logRaw = read.csv("Log_raw_data", header = F)
        allFiles = data.frame(V1=allFiles)
        lista = allFiles[!(allFiles$V1 %in% logRaw$V1),]
    }else{
        cat("Log não encontrado, processando todos os dados\n")
        lista = allFiles        
    }
    
    # lista de estações
    sta = unique(substr(as.character(lista), nchar(as.character(lista))-11, nchar(as.character(lista))-9))

# 3 Ler arquivos por estação e processar, aplicar limpeza e critérios básicos de qualidade e copiá-los para a pasta temp.
    for(j in 1:length(sta)){
        lista_sta = as.character(lista[str_split_fixed(lista,"/",7)[,6] == sta[j]])
        data_sta = list()
        
        for(i in 1:length(lista_sta)){
            cat("Processando arquivos novos:", i, lista_sta[i], "\n")
            #write.table(lista_sta[i], "Log_raw_data", append = TRUE, row.names = F, col.names=F)
            out = unlist(strsplit(lista_sta[i], "/"))[unlist(lapply(strsplit(lista_sta[i], "/"), length))]
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
                
                # 3.1.3 verificar se tem 8 colunas
                #cat("colunas:", unique(str_count(aux, ",")), "\n")
                
                # 3.2 remover todas as linha começando por caracter
                pos_grep = grep("[A-z]", substr(aux,1,1))
                if(length(pos_grep) > 0) aux = aux[-pos_grep]
                
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
                    }#else{
                     #   cat("Data ok  ",lista_sta[i], "\n")
                    #}
                    
                    # separar em colunas, arrumar labels e escrever
                    data_sta[[i]] = data.frame(matrix(ncol = 8, nrow = length(aux), unlist(strsplit(aux, ",")), byrow = T))
                    names(data_sta[[i]]) =  c("date",unlist(strsplit(label,","))[-1])
                   
                    # write.table(label, paste0(tempPath,"/",out), sep=",", quote=FALSE,
                    #             row.names=FALSE, col.names=FALSE, append=TRUE)   
                    # write.table(aux, paste0(tempPath,"/",out), sep=",", quote=FALSE,
                    #             row.names=FALSE, col.names=FALSE, append=TRUE) 
                    
                } else{
                    cat("*** ARQUIVO APENAS COM CABEÇALHO *** !!!\n")
                }
                
            }else{
                cat("*** ARQUIVO VAZIO *** !!!\n")
            }
        }

        # merge nos dados 
        test = do.call(rbind,data_sta)
        test = data.frame(date = test$date, apply(test[,-1], 2, as.numeric))
        
        # formatar data e verificar consistência
        test$date = as.POSIXct(test$date, format= fmt, tz ="GMT")
        tt =  sort(as.POSIXlt(test$date), descending = TRUE)
        ini = as.POSIXct(substr(tt[1],1,10), format = "%Y-%m-%d", tz = "GMT")
        end = as.POSIXct(substr(tt[length(tt)],1,10), format = "%Y-%m-%d", tz = "GMT")
        ref = data.frame(date=seq.POSIXt(ini, end, by = "5 min"))
        dt = merge(ref, test, all= T, by = "date")
        dt = dt[which(!is.na(dt$date)),]
        
        # remover horários sem dados
        pos = unique(which(!is.na(apply(dt[,-1],1, function(x){sum(x)}))))
        dt = dt[pos,]
        
        # se existe dado anterior concatenar
        old = list.files(mergePath, pattern=paste0(sta[j],"_merge.txt"))
        if(length(old) == 1){
            old = read.csv(paste0(mergePath,'/',old))
            dt = unique(merge(dt, old, all=T))
        }else{
            cat("Primeiros dados da estação: ",sta[j])
        }
        
        # converter e fazer médias
        data_sta_conv = deltaTConvert(dt, channels = names(dt)[-c(1:2)], soilType = "mineral", relation = "polynomial")
        data_conv_hora = timeAverage(data_sta_conv, avg.time = "hour")
        data_conv_day = timeAverage(data_sta_conv, avg.time = "day")

        # escrever dado bruto e medias
        write.csv(dt, paste0(mergePath, "/",sta[j],"_merge.txt"), row.names = F)
        write.csv(data_sta_conv, paste0(mergePath, "/",sta[j],"_merge_convert.txt"), row.names = F)
        write.csv(data_conv_hora, paste0(mergePath, "/",sta[j],"_merge_convert_Hour.txt"), row.names = F)
        write.csv(data_conv_day, paste0(mergePath, "/",sta[j],"_merge_convert_Day.txt"), row.names = F)
        
    }
