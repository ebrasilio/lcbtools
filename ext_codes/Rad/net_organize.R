#
#
# Ordenar dados de Net
# coletas 024/17 até 260/2017 estava no arquivo de fluxos médios 30 min,
#     TOA5_XF_ddddyy.data
# a partir de 300/2017 tem arquivo próprio com aquisiçao de 10 min
#     TOA5_XF_ddddyy_10.data
# a partir de 115/2019 incluidos sensores de radiaçao par e global que precisam
# ser organizados.

#  precisa dar uma saída mais analítica com estatísticas  e algumas 
# análises mais coerentes.
# 2023 - retomada dos trabalhos, atualizar dados 
# em 2022 a torre 1 foi removida e a plataforma de radiação ficou concentrada
# na torre2
#

rm(list=ls())

library(openair)
library(stringi)

#----2023----
# torre1 
tower1 <- '/home/emilia/lcb_2021/EXT/outExt/Radiação_torre1/'
rad1 <- read.csv(paste0(tower1,'EXT_Torre1_Radiation_10min.csv'),
                 stringsAsFactors = F, na.strings = '-9999')
rad1$date <- as.POSIXct(rad1$date, tz='GMT')
rad30 <- timeAverage(rad1, avg.time = '30 min', 
                     start.date = paste0(substr(rad1$date[1],1,14),'00'))

shf1 <- read.csv(paste0(tower1,'extrema_shf_Torre1_15min.csv'),
                 stringsAsFactors = F, na.strings = '-9999')
shf1$date <- as.POSIXct(shf1$date, tz='GMT')
shf1$SHF3_Avg. <- as.numeric(shf1$SHF3_Avg.)
shf30 <- timeAverage(shf1, avg.time = '30 min', 
                    start.date = paste0(substr(shf1$date[1],1,14),'00'))
tw1 <- merge(rad30, shf30, all=T, by="date")
timeVariation(tw1, names(tw1)[c(2,3,5,11)])


tower2 <- '/home/emilia/lcb_2021/EXT/outExt/Radiação_torre2/'

#----Final----
#
# Verificar se há dados novos a processar
# raw_data <- "/data1/DATA/LCB/EXT/origin"
# 
# flux1 <- list.files(raw_data, pattern = "TOA5_XF", recursive = T, full.names = T)
# flux2 <- list.files(raw_data, pattern = "TOA5_EF", recursive = T, full.names = T)
# log <- read.table('netRad.log', stringsAsFactors = FALSE, header = F)
# 
# flux <- c(flux1, flux2)
#

# lista de arquivos novos a sincronizar

flux <- flux[!(data.frame(V1=flux)$V1 %in% log$V1)]
if(length(flux) > 0){
    a <- c("\"TIMESTAMP", "Net_Avg" )
    # informação das colunas
    labels_tt <- lapply(flux,
                        function(.file)
                            which(unlist(stri_split_fixed(readLines(.file, n=2)[2], '\",\"')) %in% a))
    
    # dados e renomeação das colunas
    tt = lapply(flux,
                function(.file)
                    read.csv(.file,skip=4, na.strings='\"NAN\"', header=F))
    
    for (i in 1:length(tt)){
        names(tt[[i]]) <- unlist(stri_split_fixed(readLines(flux[[i]], n=2)[2], '\",\"'))
    }

    # dados a separar
    aux <- list()
    aux <- lapply(tt, "[",  a)
    
    new <- do.call(rbind, aux)
    names(new)[1] <- "date"
    new$date <- as.POSIXct(new$date, tz='GMT') 
    
    # organizar a série completa e de acordo com o banco
    ref <- data.frame(date=seq.POSIXt(min(new$date), max(new$date), by='10 min'))
    new <- merge(ref, new, by="date", all.x=TRUE)
    
    # Verificar se existe arquivo anterior, concatenar e imprimir.
    old_data <- "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiations_raw.csv"
    if(file.exists(old_data)){
        old <- read.csv(old_data, na.strings = '-9999')
        old$date <- as.POSIXct(old$date, tz='GMT')
        
        # verificar períodos
        if((max(old$date) < min(new$date)) & (sum(old$date %in% new$date) == 0)){
            ref = data.frame(date=seq.POSIXt(min(old$date), max(new$date), by='10 min'))
            out  <- rbind(old, new)
            out2 <- merge(ref, out, all.x = T)
           
            cat("Atualizando dados até ", substr(ref$date[nrow(ref)],1,10),"\n")
            write.csv(out2, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiations_raw.csv", 
                      row.names = F, quote = T, na='-9999')
            out30 <- timeAverage(out2, avg.time = '30 min', data.thresh = 0)
            write.csv(out30, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiations_30m.csv", 
                      row.names = F, quote = T, na='-9999')
            
        } else {
           stop("\n*******************************************\n", 
"*** DADOS REPLICADOS NOS ARQUIVOS NOVOS ***\n",
"*******************************************\n") 
        }
        
    } else{
        cat("Primeira Coleta")
        write.csv(new, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiations_raw.csv", 
                  row.names = F, quote = T, na='-9999')
        new30 <- timeAverage(new, avg.time = '30 min', data.thresh = 0)
        write.csv(new30, "/data1/DATA/LCB/EXT/STORAGE/data/I/Ext_Radiations_30m.csv", 
                  row.names = F, quote = T, na='-9999')
    }
    
    # atualizar log
    write.table(flux, '/data1/DATA/LCB/EXT/STORAGE/codes/Rad/netRad.log', append = T, row.names = F, col.names = F)
    
} else {
    stop("\n**************************************\n", 
              "*** SEM DADOS NOVOS PARA ATUALIZAR ***\n",
              "**************************************\n")
}



#
# new <- timeAverage(new, avg.time = '30 min', 
#                    start.date = paste0(substr(new$date[1],1,14),'00:00')) 
# ref <- data.frame(date=seq.POSIXt(new$date[1], new$date[nrow(new)], by='30 min'))
# new <- merge(ref,new, all=T)
# 
# 
# # Ler arquivo original
# old_data <- "/data1/DATA/LCB/EXT/STORAGE/data/I"
# old <- read.csv(paste0(old_data,'/Net_Extrema_30m.csv'))
# old$date <- as.POSIXct(old$date, tz='GMT')
# 
# # checar novamente se tem dado novo no new
# if(sum(!(new$date %in% old$date))==0){
#     cat('\n Nada novo pra incluir \n')
#     write.table(flux, 'netRad.log', append = T, row.names = F, col.names = F)
# } else {
#     out <- merge(old, new[,c(1:2)], all= T )
#     write.csv(out, '../../data/Radiation/Net_Extrema_30m.csv',
#               row.names = FALSE, quote = FALSE)
#     write.table(flux, 'netRad.log', append = T, row.names = F, col.names = F)
# }
# 





#
# Código utilizado para integrar os dados até 2017
{
    #  # arquivos 1 e 2 com 87 colunas
    # tt1 <- list()
    # for(i in 1:2){
    #     tt1[[i]] <- tt[[i]]
    # } 
    # tt1 = do.call(rbind, tt1)
    # names(tt1) = unlist(strsplit(readLines(flux[1],2)[2],'\",\"'))
    # names(tt1)[1] = "date"
    # tt1$date = as.POSIXct(tt1$date, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    # 
     # arquivos 4 a 6 com 86 colunas
    # tt2 <- list()
    # for(i in 3:length(flux)){
    #     tt2[[i]] <- tt[[i]]
    # }
    # tt2 = do.call(rbind,tt2)
    # names(tt2) = unlist(strsplit(readLines(flux[3],2)[2],'\",\"'))
    # names(tt2)[1] = "date"
    # tt2$date = as.POSIXct(tt2$date, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    # 
    # Unir todos os arquivos de 2017
    # temp = merge(tt1,tt2, all=T)
    # 
     # Corrigir dados com multiplicador errado 
    # # entre nov/2016 e abril/2018
    # ref <- data.frame(date = 
    #             seq.POSIXt(temp$date[1], as.POSIXct('2017-03-29 13:30:00', tz="GMT"), by='30 min'))
    # pos <- which(temp$date %in% ref$date)
    # temp$Net_Avg[pos] = (temp$Net_Avg[pos] -77.5194) * 77.5194
    # 
     # Net_30min - parte 1
    # net_30min <- temp[,c(1,85)]
    # 
    # 
    ## #ler dados após mudança
    # flux <- list.files("../../..", pattern = "TOA5_XF", recursive = T, full.names = T)
    # flux <- flux[substr(flux, nchar(flux)-5, nchar(flux)-4) == '10']
    # 
    # tt = lapply(flux,
    #             function(.file) 
    #                 read.csv(.file, na.strings='\"NAN\"', header=F, skip=4))
    # tt1 = do.call(rbind, tt)
    # names(tt1) = unlist(strsplit(readLines(flux[1],2)[2],'\",\"'))
    # names(tt1)[1] = "date"
    # tt1$date = as.POSIXct(tt1$date, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
    # tt1_30m <- timeAverage(tt1, avg.time = '30 min')
}
#
