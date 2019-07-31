#--------------------------------------------------------------------------
# Gerar médias hora, dia
#--------------------------------------------------------------------------
rm(list=ls())

# my functions
# dateFormat = function(dt_vec){
#     
#     dt_vec = as.character(dt_vec)
#     
#     b = as.POSIXct(dt_vec, format = whichFormat(dt_vec), tz = "GMT")
#     return(b)    
# }
# 
# ## todo-aplicar a todas as posições do vetor
# 
# whichFormat = function (charvec) {
#     if (is.null(charvec)) 
#         return("unknown")
#     if (all(is.na(charvec))) 
#         return(NA)
#     charvec = as.character(charvec)
#     nb = unique(nchar(charvec))
#     
#     
#     if(length(nb) == 1){
#         if(unique(substring(charvec, 3, 3) == "-")){
#             if(nb == 10){
#                 return("%d-%m-%Y")  
#             }else if(nb == 16){
#                 return("%d-%m-%Y %H:%M")
#             }else {
#                 warning("character string is not in a standard unambiguous 
#                 format")
#             }    
#         }else{
#             if(nb == 16){
#                 return("%Y-%m-%d %H:%M")
#             }
#             
#             unique(substring(charvec[1], 5, 5))    
#         }
#         
#     }else{
#         warning("mais de um formato")
#     }
# }

#============================================================

library(openair)

setwd("/data1/DATA/LCB/EXTREMA/STORAGE/codes/H_inicial/")

pathIn = "../../data/H/temp"
pathOut = "../../data/H/merge"

inpfiles = list.files(pathIn, full.names = T)
for(i in 1:length(inpfiles)){
    cat("Processando file: ", inpfiles[i], "\n")
    dt = read.csv(inpfiles[i], header=T)[,1:8]
    if(length(unique(nchar(as.character(dt$T_St)))) > 1){
        ln = which(nchar(as.character(dt$T_St)) != 16)
        stop("\nProblemas com data, verificar:\nFile:", inpfiles[i])
    }else{
        pos1 = which(substr(dt$T_St,1,4) %in% as.character(2010:2020))
        pos2 = which(!substr(dt$T_St,1,4) %in% as.character(2010:2020))
        dt$T_St = as.character(dt$T_St)
        dt$T_St[pos2] = paste0(substr(dt$T_St[pos2],7,10), substr(dt$T_St[pos2],3,6), 
                               substr(dt$T_St[pos2],1,2), substr(dt$T_St[pos2],11,16))
        date = as.POSIXct(dt$T_St, format="%Y-%m-%d %H:%M", tz="GMT")
        dt = apply(dt[,-1],2,as.numeric)
        dt = data.frame(date=date, 
                apply(dt,2,function(x){return(round(x,4))}))
#        ref = data.frame(date=seq.POSIXt(min(date, na.rm=T), max(date, na.rm=T),"2 min"))

        
        ## faltou verificar a sequencia correta de tempo
        
        dt_hora = timeAverage(dt, avg.time = "hour")
        dt_hora = data.frame(date = dt_hora$date, 
                             apply(dt_hora[,-1],2,function(x){return(round(x,4))}))
        dt_day = timeAverage(dt, avg.time = "day")
        dt_day = data.frame(date = dt_day$date, 
                            apply(dt_day[,-1],2,function(x){return(round(x,4))}))
        write.table(dt, 
                    paste0(pathOut,substr(inpfiles[i], nchar(inpfiles[i])-6, nchar(inpfiles[i])-4),"_raw.txt"),
                    sep=",", row.names=FALSE)
        write.table(dt_hora, 
                    paste0(pathOut,substr(inpfiles[i], nchar(inpfiles[i])-6, nchar(inpfiles[i])-4),"_hour.txt"),
                    sep=",", row.names=FALSE)
        write.table(dt_day,
                    paste0(pathOut,substr(inpfiles[i], nchar(inpfiles[i])-6, nchar(inpfiles[i])-4),"_daily.txt"),
                    sep=",", row.names=FALSE)
        
    }
}


