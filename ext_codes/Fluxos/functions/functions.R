#-----------------------------------------------------------------------------------------#
# EMSB - 26/06/2017                                                                       #
#   Função para plotar dados de alta frequencia do sistema de Fluxos, calculando a        #
#   Porcentagem de dados válidos dentro do arquivo                                        #
#-----------------------------------------------------------------------------------------#


diag_ec = function(path_in, colsToPlot = c(1:4,6,7)){
  inp = list.files(path_in, full.names=T, recursive=T)
  for(i in 1:length(inp)){
    cat(i,"\n")
    aux = read.csv(inp[i], skip=4, na.strings="NAN", 
                   stringsAsFactors = F, header=F)
    jpeg(paste0("RawDataIS/ext_",substr(inp[i],nchar(inp[i])-18,nchar(inp[i])-4),".jpeg"),
         width=1200,height=800)
    op = par(mfrow=c(2,3), las=1)
      plot(aux$V1, pch=20,main=paste0("U - n= ",  round(sum(!is.na(aux$V1))/nrow(aux)*100,2),"%"))
      plot(aux$V2, pch=20,main=paste0("V - n= ",  round(sum(!is.na(aux$V2))/nrow(aux)*100,2),"%"))
      plot(aux$V3, pch=20,main=paste0("W - n= ",  round(sum(!is.na(aux$V3))/nrow(aux)*100,2),"%"))
      plot(aux$V4, pch=20,main=paste0("Ts - n= ", round(sum(!is.na(aux$V4))/nrow(aux)*100,2),"%"))
      plot(aux$V6, pch=20,main=paste0("CO2 - n= ",round(sum(!is.na(aux$V6))/nrow(aux)*100,2),"%"))
      plot(aux$V7, pch=20,main=paste0("H2O - n= ",round(sum(!is.na(aux$V7))/nrow(aux)*100,2),"%"))
   dev.off() 
  }
  return(0)
}


#-----------------------------------------------------------------------------------------#
# EMSB - 26/06/2017
#   Função para ler dados de fluxos processados e fluxos médios do campo.                 #
#-----------------------------------------------------------------------------------------#

le_flx = function(path, pattern="full_output", remove = NULL, skip=3){
  library(reshape2)
  
  inp = list.files(path, recursive = T, pattern = pattern, full.names = T)
  aux = lapply(inp, 
            function(.file) read.csv(.file,
                                     stringsAsFactors = F, header = FALSE, skip=skip, 
                                     na.strings = c("-9999.0", "NAN")))
  # problema de labels 
  if(pattern == "TOA5_XF"){
      pos = which(grepl("08617",inp))    
      aux[[pos]] = aux[[pos]][-24]
      names(aux[[pos]]) = names(aux[[pos -1]])
  }
  aux = do.call(rbind,aux)
  names(aux) = unlist(strsplit(readLines(inp[1],n=2)[2],","))
  if(pattern == "full_output"){
    aux = data.frame(date=as.POSIXct(paste(aux$date, aux$time), tz="GMT"), aux[,-c(1:7)])  
  } else {
    aux = data.frame(date=as.POSIXct(aux[,1], format="%Y-%m-%d %H:%M:%S", tz="GMT"), aux[,-1])
  }
  
  ref = data.frame(date = seq.POSIXt(min(aux$date), max(aux$date), by="30 min"))
  aux = merge(ref, aux, by="date", all=T)
  
  # removing qc = 2 the worse case
  if(!is.null(remove)){
    pos = which( substr(names(aux),1,2) == "qc")
    for(i in 1:length(pos)){
      cat(i, "\n")
      aux[which(aux[,pos[i]] == remove), pos[i]-1] = NA
    }  
  }
  

  return(aux)
}


