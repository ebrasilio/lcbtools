#----------------------------------------------------------------------------#
# Emilia M S Brasilio - 30/09/2014                                           #
# Processamento dos dados do vertedor de SVG, -  todos os instrumentos       #
#----------------------------------------------------------------------------#
rm(list = ls())

library(openair)
library(stringi)


args <- commandArgs(TRUE)
if(length(args) == 0){
  stop("\n\t\t*** Falta informar data da coleta ***\n\n")
} else if(args[1] == "-h"){
  cat("\n *** Informar ano e dia do ano para processar *** \n Exemplo:\n \t
        2016 025 \n\n")
}else{
  outdir <- paste0('/data1/DATA/LCB/PDG/', args[1],'/P3V')
  inpdir <- paste0(outdir, "/genesis")
  cat("\n",inpdir,"\n")
}

# arquivos para processar
coleta <- paste0(args[2],substr(args[1],3,4))
baro <- paste0(inpdir, '/P3VB', coleta, '.csv')
level <- gsub('P3VB', 'P3VL', baro)

if(file.exists(baro) & file.exists(level)){
  cat('\n Processing: ', coleta)
  br <- read.csv(baro, skip = 11, header = FALSE)
  
  # acertando data
  if(sum(nchar(unlist(stri_split_fixed(br$V1, '/')))[1:3] == c(2,2,4)) == 3){
    date <- as.POSIXct(paste(br$V1, br$V2), format = "%d/%m/%Y %H:%M:%S", tz = "GMT")    
  } else if(sum(nchar(unlist(stri_split_fixed(br$V1, '/')))[1:3] == c(2,2,4)) == 1){
    date <- as.POSIXct(paste(br$V1, br$V2), format = "%Y/%m/%d %H:%M:%S", tz = "GMT")
  } else if(sum(nchar(unlist(stri_split_fixed(br$V1, '/')))[1:3] == c(2,2,4)) == 0){
    date <- as.POSIXct(paste(br$V1, br$V2), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  }
  br <- data.frame(date, br[,4:5])
  names(br)[2:3] <- c("Baro", "T_agua")
  
  lv <- read.csv(level, skip = 12, header = FALSE)
  
  # acertando data
  if(sum(nchar(unlist(stri_split_fixed(lv$V1, '/')))[1:3] == c(2,2,4)) == 3){
    date <- as.POSIXct(paste(lv$V1, lv$V2), format = "%d/%m/%Y %H:%M:%S", tz = "GMT")    
  } else if(sum(nchar(unlist(stri_split_fixed(lv$V1, '/')))[1:3] == c(2,2,4)) == 1){
    date <- as.POSIXct(paste(lv$V1, lv$V2), format = "%Y/%m/%d %H:%M:%S", tz = "GMT")
  } else if(sum(nchar(unlist(stri_split_fixed(lv$V1, '/')))[1:3] == c(2,2,4)) == 0){
    date <- as.POSIXct(paste(lv$V1, lv$V2), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  }
  lv <- data.frame(date, lv[,4:5])
  names(lv)[2:3] <- c("Level", "T_ar")
  
} else {
  stop('\n**** Não existe arquivo level****\n') 
}

#---------------------------------------------------------------------------------------        
st <- as.POSIXct(format(round(br$date[1], units = "hours"), 
                        format="%Y-%m-%d %H:%M:%S"), tz = "GMT")

# média 10 minutos para sincronizar
br10 <- timeAverage(br1, avg.time="10 min")
br10$date <- br10$date + 10 * 60 
lv10 <- timeAverage(lv1, avg.time="10 min")
lv10$date <- lv10$date + 10 * 60 
vert <- merge(br10, lv10, by = "date", all = T)
vert$Cota_cm <- (vert$Level - vert$Baro)*10^2 / 9.8
vert$date <- as.character(vert$date)

# saida 
OutFile <- paste0(outdir, "/pdg_p3v_", args[1], "_", args[2], ".ascii")
write.csv(vert, OutFile, row.names = FALSE, na = "-9999")




