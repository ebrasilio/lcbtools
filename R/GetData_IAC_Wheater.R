#==================================================================================
# Script desenvolvido para formatar dados copiados                                =
# da página do IAC, através do script GetData_IAC_Wheater.h                       =
#                                                                                 =
#                                       Emilia M S Brasilio                       =
#==================================================================================

rm(list = ls())
library(openair)
library(XML)

# leitura do metadata
pathdir <- "/data2/IAC_DATA/"
#index   <- read.csv("/data2/IAC_DATA/stations_metadata.csv")
index   <- read.table("/data2/IAC_DATA/stations_metadata.csv",
                      header = T)
path    <- "http://www.ciiagro.org.br/ema/index.php?id="

# back-up da base de dados antes de qualquer coisa
system(paste0("cp ", pathdir, "DATA/*.dat ", pathdir, "DATA/bk"))

for(ist in 1:nrow(index)){
  # Leitura dos dados da página para cada estação
  cat(paste0(ist, "\t", index$id[ist],"\t", index$Posto[ist],"\n"))
  con <- paste0(path, index$id[ist])
  data <- readLines(con, 191)
  
  LimInicio <- gregexpr("<td>", data[191])
  LimFinal <-  gregexpr("</td>", data[191])
  
  inicio <- LimInicio[[1]][1:length(LimInicio[[1]])]
  final  <- LimFinal[[1]][1:length(LimFinal[[1]])]
  
  dt <- matrix(ncol = 6, nrow = (length(inicio)/5))
  for(j in 1:length(inicio)){
      l <- ceiling(j/6)
      c <- j - 6 * (l - 1)
      
      dt[l,c] <- substr(data[191], inicio[j]+4, final[j]-1)  
  }
  
  # Gerar data.frame de numeros
  date <- as.POSIXct(dt[,1], format = "%d/%m/%Y %H:%M", tz = "GMT")
  dt2 <-  data.frame(apply(dt[,-c(1,6)], 2, function(x){
      x <- sub(",", ".", x)
      return (x)}))
  dt <- data.frame(date, dt2)
  names(dt) <- c("date","Temp","Umidade","Prec","SolarRad")
  
  dt <- dt[!is.na(date),]
  dt <- data.frame(date = dt$date,apply(dt[,-1], 2, as.numeric))
  
  ## Ler dados antigos
  ffile <- paste0(pathdir,"DATA/", index$id[ist], "_", index$Posto[ist], "_IAC.dat")
  inp2 <- read.csv(ffile)
  inp2 <- unique(inp2)
  
  inp2$date <- as.POSIXct(inp2$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  out <- merge(inp2, dt, all = TRUE)
  rm(inp2, dt)
  # QC 
  
  #out_file <- paste0("DATA/",index$id[ist],"_",index$Posto[ist],"_IAC.dat")
  write.csv(out, file = ffile, row.names = F)
  
  ### figuras para cada estação
  jpeg(paste0(pathdir,"OUT/",index$id[ist],"_",index$Posto[ist],"_IAC.jpeg"))
  timePlot(out, names(out)[-1], main = index$Posto[ist])
  dev.off()
}


