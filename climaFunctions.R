#
### Função para checar se existem dados novos para a estação
  #  TODO Limpar em seguida

checkNewData <- function(inpDir, station, last=NULL){

  new <- list.files(inpDir, pattern = station,
                    recursive = TRUE, full.names = TRUE)
  new <- new[!str_detect(new, 'merge')]
  
  if(is.null(last)){
    return(new)
  } else {
    new_files <- data.frame(files=new,stringsAsFactors=F)
    new_files$date <- as.POSIXct(substr(new_files$files, nchar(new_files$files)-8, 
                                         nchar(new_files$files)-4), format='%y%j', tz = 'GMT')
    new <- new_files$files[which(new_files$date >= as.Date(last))]
    return(new)    
    }
}



#
### ler e organizar dados novos
  # new - vetor de arquivos novos a processar

new_dt <- function(new, station){
  
  dt <- lapply(new, function(x) readLines(x))
  
  # armazenar labels
  label <- lapply(dt, function(x) {
    label <- unique(x[str_detect(x, 'T_St')])
  })
  
  if(length(unique(unlist(label))) == 1) {
    cat('Labels em ordem\n') }

  label <- c("date","Bat","Dm","Sm","Ta","Ua","Pa","Rc","Vs")
 
  # leitura e organização dos dados
  dt <- lapply(dt, function(x){
                x <- x[!str_detect(x, '[:alpha:]')]
                x <- x[ str_count(x, ',') == 8 ]
                if(length(x) == 1)x <- c(x,x)
                x <- data.frame(str_split_fixed(x, ",", 9))
                x <- data.frame(date = x$X1, apply(x[,-1], 2, as.numeric))
  })
  dt1 <- plyr::ldply(Filter(NROW, dt), rbind)

  if(nrow(dt1) == 0){
    cat('\n Arquivo(s) sem dados válidos \n')
  }else{
    if(length(label) != ncol(dt1)){
      stop(cat('\n Numero de labels e coluna não conferem !! \n'))
    }else{
      names(dt1) <- label  
    }
    # remover dados antes de 2014
    year <- as.numeric(substr(Sys.Date(), 1, 4))
    pos <- substr(dt1$date, 1, 4) %in% as.character(2014:year)
    dt1 <- dt1[pos,]
    
    # verificar referência
    dt1$date <- as.POSIXct(dt1$date, format="%Y-%m-%d %H:%M", tz = 'GMT')
    dt1 <- unique(dt1[!is.na(dt1$date),])
    if ( length(as.POSIXct(names(which(table(dt1$date) != 1)), tz='GMT')) != 0){
      dt1 <- dt1[-c(which(dt1$date %in% as.POSIXct(names(which(table(dt1$date) != 1)), tz='GMT'))),]
    }
    ref <- data.frame(date=seq.POSIXt(min(dt1$date), max(dt1$date), '2 min'))
    dt2 <- merge(ref, dt1, all.x = TRUE)
    # QC
    # if(sum(dt2$Rc > 5, na.rm=T) != 0) {
    #   dt2$Rc[which(dt2$Rc > 5)] <- NA
    # }
    
    # limpar dados novos sempre que bateria cair para menor que 5 Volts
    pos <- which(dt2$Vs < 7)
    dt2$Dm[pos] <- NA
    dt2$Sm[pos] <- NA
    dt2$Ta[pos] <- NA
    dt2$Ua[pos] <- NA
    dt2$Pa[pos] <- NA
    dt2$Rc[pos] <- NA
    
    dt2$Pa[dt2$Pa < 600] <- NA  
    dt2$Ua[dt2$Ua < 10] <- NA
    dt2$Ta[dt2$Ta > 50 | dt2$Ta < 1] <- NA
    dt2$Rc[dt2$Rc >10] <- NA
    # acertar códigos de erro
#    apply(dt2, 2, function(x)range(x, na.rm=T))
    return(dt2)
  }
}


#
###  Cria data-frame com referência temporal.
  #   

check_ref <- function(min, max, by = '30 min'){
  ref <- data.frame(date=(seq.POSIXt(min, max, by)))
  return(ref)
}


#
### Verificar se não há novas estações 
  #

checkNewStations <- function(inpDir, oldSta){
  new <- list.files(inpDir, recursive=T) 
  sta <- stringr::str_extract(
                unique(substr(new,nchar(new)-11, nchar(new)-9)),
                '[C][0-9][0-9]')
  sta <- sta[!is.na(sta)]
  if(sum(!sta %in% oldSta) != 0){
    cat('\n Estações novas ou nunca processadas:\n')
    print(sta[!(sta %in% oldSta)])
    out <- sta[!(sta %in% oldSta)]
  return(out)
  } else {
    cat('\n Sem estações novas \n')
    return(NULL)
  }
}


printOuts <- function(mydt, out, station){
  library(openair)
  mydt <- data.frame(date=mydt$date,
                     apply(mydt[,-1], 2, function(x)round(x,4)))
  #médias
  hour <- timeAverage(mydt, avg.time = 'hour',statistic = 'mean', start.date = paste(substr(mydt$date[1],1,10),'00:00:00'))
  hour$Rc  <- timeAverage(mydt, avg.time = 'hour', statistic = 'sum', start.date = paste(substr(mydt$date[1],1,10),'00:00:00'))$Rc
  hour <- data.frame(date=hour$date,
                     apply(hour[,-1], 2, function(x)round(x,4)))
  day <- timeAverage(mydt, avg.time = 'day',statistic = 'mean', start.date = paste(substr(mydt$date[1],1,10),'00:00:00'))
  day$Rc  <- timeAverage(mydt, avg.time = 'day', statistic = 'sum', start.date = paste(substr(mydt$date[1],1,10),'00:00:00'))$Rc
  day <- data.frame(date=day$date,
                    apply(day[,-1], 2, function(x)round(x,4)))
  
  #imprimir
  cat('\n Escrevendo arquivos finais ', station, '  \n')
  write.csv(mydt, paste0(out,'/', sta[i], 'clear_merge.txt'), row.names = FALSE, na='-9999')
  write.csv(hour, paste0(out,'/', sta[i], 'clear_merge_H.txt' ), row.names = FALSE, na='-9999')
  write.csv(day, paste0(out,'/', sta[i], 'clear_merge_D.txt' ), row.names = FALSE, na='-9999')
  
}



checkRange <- function(new, station){
  dt <- lapply(new, function(x) readLines(x))
  
  # armazenar labels
  label <- lapply(dt, function(x) {
    label <- unique(x[str_detect(x, 'T_St')])
  })
  
  if(length(unique(unlist(label))) == 1) {
    cat('Labels em ordem\n') }
  
  label <- c("date","Bat","Dm","Sm","Ta","Ua","Pa","Rc","Vs")
  
  # leitura e organização dos dados
  dt <- lapply(dt, function(x){
    x <- x[!str_detect(x, '[:alpha:]')]
    x <- x[ str_count(x, ',') == 8 ]
    if(length(x) == 1)x <- c(x,x)
    x <- data.frame(str_split_fixed(x, ",", 9))
    x <- data.frame(date = x$X1, apply(x[,-1], 2, as.numeric))
  })
  
  out <- vector(length=length(new))
  for(j in 1:length(dt)){
  out[j] <- paste0(j, ' ', substr(new[j], nchar(new[j])-11, nchar(new[j])), 
              " -- ", as.character(dt[[j]][1,1]),' -- ',  as.character(dt[[j]][nrow(dt[[j]]),1]))
  }
  return(out)  
}


#
### Precisa documentar e verificar
  #

cleanFiles <- function(mydt){
  #if(length(mydt) != 0){ # if 1
    
    # label = gsub(",,","",mydt[substr(mydt,1,4) == "T_St"][1])
    # if(is.null(label)|is.na(label)){
    #   label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    # } else if(label != "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"){
    #   label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    # }
    
    if(length(mydt) != 0){
      mydt = gsub(",,", "", mydt)                                             # campos vazios
      mydt = gsub("/", "-", mydt)                                           # separador tempo
      mydt <- mydt[which(is.na(str_match(mydt, "Bateria descarregada")))]
      mydt <- mydt[str_count(mydt,',') == 7]
      pos_grep = grep("[A-z]", substr(mydt,1,1))                     # remove cabeçalho e lixos
      if(length(pos_grep) > 0)mydt = mydt[-pos_grep]
      
      p1 = which(str_locate(mydt, ",")[,1] != 17)                    # campo da data incompleto
      if(length(p1) > 0) mydt = mydt[-p1]                                     # remover linha
      
      # acertando timestamp
      if( length(mydt) == 0 ){
        cat("Arquivo Vazio \n")
        return()
      } else {
        ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))      # acertando data
        data = matrix(unlist(strsplit(substr(ts,1,10), "-")), ncol=3,byrow = T)
        a = apply(data, 2, function(x)unique(nchar(x)))
        
        if(length(unique(unlist(lapply(a, length)))) == 1){
          yr_pos = which(apply(data, 2, function(x)unique(nchar(x))) == 4)
          if(yr_pos == 1){
            fmt = "%Y-%m-%d %H:%M"
            aux <- mydt
          } else {
            aux = paste0(
              substr(mydt,7,10), substr(mydt,3,6), substr(mydt,1,2),
              substr(mydt,11,nchar(mydt)))
            fmt = "%Y-%m-%d %H:%M"
          }
        } else {                                                      # problemas com data sem padrão
          pos2 = which(nchar(data[,1]) == 2)
          cat('\n **** entrando no laço correto ****\n')
          mydt[pos2] = paste0(
            substr(mydt[pos2],7,10),substr(mydt[pos2],3,6), substr(mydt[pos2],1,2),
            substr(mydt[pos2],11,nchar(mydt[pos2])))
          aux <- mydt
          fmt = "%Y-%m-%d %H:%M"
          ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))
        }
        
        # acertando os dados
        aux1 <- data.frame(matrix(ncol = 8, nrow = length(aux), unlist(strsplit(aux, ",")), byrow = T))
        names(aux1) =  c("date",unlist(strsplit(label,","))[-1])
        
        # verificando dados de formato correto e ano no futuro não existente
        year.hj <- substr(Sys.Date(),1,4)
        aux1 <- aux1[!(substr(aux1$date,1,4) > year.hj),]
      }
      return(aux1)
    } else{
      return()
    } # if 2
 #} # if 1
} #função


