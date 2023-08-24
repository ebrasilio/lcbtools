#---------------------------------------------------------------
#
#---------------------------------------------------------------

read_wxt <- function(inp = inp, sensor=1) {
  # inp_1 é a lista de arquivos do wxt para ler e ordenar
  alt <- c("0.5m","1m","2m","6m")
  
  mydt <- lapply(inp, function(.file){
    read.csv(.file, skip=4, header=F, na.strings=c("NAN","NA"),stringsAsFactors=F)
  })
  mydt <- do.call(rbind, mydt)
  names(mydt) <- c("date", paste(c("Dn","Dm","Dx","Sn","Sm","Sx","Ta","Tp","Ua",
                                   "Pa","Rc","Vs"), alt[sensor], sep='_'))
  mydt$date <- as.POSIXct(mydt$date, tz="GMT")
  out <- mydt[order(mydt$date),]
  
  return(out)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my_merge1 <- function(mydt1=wxt1, mydt2=wxt1_a){
  
  if (length(which(substr(mydt1$date, 18,19) != '00')) != 0){
    cat('mydt1 tem erro nos segundos, arrumando...')
    mydt1 <- timeAverage(mydt1, avg.time='1 min', 
                         start.date=paste0(substr(mydt1$date[1],1,17),'00'))
  }
  if (length(which(substr(mydt2$date, 18,19) != '00')) != 0){
    cat('mydt2 tem erro nos segundos, arrumando...')
    mydt2 <- timeAverage(mydt2, avg.time='1 min', 
                         start.date=paste0(substr(mydt2$date[1],1,17),'00'))
  }
  
  mydt_1m <- rbind(mydt1, mydt2)
  
  ref <- data.frame(date = seq.POSIXt(min(mydt_1m$date),max(mydt_1m$date), by = '1 min'))
  out <- merge(ref, mydt_1m, all.x=T, by='date')
  
  return(out)
}

#+++++++++++++++++++++++++++
# my plot, dada uma var plotar 4 níveis
#+++++++++++++++++++++++++++

my_plot <- function(mydt, lim=NULL){
  mydt <- melt(mydt, id.vars='date')
    if(!is.null(lim)){
      p1 <- 
        ggplot(mydt, aes(x=date, y=value, col=variable)) +
        geom_point() + 
        geom_line() + ggtitle(avg) + 
        ylim(lim)
    }else{
      p1 <- 
        ggplot(mydt, aes(x=date, y=value, col=variable)) +
        geom_point() + 
        geom_line() + ggtitle(avg)
    }  
  return(p1)
}
