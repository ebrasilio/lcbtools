#---------------------------------------------------------------
#
#---------------------------------------------------------------

read_wxt <- function(inp = inp, sensor=1) {
  # inp_1 é a lista de arquivos do wxt para ler e ordenar
  alt <- c("0.5m","1m","2m","6m")
  
  mydt <- lapply(inp, function(.file){
    read.csv(.file, skip=4, header=F, na.strings="NAN",stringsAsFactors=F)
  })
  mydt <- do.call(rbind, mydt)
  names(mydt) <- c("date", paste(c("Dn","Dm","Dx","Sn","Sm","Sx","Ta","Tp","Ua",
                                   "Pa","Rc","Vs"), alt[sensor], sep='_'))
  mydt$date <- as.POSIXct(mydt$date, tz="GMT")
  out <- mydt[order(mydt$date),]
  
  return(out)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

my_merge <- function(mydt1=wxt1, mydt2=wxt1_a){
  
  mydt <- rbind(mydt1, mydt2)
  mydt_10min <- timeAverage(mydt, avg.time='10 min')
  mydt_SS <- timeAverage(mydt, avg.time="10 min", statistic="sum")
    pos <- which(!is.na(str_match(names(mydt), "Rc")))
  mydt_10min[,pos] <- mydt_SS[,pos]
  ref <- data.frame(date = seq.POSIXt(min(mydt_10min$date),max(mydt_10min$date), by = '10 min'))
  out <- merge(ref, mydt_10min, all.x=T, by='date')
  
  return(out)
}

#+++++++++++++++++++++++++++
# my plot, dada uma var plotar 4 níveis
#+++++++++++++++++++++++++++

my_plot <- function(var = "Ta", avg = "10 min", lim=NULL){
  # var="Rc"; avg=month
  
   #if(var = 'Vh'){}
  #else{
   vars1 <- c(1,which(!is.na(str_match(names(wxt1), var))))
   vars2 <- c(1,which(!is.na(str_match(names(wxt1), var))))
   vars3 <- c(1,which(!is.na(str_match(names(wxt1), var))))
   vars4 <- c(1,which(!is.na(str_match(names(wxt1), var))))
  #}
  
  mydt <- merge(wxt1[,vars1], wxt2[,vars2])
  mydt <- merge(mydt, wxt3[,vars3])
  mydt <- merge(mydt, wxt4[,vars4])
  
  # média > 10 min
  if(avg != "10 min"){
    #if(var == 'Rc'){
    #mydt <- timeAverage(mydt, avg.time = avg, statistic = 'sum')
    #}else{
      mydt <- timeAverage(mydt, avg.time = avg)
    }
  #}
  
  # plot
  # básicas, chuva, vento
  mydt <- melt(mydt, id.vars="date")
  
  # if(var == "Rc"){
  #   p1 <- 
  #     ggplot(mydt, aes(x=date, y=value, fill=variable)) +
  #     geom_bar(stat="identity", position=position_dodge()) + 
  #     ggtitle(avg)
  #   
  # }else{
  #   
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
  # }
  return(p1)
}