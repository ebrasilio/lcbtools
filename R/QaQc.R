library(openair)
library(imputeTS)


#-------------------------------------------------------------------------------------------------
count_na_day <- function(mydata){
  dy = unique(substr(mydata$date,1,10))
  fail <- list()
  for(i in 1:length(dy)){
    aux <- selectByDate(mydata, dy[i], dy[i])
    fail[[i]] <- as.vector(apply(aux, 2, function(x){sum(is.na(x))}))
    do.call(cbind, fail)
  }
  fail <- data.frame(do.call(rbind, fail))
  names(fail) <- names(mydata)
  fail$date <- as.POSIXct(dy, tz = 'GMT')
  return(fail)
}
#---------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
fill_gap <- function(mydata){
  # menos que 10 gaps usar regressão linear
  # mais de 10 gaps replicar dado do dia anterior
  gap <- count_na_day(mydata)
  aux <- mydata
  
  for(i in 2:ncol(aux)){
    #cat(i,'\n')
    if(sum(gap[,i]) != 0 & (sum(gap[,i]) < (nrow(gap)*24))){
      aux[,i] <- as.vector(na.interpolation(dt[,i], option='spline'))
    }
  }
  return(aux)
}
#--------------------------------------------------------------------------------------- 
