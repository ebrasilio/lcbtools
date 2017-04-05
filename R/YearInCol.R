#-------------------------------------------------------------------------------------------
# EMSB - LCB - Ago/2015
# Função para separar arquivos de dados com anos em colunas 
#
#-------------------------------------------------------------------------------------------
library(openair)
library(timeDate)

YearInCol <- function(data, tyde = FALSE){
    dt <- data
    year <- unique(substr(dt$date, 1,4))
    dtC <- selectByDate(dt, year = year[1])
    names(dtC)[2] <- year[1]
    dtC$date <- as.POSIXct(paste0(substr(dtC$date, 1,2), "00", substr(dtC$date, 5,19)),
                           format = whichFormat(dtC$date), tz = "GMT")
    if(length(year) > 1){
        for(i in 2:length(year)){
            aux <- selectByDate(dt, year = year[i])
            names(aux)[2] <- year[i]
            aux$date <- as.POSIXct(paste0(substr(aux$date, 1,2), "00", substr(aux$date, 5,19)),
                                   format = whichFormat(aux$date), tz = "GMT")
            dtC <- merge(dtC, aux, by = "date", all = T)
        }
    }
    
    if (!tyde){
        return(dtC)    
    }else{
        return(melt(dtC, id.vars = "date"))
    }
    
} 



