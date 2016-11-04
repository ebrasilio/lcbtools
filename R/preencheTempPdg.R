#----------------------------------------------------
# Comparar temperaturas
#----------------------------------------------------

library(openair)
library(gdata)

source("/home/emilia/Git/Scripts/readLcbData.R")

inp <- c("pdg_026_TAR_Mastro.10m","pdg_027_UR_Mastro.10m","pdg_027_UR.10m","pdg_026_TAR.10m")

sitio <- "PDG1"    

dt <- readLcbData(inp, sitio)
timePlot(selectByDate(dt, "16/12/2014", "31/01/2015"), names(dt)[-1], group=T,lty=1)

scatterPlot(selectByDate(dt, "16/12/2014", "31/01/2015"), x="TAR_Mastro", y ="TAR",
            linear=T, ci=T)

scatterPlot(selectByDate(dt, "16/12/2014", "31/01/2015"), x="UR_Mastro", y ="UR",
            linear=T, ci=T)

# jogar fora dados ruins
dt$TAR_Calc <- dt$TAR
dt$TAR_Calc[ dt$TAR < 3 ] <- NA

dt$UR_Calc  <- dt$UR
dt$UR_Calc [ dt$TAR < 3] <- NA

pos <- which(is.na(dt$TAR_Calc) & !is.na(dt$TAR_Mastro))

dt$TAR_Calc[pos] <- dt$TAR_Mastro[pos]    * 0.9716 + 1.1809
dt$UR_Calc[pos]  <- dt$UR_Mastro[pos]  * 0.9971 + 4.4088
vars <- c("TAR_Calc", "UR_Calc")
names.out <-c("pdg_026_TAR_Calc.10m", "pdg_027_UR_Calc.10m") 
for(i in which(names(dt) %in% vars)){
    out <- data.frame(dt$date, dt[,i])
    names(out) <- names(dt)[c(1,i)]
    out$yr  <- strftime(out$date, format="%Y", tz="GMT")
    out$doy <- strftime(out$date, format="%j", tz="GMT")
    out$hh  <- strftime(out$date, format="%H", tz="GMT")
    out$mn  <- strftime(out$date, format="%M", tz="GMT")
    out1 <- data.frame(out[,c(3:6,2)], stringsAsFactors = FALSE)
    out1[,5] <- format(round(out1[,5], 8), nsmall=8)
    out1[which(substr(out1[,5], nchar(out1[,5])-1, nchar(out1[,5])) == "NA"),5] <- "-9999.00000000"
    
    write.fwf(out1, paste0("/data1/BANCO/SITES/",sitio,"/",names.out[i-5]), rownames=FALSE,
              colnames=FALSE, justify="right", sep= "", 
              width = c(4,4,4,4,16), na="-9999.00000000")
    
}


