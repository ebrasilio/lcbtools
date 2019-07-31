rm(list=ls())

library(openair)

inPath = "/data1/DATA/LCB/EXT/STORAGE/data/C/merge"
outPath= "/data1/DATA/LCB/EXT/STORAGE/codes/C_inicial/out"

inpFiles = list.files(inPath, "clear_merge.txt", full.names = T)

test.data  <- lapply(inpFiles,
                     function(.file)
                         read.csv(.file))
c_data = test.data[[1]]
date = as.POSIXct(c_data$X, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
c_data = data.frame(date=date, c_data[,-1])
# -------------------------------------------------------------------
bat  = data.frame(c_data[,c(1,2)])
wd   = data.frame(c_data[,c(1,3)])
pa   = data.frame(c_data[,c(1,4)])
rain = data.frame(c_data[,c(1,5)])
ws   = data.frame(c_data[,c(1,6)])
ta   = data.frame(c_data[,c(1,7)])
ur   = data.frame(c_data[,c(1,9)])
names(bat)=names(wd)=names(pa)=names(rain)=names(ws)=names(ta)=names(ur) =
    c("date", substr(inpFiles[1],nchar(inpFiles[1])-17,nchar(inpFiles[1])-15))

for(i in 2:length(test.data)){
    cat(i, "\n")
    aux  = test.data[[i]]
    date = as.POSIXct(aux$X, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    aux  = data.frame(date=date, aux[,-1])
    bat1  = data.frame(aux[,c(1,2)])
    wd1   = data.frame(aux[,c(1,3)])
    pa1   = data.frame(aux[,c(1,4)])
    rain1 = data.frame(aux[,c(1,5)])
    ws1   = data.frame(aux[,c(1,6)])
    ta1   = data.frame(aux[,c(1,7)])
    ur1   = data.frame(aux[,c(1,9)])
    names(bat1)=names(wd1)=names(pa1)=names(rain1)=names(ws1)=names(ta1)=names(ur1) =
        c("date", substr(inpFiles[i],nchar(inpFiles[i])-17,nchar(inpFiles[i])-15))
        bat  = merge(bat, bat1, all=T)
        wd   = merge(wd, wd1, all=T)
        pa   = merge(pa, pa1, all=T)
        rain = merge(rain, rain1, all=T)
        ws   = merge(ws, ws1, all=T)
        ta   = merge(ta, ta1, all=T)
        ur   = merge(ur, ur1, all=T)
}

ini = seq(as.Date("2014/8/1"),as.Date("2017/2/1"), by="month")
for(i in 1:(length(ini)-1)){
    pdf(paste0(outPath, "/Rain_",i, ".pdf"), width = 12)
    timePlot(selectByDate(rain, ini[i],ini[i+1]), names(rain)[2:17],
             y.relation="free",key.columns = 8)
    dev.off()
}
