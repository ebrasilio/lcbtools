### Process Biomet file to EddyPro
library(openair)

source("/home/emilia/git/lcbtools/R/readLcbData.R")

#biomet existente
label <- unlist(strsplit(readLines("../2007-2016biomet.csv",2)[1],","))
label1 <- readLines("../2007-2016biomet.csv",2)[2]
dt <- read.csv("../2007-2016biomet.csv",skip=2, header=F, 
               colClasses = c("character","character", rep("numeric",3)),
               na.strings = "-9999.0", stringsAsFactors = F)
names(dt) <- label
date <- as.POSIXct(paste(dt$TIMESTAMP_1, dt$TIMESTAMP_2), 
                   format="%Y-%m-%d %H%M", tz="GMT")
dt <- data.frame(date, dt[,-c(1,2)])
st_new = dt$date[nrow(dt)]

# dados da base
sitio <- "svg"
a = list.files("/data1/BANCO/SITES/SVG1/", pattern="30m")
inp.files <- a[c(62,66,18)]
inp <- readLcbData(inp.files, sitio)
en_new = as.POSIXlt(Sys.Date(), tz="GMT")

#new = selectByDate(inp, st_new, en_new)
#names(new) = c("date", label[3:5])
names(inp) <- c("date",'Ta_1_1_1','UR_1_1_1','Pa_1_1_1')

#merge tudo
#dt.out <- merge(dt, new, all=T)

# check
#timePlot(dt.out, names(dt.out)[-1], y.relation="free")
# preencher dias com falhas....

# reshape para 10 min
inp <- selectByDate(inp, '2007-12-31','2017-12-31')

ref = data.frame(date=seq.POSIXt(as.POSIXlt(inp$date[1],tz='GMT'),
				  as.POSIXlt('2017-12-31',tz='GMT'),by='10 min'))

inp_10m <- merge(ref, inp, all.x=T, by = 'date')

library(zoo)
ref = data.frame(date=seq.POSIXt(as.POSIXlt('inp$date[1],tz='GMT'),
				  as.POSIXlt('2017-12-31',tz='GMT'),by='10 min'))

ztar = zoo(inp_10m$Ta_1_1_1, inp_10m$date)
zur = zoo(inp_10m$UR_1_1_1, inp_10m$date)
zpa = zoo(inp_10m$Pa_1_1_1, inp_10m$date)
z = merge(ztar, zur)
z = merge(z, zpa)

z$tar = na.approx(z$ztar)
z$ur  = na.approx(z$zur)
z$pa  = na.approx(z$zpa)]

dt.out = data.frame(date=inp_10m$date, z[,4:6])
dt.out$tar = dt.out$tar + 273.15
dt.out$pa  = dt.out$pa * 1000
names(dt.out) = names(inp_10m)


#format 
TIMESTAMP_1 <- substr(dt.out$date,1,10)
TIMESTAMP_2 <- paste0(substr(dt.out$date,12,13),substr(dt.out$date,15,16))
dt.out <- data.frame(TIMESTAMP_1, TIMESTAMP_2, dt.out[,-c(1)])

write.table(dt.out,'/data3/EDDYPRO_SVG/2017/2007-2017biomet_10min.csv', 
	row.names=F, quote=F, sep = ",")



#label1[1] <- paste0(label1[1],",H_1_1_1,LE_1_1_1,FC_1_1_1")
#label1[2] <- paste0(label1[2],",W/m2,W/m2,umol/m2s")









