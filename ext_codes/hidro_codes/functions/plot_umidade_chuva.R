#-----------------------------------------------------------------
# Main Script to read EXT climate and soil data and plot
#
#-----------------------------------------------------------------
#rm(list = ls())

library(openair)
source("~/git/lcbtools/R/soilHumidityFunctions.R")

soil_data <- "/data1/DATA/LCB/EXT/STORAGE/data/H/temp"
clim_data <- "/data1/DATA/LCB/EXT/STORAGE/data/C/merge"

#---------------------------------
# reading rain data
#---------------------------------
inp.files = list.files(clim_data, full.names = T, pattern="clear_merge_H")

test.data  <- lapply(inp.files, 
                     function(.file) 
                         read.csv(.file))
c_data = test.data[[1]]
date = as.POSIXct(c_data$X, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
c_data = data.frame(date=date, c_data[,-1]) 
chuva = data.frame(c_data[,c(1,5)])
names(chuva)[2] = substr(unlist(lapply(strsplit(inp.files[1], "/"),
                                       function(x){return(x[length(x)])})),1,3)
for(i in 2:length(test.data)){
    cat(i,"\n")
    aux = test.data[[i]]
    date = as.POSIXct(aux$X, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    aux = data.frame(date=date, aux[,-1]) 
    aux = data.frame(aux[,c(1,5)])
    names(aux)[2] = substr(unlist(lapply(strsplit(inp.files[i], "/"),
                                           function(x){return(x[length(x)])})),1,3)
    chuva = merge(chuva, aux, all=TRUE)
}


#----------------------------------------------
# soil humidity
#----------------------------------------------
files = list.files(soil_data, pattern = "merge_convert_H") # 2 original measured time
pdf("solo_painels_0-1.pdf", width = 12)
for(i in 1:length(files)){
    cat(substr(files[i],1,3),"\n")
    dt = read.csv(paste0(soil_data, "/",files[i]))[,c(1,8:13)]
    dt$date = as.POSIXct(dt$date, tz = "GMT")
    names(dt)[-1] = c("10","20","30","40","60","100")
    dt = merge(dt, chuva[,c("date", "C04")])
    names(dt)[substr(names(dt),1,1)=="C"] <- "Rain"
    print(plotProfileRain(dt))
}
dev.off()


    dt = read.csv(paste0(dir, "/",files[i]))[,c(1,8:13)]
    dt$date = as.POSIXct(dt$date, tz = "GMT")
    names(dt)[-1] = c("10","20","30","40","60","100")
    # teste = melt(dt, id.vars = "date")
    # teste$variable = as.numeric(as.character(teste$variable))
    # names(teste) = c("x", "y", "z")
    # tt = interp2xyz(selectByDate(teste, year=2016), data.frame=TRUE)
    # 
    # 
    # ggplot(tt, aes(x=x, y=y, z=z, fill=z))+ 
    #     geom_tile() + 
    #     coord_equal() +
    #     geom_contour(color = "white", alpha = 0.5) + 
    #     scale_fill_distiller(palette="Spectral", na.value="white") + 
    #     theme_bw()

    
    
    
    
    soil = selectByDate(dt, year=2015:2016)
    x <- soil$date
    y <- c(10,20,30,40,60,100)
    z <- as.matrix(soil[,c(2:7)], nrow = nrow(soil), by = row)
    exp <- expression(m^3/m^3)
    
    filled.contour(x, y, z, ylim = rev(range(y)),
                   zlim =c(0,1), nlevels = 15,
                   col = rev(tim.colors(20)), font.main=1, cex.main=1.5,
                   main = paste0(substr(files[i],1,3)," - ",year=2015),
                   ylab = "Profundidade (cm)", axes = TRUE, drawlabels=T,
                   key.title = title(main = exp),
                   key.axes = axis(4, seq(0, 1, by = 0.1)))
    abline(h= y, col = "grey", lty = 3, lwd = 2)
    tp <- timePlot(chuva, "C13", plot.type="h", lwd=3, col=4,
                   date.breaks = 12, ylab = "")$plot
    tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
                                   x=list(format="%b", cex=1.5, rot = 0)),
                 main = paste("Precipitação Acumulada - dia\n",i))
    
    
    # pdgdta <- selectByDate(pdg, year = i)
    # x <- pdgdta$date
    # y <- c(10,20,50,80,100,150,200,250)
    # z <- as.matrix(pdgdta[,c(2,3,6:11)], nrow = nrow(pdgdta), by = row)
    # exp <- expression(m^3/m^3)
    # 
    # filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0),
    #                zlim = c(0, 0.3), nlevels = 12,
    #                col = rev(tim.colors(15)), font.main=1, cex.main=1.5,
    #                main = paste("Umidade do Solo - PEG (dia)\n", i),
    #                ylab = "Profundidade (cm)",
    #                key.axes = axis(4, seq(0, 0.3, by = 0.05)))
    # mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
    # abline(h= y, col = "grey", lty =3)
    
    tp <- timePlot(pdgdta, "Chuva", plot.type="h", lwd=3, col=4,
                   date.breaks = 12, ylab = "")$plot
    tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
                                   x=list(format="%b", cex=1.5, rot = 0)),#)#,
                 main = paste("Precipitação Acumulada - dia\n",i))
    timePlot(pdgdta, names(pdgdta)[2:11])
    print(tp)
    
# head(faithful)
