## main program


rm(list=ls())

library(ggplot2)
library(grid)    #plot.margins
library(openair)
library(scales)
library(reshape)

source("R/multPlot.R")

#-------------
# Reading Data
#-------------

ta <- read.csv("data/Ta_basefloresta5min.csv", sep=";", dec=".")
date <- as.POSIXct(ta$Data, format="%d/%m/%Y %H:%M", tz="GMT", usetz=T)
ta <- data.frame(date=date, Ta_base=ta$Ta_Base)

vd <- read.csv("data/ventoserieWXT.csv", sep=";", dec=".")
date <- as.POSIXct(vd$Data, format="%d/%m/%Y %H:%M", tz="GMT", usetz=T)
vd <- data.frame(date=date, vd[,-1])

#--------------
# Temperaturas
#--------------
tt <- merge(ta, vd[,c(1,4:5)], all=T)
names(tt)[3:4] <- c("Ta_topo","Ta_pasto")
# timePlot(tt, names(tt[-1]), pch = 20)
# dois dias para ter certeza que as coisas funcionaram
# pacote openair não "liga" os pontos quando há descontinuidade, se você não
# usar a opção "pch" (tipo de pontos), Ta_base, não aparece.
# timePlot(selectByDate(tt,"24/9/2015","25/9/2015"), names(tt[-1]), pch = 20)

#--------------
# Wind
#--------------
names(vd)[c(2,3)] <- c("wd","ws")
mydata <- data.frame(vd[,1:3])


#-------------------------------------------------------------------
# Plots para 3 dias
#-------------------------------------------------------------------
st <- min(tt$date, mydata$date)
en <- max(tt$date, mydata$date)

# vetor tempo de 3 dias
ini <- seq.POSIXt(st, en, by = "2 days")

pdf("out/plotThreeDays_Extrema2.pdf", width=12)
for(i in 1:(length(ini)-1)){
    # pannel to temp
    auxTa <- selectByDate(tt, ini[i], ini[i+1])
    auxTa <- melt(auxTa,id.vars = "date")   
    plTa <- 
        ggplot(auxTa, aes(x=date, y=value, color=variable)) +
        geom_line() + 
        geom_point() +
        ylab("Temp °C") + xlab("") +
        theme(legend.position = "top",
              legend.text = element_text(size=18, color="black"),
              legend.title= element_blank(),
              axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
              axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
              #axis.title.x = element_text(size=14,color="black"),
              axis.title.y = element_text(size=20,color="black"))
    ## pannel to wind
    auxWd <- selectByDate(mydata, ini[i], ini[i+1])#[1:10,]
    auxWd$u <- (-1 * auxWd$ws) * sin((auxWd$wd) * pi / 180.0)
    auxWd$v <- (-1 * auxWd$ws) * cos((auxWd$wd) * pi / 180.0)
    
    dw = auxWd[,c(1,ncol(auxWd)-1, ncol(auxWd))]
    
    
    # label para as legendas
    #t <- data.frame(date = dw$date[seq(1, nrow(mydata),6)])
    dwd_p <-
        ggplot(data=dw, aes(x=date, y=0)) +
        theme(plot.margin=unit(c(-0.7,0.8,0.5,0.9),'lines')) +
         geom_segment(aes(xend=date+u*3600, yend=v),
                     arrow=arrow(length=unit(0.25,'cm')), size = 1)+ ylim(c(-5,5)) +
        ylab("Vento m/s")+
        theme(axis.title.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
              axis.title.y = element_blank(),#element_text(size=14, colour= "black"),
              axis.text.x  = element_text(angle=0, vjust=0, size=14, colour= "black"),
              axis.text.y  = element_text(angle=0, vjust=0, size=14, colour= "black"))
    vp.setup(2,1)    
    print(plTa, vp = vp.layout(1,1))
    print(dwd_p, vp = vp.layout(2,1))
}
dev.off()




