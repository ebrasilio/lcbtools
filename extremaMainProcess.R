#
### Sistema de processamento para extrema
  #

#---- Settings ----
rm(list=ls())

library(stringr)
library(openair)
library(ggplot2)
library(reshape2)

setwd('/data1/DATA/LCB/EXT/STORAGE/codes/extremaMainProcess/')





#---- Clima ----
source('clima_codes/mainClima2.R')
source('clima_codes/climaSplitVars.R')

tt <-  read.csv('/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample/C26clear_merge_H.txt',
                na.strings=c('NA','-9999'), stringsAsFactors=F)
tt$date <- as.POSIXct(tt$date, tz='GMT')
timePlot(selectByDate(tt, year=2017), names(tt)[c(5,8)], y.relation='free')

#---- Umidade do Solo ----



#---- Fluxos ----


#---- Radiação ----


#---- Perfil ----

#---- Check ----
#
#
library(scales)

# cday <- list.files('/data/ext/data', pattern='Ext_chuva_Dia', full.names=T, recursive=T)
# test.data  <- lapply(cday, 
#                         function(.file) 
#                           read.csv(.file, na.strings = c("-9999", "NA")))
# 
# a = Reduce(function(x, y) merge(x, y, all=TRUE), test.data)
a <- read.csv('/data/ext/out/Ext_chuva_Dia.csv')
b <- data.frame(date=as.POSIXct(a$date, tz='GMT'),
                apply(a[,-1], 2, function(x){
                  pos <- which(!is.na(x))
                  x[pos] <- 1
                  return(x)} ))

c <- b[, c(1, order(names(b))[1:(ncol(b)-1)])]



pdf("/data/ext/code/Data_Availability_final.pdf", width=12, height=8)

cc <- melt(c, id.vars="date")
cc$date <- as.Date(cc$date)
xlabs <- unique(cc$date[ which(substring(cc$date,6,10) %in% c('01-01','07-01'))])
p <-
  ggplot(cc, aes(x=date, y=value, colour=variable)) + geom_line(size=3) +
  facet_grid(variable~.) +
  ylim(c(0.9, 1.1)) +
  scale_y_continuous(breaks=1, labels=NULL) +
  scale_x_date(date_breaks='6 months',date_labels="%Y-%m-%d",
               date_minor_breaks='1 month',
               limits=as.Date(c("2014-05-01", "2019-12-31"))) +
  theme_bw() +
  #ggtitle(paste0('year ', i)) +
  theme(legend.position="none",
        strip.text.y=element_text(colour="blue", face='bold',angle=0),
        strip.background.y=element_rect(fill="white"),
        panel.background=element_rect(fill=NA),
        panel.grid.major.x=element_line(colour='black'))
print(p)

dev.off()


+----------------------------------------------------------------------------------
for(i in 2014:2019){
  cc <- melt(selectByDate(c, year=i), id.vars="date")
  cc$date <- as.Date(cc$date)
dten <- as.Date(cc$date[nrow(cc)]) 
  xlabs <- cc$date[ which(substring(cc$date,9,10) %in% c('01'))]
  p1 <- 
      ggplot(cc, aes(x=date, y=value, colour=variable)) + geom_line(size=3) +
      facet_grid(variable~.) +
      ylim(c(0.9, 1.1)) + 
      scale_y_continuous(breaks=1, labels=NULL) +
      scale_x_date(date_breaks='2 months',date_labels="%Y-%m-%d", 
                 date_minor_breaks='1 month',
                 limits=as.Date(c(cc$date[1], dten))  ) +
  
      # scale_x_date(date_breaks='6 months',date_labels="%Y-%m-%d", 
      #              date_minor_breaks='1 month', 
      #              limits=as.Date(c(paste0(i,"-01-01"), paste0(i,"-12-31")))) +
      theme_bw() +
      ggtitle(paste0('year ', i)) +
      theme(legend.position="none",
            strip.text.y=element_text(colour="blue", face='bold',angle=0),
            strip.background.y=element_rect(fill="white"),
            panel.background=element_rect(fill=NA),
            panel.grid.major.x=element_line(colour='black'))
  print(p1)
#}
dev.off()




timePlot(c, names(c)[-1], group=F, lwd=5, ylab="",
         ylim=c(0.9, 1.1), key.columns=5)
         




