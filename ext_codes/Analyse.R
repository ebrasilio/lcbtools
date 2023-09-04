#...........................................................#
# figuras para analise final de extrema                     #
#                                                           #
#...........................................................#

rm(list=ls())

library(openair)
library(ggplot2)
library(reshape2)
library(patchwork)

#colors
c25 <- c(
  "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", 
  "gold1", "skyblue2","#FB9A99",
  "brown", "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3","darkorange4")

source('/home/emilia/git/lcbtools/R/multPlot.R')

#pathV <- '/home/emilia/lcb_2021/Ext-Mariane/Dados_Extrema/Vertedores/'
pathV <- '/data3/Dados_Extrema/Vertedores/'
pathC <- '/data1/DATA/LCB/EXT/outExt/'
          
#---- chuva e vazão ----
# ler dados de Q1, Q1a, Q2, Q3 Q4 e Foz PossesQ
# concatenar todos e plotar mm/dia série toda e foco em 2022/2023

q1  <- read.table(paste0(pathV, 'Q1/Q1_chuva_D.txt'), header = TRUE, stringsAsFactors = FALSE)
q1$date <- as.POSIXct(q1$date, tz='GMT')
q1a <- read.table(paste0(pathV, 'Q1a/Q1a_chuva_D.txt'), header = TRUE, stringsAsFactors = FALSE)
q1a$date <- as.POSIXct(q1a$date, tz='GMT')
q2  <- read.table(paste0(pathV, 'Q2/Q2_chuva_D.txt'), header = TRUE, stringsAsFactors = FALSE)
q2$date <- as.POSIXct(q2$date, tz='GMT')
q3  <- read.table(paste0(pathV, 'Q3/Q3_chuva_D.txt'), header = TRUE, stringsAsFactors = FALSE)
q3$date <- as.POSIXct(q3$date, tz='GMT')
q4  <- read.table(paste0(pathV, 'Q4/Q4_chuva_D_2.txt'), header = TRUE, stringsAsFactors = FALSE)
q4$date <- as.POSIXct(q4$date, tz='GMT')
Foz <- read.table(paste0(pathV, 'FozPosses/FozPosses_chuva_D.txt'), header = TRUE, stringsAsFactors = FALSE)
Foz$date <- as.POSIXct(Foz$date, tz='GMT')
For <- read.table(paste0(pathV, 'Forjos/Forjos_chuva_D_2.txt'), header = TRUE, stringsAsFactors = FALSE)
For$date <- as.POSIXct(For$date, tz='GMT')

# merge dados
dt <- merge(q1[,c(1,2,4)], q1a[,c(1,2,4)], by = "date", all = T)
dt <- merge(dt, q2[,c(1,2,4)], by = "date", all = T)
dt <- merge(dt, q3[,c(1,2,4)], by = "date", all = T)
dt <- merge(dt, q4[,c(1,2,4)], by = "date", all = T)
dt <- merge(dt, Foz[,c(1,2,4)], by = "date", all = T)
dt <- merge(dt, For[,c(1,2,4)], by = "date", all = T)

# chuva medida sem correção
dt1 <- melt(dt[,c(1,3,5,7,9,11,13,15)],id.vars = 'date')
#dt1$mn <- substr(strftime(dt1$date, format="%b"),1,1)

if(as.numeric(strftime(dt1$date[1], format="%m")) > 7){
  start <- as.POSIXct(paste0(substr(dt1$date[1],1,5),"07-01"), tz='GMT')
} else {
  
}

pl1 <- ggplot(selectByDate(dt1, year=2022:2023), aes(x=date, y=value, col=variable)) + 
  geom_line() + 
  scale_color_manual(values = c25) + 
  scale_y_continuous(trans ='log10') +
  ylab('Vazão[mm/d][log10]') + xlab("") +
  scale_x_datetime(date_breaks = '1 months',
                   #limits = c(dt1$date[1], dt1$date[nrow(dt1)]),
                   date_minor_breaks = "1 month") +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.background = element_rect(fill=NA),
        axis.text.x= element_text(color="black", size=10,angle=45, vjust=0.5),
        axis.text.y= element_text(color="black", size=12, vjust=0.5),
        panel.border = element_rect(linetype = "solid", colour = "gray",fill=NA))+
  guides(color = guide_legend(nrow=1,byrow=TRUE)) 

pl11 <- ggplot(selectByDate(dt1, year=2022:2023), aes(x=date, y=value, col=variable)) + 
  geom_line() + 
  scale_color_manual(values = c25) + 
  #scale_y_continuous(trans ='log10') +
  ylab('Vazão[mm/d]') + #[log10]') + 
  xlab("") +
  scale_x_datetime(date_breaks = '1 months',
                   #limits = c(dt1$date[1], dt1$date[nrow(dt1)]),
                   date_minor_breaks = "1 month") +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key = element_rect(fill="white"),
        legend.background = element_rect(fill=NA),
        axis.text.x= element_text(color="black", size=10,angle=45, vjust=0.5),
        axis.text.y= element_text(color="black", size=12, vjust=0.5),
        panel.border = element_rect(linetype = "solid", colour = "gray",fill=NA))+
  guides(color = guide_legend(nrow=1,byrow=TRUE)) 

# chuva sem preencher/data1/DATA/LCB/EXT/outExt/Clima_por_variavel
rc <- read.csv(paste0(pathC,'Clima_por_variavel/Ext_Rc_Hora.csv'))
rc$date <- as.POSIXct(rc$date, tz='GMT')
rc <- timeAverage(rc, avg.time = '1 day', statistic = 'sum')

aux <- melt(rc[,c(1,13)], id.vars='date')
pl2 <- ggplot(selectByDate(aux, year=2022:2023), aes(x=date, y=value)) + 
       geom_bar(stat="identity", fill="black", colour="black") + 
       scale_y_reverse() +
       ylab('Chuva[mm/d]') + xlab("") +
       scale_x_datetime(date_breaks = '1 months',
                        date_minor_breaks = "1 month") +
       theme(legend.position = "none", 
             axis.text.x = element_blank(),
             axis.text.y= element_text(color="black", size=12, vjust=0.5),
             panel.border = element_rect(linetype = "solid", colour = "gray",fill=NA)) 

pdf(paste0(pathC,'out_plots_set2023.pdf'),width=12)
  # chuva e vazao log10
  pwrk<- pl2/pl1
  pwrk + plot_layout(heights = c(1,2))

  # chuva e vazao 
  pwrk<- pl2/pl11
  pwrk + plot_layout(heights = c(1,2))
dev.off()
