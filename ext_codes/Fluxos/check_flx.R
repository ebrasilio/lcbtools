#-----------------------------------------------------------------------#
# EMSB - 26/06/2017                                                     #
# Organizar dados de Fluxos e Radiação de todas as coletas válidas de   #
# Extrema                                                               #
# TODO acertar rad para ler o arquivo processado eapenas atualizar                                                                      #
#-----------------------------------------------------------------------#

rm(list=ls())

# library(openair)
# library(chron)
# library(ggplot2)
# library(scales)
# library(grid)

#função para ler os dados de fluxos processados pelo eddypro.
source("functions/functions.R")
source("~/git/lcbtools/R/plots.R") # vp.layout

path = "/data1/DATA/LCB/EXT/"
IrgaS = le_flx(path)

# filtros
IrgaS$H[which(IrgaS$H > 200 | IrgaS$H < -100)] <- NA
IrgaS$LE[which(IrgaS$LE > 400 | IrgaS$LE < -100)] <- NA
IrgaS$co2_flux[which(IrgaS$co2_flux > 40 | IrgaS$co2_flux < -40)] <- NA

# organizando dados para ...
flx_I = IrgaS[,c(1,4,6,8)]
names(flx_I)[-1] = paste0(names(flx_I)[-1],"_IRGS")
timePlot(flx_I, names(flx_I)[-1])

# ler dados de radiação 
rad = le_flx(path, pattern="TOA5_XF", skip=4)


# ler dados de chuva da estação C20
c20 = read.csv("/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample/C05clear_merge.txt")
c20 = data.frame(date=as.POSIXct(c20$X, format="%Y-%m-%d %H:%M:%S", tz = "GMT"),
                 c20[,-1])
c20_m = timeAverage(selectByDate(c20, "15/5/2017","1/6/2017"), avg.time = "30 min")
c20_acm = timeAverage(selectByDate(c20, "15/5/2017","1/6/2017"), avg.time = "30 min", statistic = "sum")
c20_m$Rc.mm = c20_acm$Rc.mm
c20_m$Ta.C[c20_m$Ta.C > 25] <- NA


flx = merge(flx_I[,1:4], rad[, c(1,85)], by="date", all=T)
names(flx)[5] = "Net"
flx = merge(flx, c20_m[,c(1,5,7)])

# 
# pdf("Plots.pdf", width=17)
# timePlot(selectByDate(flx,"15/5/2017","2/6/2017"), names(flx)[c(2:3,5)], group=T, 
#          lty=1, ylab="",main="Média 30min", lwd=c(2,2,2))
# 
# timePlot(selectByDate(flx,"15/5/2017","2/6/2017"), names(flx)[6], group=T, 
#          lty=1, ylab="", lwd=2)
# dev.off()


# Outs
# original - IrgaS
# write.csv(IrgaS, paste0(path,"STORAGE/data/F/raw/Flx_EddyPro_Original_15117.csv"), row.names=F)
# 
# # radiação flx_I + rad
# write.csv(flx, paste0(path,"STORAGE/data/F/Flx_Extrema.csv"), row.names=F)

#--------------------------------------------------------------------------------------
# plotar alta-frequencia do sistama de eddy-covariance

# source("/data3/TAREFAS/2017_SVG/budgets/codes/functions.R")
# budget = flx
# budget$Net_G = budget$Net * 0.98
# budget$H_LE = budget$H_IRGS + budget$LE_IRGS
# 
# # pulso fora 
# budget[which(budget$date == "2017-05-22 10:30:00"),] = NA
# 
# # plots:
# # timePlot(budget, names(budget)[c(7:8)], group=T)
# 
# 
# # dados diurnos apenas:
# scatterPlot(budget,x="Net_G",y="H_LE", ci.mod=T,linear=T, main="todos os dados",
#             pch = 18)
# # dados dias secos
# 
# a = budget[which(budget$Rc.mm == 0),]
# pdf("scater_estrema.pdf")
# 
# budget = selectByDate(budget,"21/5/2017","31/5/2017")
# a = selectByDate(a,"21/5/2017","31/5/2017")
# scatterPlot(selectByDate(budget, hour=7:17),x="Net_G",y="H_LE",
#             ci.mod=T,linear=T, main="Dados diurnos", pch = 18,
#             par.settings=list(fontsize=list(text=20)))
# 
# scatterPlot(selectByDate(a, hour=7:17),
#             x="Net_G",y="H_LE",
#             ci.mod=T,linear=T, main="Dados diurnos - sem chuva", pch = 18,
#             par.settings=list(fontsize=list(text=20)))
# dev.off()
# 
# a = timeAverage(budget[c(1,7,8)], id.vars = "date")
# a$hour = hours(a$date)
# 


#scatterHourly(mydata = a, x="Net_G", y="H_LE")


#--------------------------------------------------------------------------------------
# gerar diagnósticos


#--------------------------------------------------------------------------------------
names(flx) = c("date","H","Le","Co2","Net","Rain","Tar")

pos = which(substr(flx$date,1,10) == "2017-05-19")
flx$H[pos] = NA
flx$Le[pos] = NA

#flx_new = selectByDate(flx, "17/5/2017","1/6/2017")
cicle = timeVariation(flx, names(flx)[c(2:3,5)])$data$hour

# 15/05 à 31/05
p = 
    ggplot(cicle, aes(x=hour, y=Mean, col=variable)) +
    geom_line(size=1.2) + 
    geom_point(size=2) +
    ylab("W/m2") + xlab("Hora Local") + 
    theme_bw() +
    scale_x_continuous(breaks=seq(0,23,3)) +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.title.x = element_text(size=12,color="black"),
          axis.title.y = element_text(size=12,color="black"),
          panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
          panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))

# calculando EVP diária
media_dia = timeAverage(flx, avg.time = "day", data.thresh = 0)
media_dia$Net[media_dia$Net < 0] = NA
media_dia_acm = timeAverage(flx, avg.time = "day", statistic = "sum")
media_dia$Rain = media_dia_acm$Rain

media_dia_acm_evp = timeAverage(selectByDate(flx, hour=7:17), avg.time = "day", data.thresh = 0)
media_dia_acm_evp$Evp = media_dia_acm_evp$Le * 0.01763
media_dia = merge(media_dia, media_dia_acm_evp[,c(1,8)])

names(media_dia)[5:8] = c("Saldo de Radiação","Chuva","Temperatura do Ar","Evapotranspiração")
a = melt(media_dia, id.vars="date")

library(grid)

grid.newpage()
p1 = 
    ggplot(a[a$variable %in% c("Chuva","Evapotranspiração"),]) +
    geom_bar(aes(x=date, y=value, fill=variable), stat="identity", position="dodge") +
    theme_bw() + ylab("mm/dia") + xlab("") +
    facet_grid(variable ~., scales = "free", as.table = FALSE) +
    scale_x_datetime(breaks=date_breaks("3 days"),
                     limits = as.POSIXct(c("2017-05-15", "2017-06-01"), format="%Y-%m-%d",
                                         tz="GMT"),
                     labels=date_format("%d/%b", tz="GMT")) +
    theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        axis.text.x = element_text(angle=0, vjust=0, size=15, colour= "black"),
        axis.text.y = element_text(angle=0, vjust=0, size=15, colour= "black"),
        axis.title.x = element_text(size=15,color="black"),
        axis.title.y = element_text(size=15,color="black"),
        strip.text = element_text(size=15),
    panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
    panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))

p11 =
    ggplot(a[a$variable %in% c("Saldo de Radiação","Temperatura do Ar"),]) +
    geom_line(aes(x=date, y=value, color=variable), size=1.5) +
    geom_point(aes(x=date, y=value, color=variable), size=5) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() + ylab("W/m²                       °C") + xlab("") +
    facet_grid(variable ~., scales = "free", as.table = FALSE) +
    scale_x_datetime(breaks=date_breaks("3 days"),
                     labels=date_format("%d/%b", tz="GMT")) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(angle=0, vjust=0, size=15, colour= "black"),
          axis.text.y = element_text(angle=0, vjust=0, size=15, colour= "black"),
          axis.title.x = element_text(size=15,color="black"),
          axis.title.y = element_text(size=15,color="black"),
          strip.text = element_text(size=15),
          panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
          panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))

p2 = 
    ggplot(a[a$variable %in% c("Net"),]) +
    geom_line(aes(x=date, y=value, col=variable)) +
    geom_point(aes(x=date, y=value, col=variable)) +
    theme_bw() + ylab("W/m2") + xlab("") +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.title.x = element_text(size=12,color="black"),
          axis.title.y = element_text(size=12,color="black"),
          panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
          panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))

p3 = 
    ggplot(a[a$variable %in% c("Tar"),]) +
    geom_line(aes(x=date, y=value, col=variable)) +
    geom_point(aes(x=date, y=value, col=variable)) +
    theme_bw() + ylab("°C") + xlab("") + ylim(c(14,20))+
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.title.x = element_text(size=12,color="black"),
          axis.title.y = element_text(size=12,color="black"),
          panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
          panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))

p4 = 
    ggplot(a[a$variable %in% c("Tar"),]) +
    geom_line(aes(x=date, y=value, col=variable)) +
    geom_point(aes(x=date, y=value, col=variable)) +
    theme_bw() + ylab("°C") + xlab("") + ylim(c(14,20))+
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
          axis.title.x = element_text(size=12,color="black"),
          axis.title.y = element_text(size=12,color="black"),
          panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
          panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))


source("~/git/lcbtools/R/plots.R")

pdf("mariane_data_ciclo_diurno.pdf")
    print(p)
dev.off()

pdf("mariane_data_chuva_evp.pdf",width=12, height = 10)
vp.setup(2,1)
    print(p1, vp = vp.layout(1,1))
    print(p11, vp = vp.layout(2,1))
    
dev.off()


timePlot(c20_m, names(c20_m)[6])
