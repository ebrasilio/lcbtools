#-----------------------------------------------------------------------#
# EMSB - 26/06/2017                                                     #
# Organizar dados de Fluxos e Radiação de todas as coletas válidas de   #
# Extrema                                                               #
#-----------------------------------------------------------------------#

rm(list=ls())

library(stringr)
library(openair)
library(fields)

path = '/data1/DATA/LCB/EXT/STORAGE/data/F/raw'

# arquivos existentes
inp = list.files(path, pattern = 'full_output', full.names = T)
dt <- lapply(inp, function(.file){ read.csv(.file, skip=3, na.strings ='-9999.0',
                                              stringsAsFactors = F, header=F)})
#lapply(dt, function(X){range(X[,'V2'])})

labels = str_split_fixed(readLines(inp[5],2)[2],',',110)
##  numero diferente de colunas 

flx <- do.call(rbind,dt)

#lapply(dt, dim)
names(flx) <- labels
flx <- data.frame(date=as.POSIXct(paste(flx$date, flx$time), tz = "GMT"),
                 flx[,-c(1:7)])
ref = data.frame(date = seq.POSIXt(min(flx$date), max(flx$date), by='30 min'))

flux = merge(ref, flx, by='date', all.x = T)

# plots
tt <- seq.POSIXt(as.POSIXct(min(substr(flx$date,1,10)), tz="GMT"), 
                 as.POSIXct(max(substr(flx$date,1,10)), tz="GMT")+7*24*60*60, by='7 days')

pdf('/data3/TAREFAS/emilia/extrema_2019/flx_semana.pdf')
for(i in 1:(length(tt)-1)){
    timePlot(selectByDate(flux,tt[i],tt[i+1]), names(flux)[c(4,6,8)], y.relation = 'free')
}
dev.off()

#filtros
flux$H[ flux$H > 500] <- NA
flux$H[ flux$H < -200] <- NA
flux$co2_flux [flux$co2_flux > 200 ] <- NA
flux$co2_flux [flux$co2_flux < -200 ] <- NA
flux$LE[ flux$LE > 1000] <- NA
flux$LE[ flux$LE < -500] <- NA


write.csv(flux, '../../data/F/raw/Flx_Extrema_allvars.csv', row.names = F,
          na = '-9999.0')

flux_princ = flux[,which(names(flux) %in% c("date","H","LE","co2_flux","u.","wind_speed","wind_dir"))]
write.csv(flux_princ,'../../data/F/Flx_Extrema.csv', row.names = F,
           na = '-9999.0')
# 
# write.csv(flux, '../../data/F/raw/Flx_Extrema_allvarsFilter.csv', row.names = F,
#           na = '-9999.0')

#----------------------------------------------------------
# pdf('/data3/TAREFAS/emilia/extrema_2019/flx.pdf', width=12)
# timePlot(flux_princ, names(flux_princ)[2:4], y.relation = 'free')
# dev.off()

#----------------------------------------------------------------------------------
# Net - dois formatos: instalado em 2017, antes disso não tem dados


a1 = list.files('/data1/DATA/LCB/EXT/origin','TOA5_XF',recursive = T, full.names=T)
a1 <- a1[!str_detect(a1, '2016') ] # removendo dados de 2016

#b1 <- list.files('/data1/DATA/LCB/EXT/origin','TOA5_EF',recursive = T, full.names=T)
#a1 <- c(a1, b1)

filea1 <- lapply(a1, function(.file)read.csv(.file, skip=4, na.strings="NAN",
                stringsAsFactors=F,header=F))

# numero de colunas distintos, separar em grupos
dim_a1 <- unlist(lapply(lapply(filea1, dim),`[`,2))
ll1 <- a1[which(dim_a1 == unique(dim_a1)[1])]
ll2 <- a1[which(dim_a1 == unique(dim_a1)[2])]
#ll3 <- a1[which(dim_a1 == unique(dim_a1)[3])]

# formato 1 
rad1 = do.call(rbind,filea1[which(dim_a1 == unique(dim_a1)[1])])
labels = str_split_fixed(readLines(a1[which(dim_a1 == unique(dim_a1)[1])[1]],
                                   2)[2],'\",\"',87)
names(rad1)= c("date", labels[-1])
rad1$date = as.POSIXct(rad1$date, tz="GMT")

# formato 2
rad2 = do.call(rbind,filea1[which(dim_a1 == unique(dim_a1)[2])] )
labels = str_split_fixed(readLines(a1[which(dim_a1 == unique(dim_a1)[2])[1]],
                                   2)[2],'\",\"',86)
names(rad2)= c("date", labels[-1])
rad2$date = as.POSIXct(rad2$date, tz="GMT")

# formato 3 a partir da coleta 30017
a1 <- list.files('/data1/DATA/LCB/EXT/origin','TOA5_EF',recursive = T, full.names=T)
filea1 <- lapply(a1, function(.file)read.csv(.file, skip=4, na.strings="NAN",
                                             stringsAsFactors=F,header=F))
rad3 = do.call(rbind,filea1)
labels = str_split_fixed(readLines(a1[1],2)[2],'\",\"',4)
names(rad3)= c("date", labels[-1])
rad3$date = as.POSIXct(rad3$date, tz="GMT")

               
# ajustando os pedaços
rad = merge(rad1, rad2, all=T)
rad = merge(rad, rad3, all=T)
ref = data.frame(date=seq.POSIXt(min(rad$date), max(rad$date), by='10 min'))
out= merge(ref, rad, by='date', all=T)

# =====================================
# Removendo dado ruim até 29/03/2017
#--------------------------------------
ref <- data.frame(date=seq.POSIXt(min(rad$date), as.POSIXct("2017-03-29 18:00:00", tz ='GMT'), by='10 min'))
out$Net_Avg[out$date %in% ref$date] <- NA

out30 = timeAverage(out, avg.time = '30 min', data.thresh = 0)
# write.csv(out30,'../../data/I/temp/Net_Extrema_allvars.csv', row.names = F,
#           na = '-9999.0')

out_net = out30[,c(1,2)]
write.csv(out_net,'/data1/DATA/LCB/EXT/STORAGE/data/I/Net_Extrema_30m.csv', row.names = F,
          na = '-9999.0')

#--------------------------------------------------------------------------------------------
# pdf("/data3/TAREFAS/emilia/extrema_2019/net_30min.pdf", width = 12)
# timePlot(out_net, names(out_net)[-1])
# dev.off()

source("functions/functions.R")
source("~/git/lcbtools/R/plots.R") # vp.layout

# ler dados de chuva da estação C20
# c20 = read.csv("/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample/C20clear_merge.txt")
# c20 = data.frame(date=as.POSIXct(c20$X, format="%Y-%m-%d %H:%M:%S", tz = "GMT"),
#                  c20[,-1])
# c20_m = timeAverage(c20, avg.time = "30 min")
# c20_acm = timeAverage(c20, avg.time = "30 min", statistic = "sum")
# c20_m$Rc.mm = c20_acm$Rc.mm
# c20_m$Ta.C[c20_m$Ta.C > 25] <- NA
# 
# flx2 = merge(flux[,c(1,4,6)], out30[, c(1,2)], by="date", all=T)
# flx2 = merge(flx2, c20_m[,c(1,5,7,8)], all=T)
# 
# pdf('c20.pdf',width=12)
# timePlot(c20, names(c20)[-1], y.relation = 'free',main='C20-2min_2017-2018', ylab = "")
# timePlot(c20_m, names(c20)[-1], y.relation = 'free',main='C20-30min_2017-2018', ylab = "")
# dev.off()
# 
# pdf("Plots.pdf", width=17)
# timePlot(selectByDate(flx,"15/5/2017","2/6/2017"), names(flx)[c(2:3,5)], group=T, 
#          lty=1, ylab="",main="Média 30min", lwd=c(2,2,2))
# 
# timePlot(selectByDate(flx,"15/5/2017","2/6/2017"), names(flx)[6], group=T, 
#          lty=1, ylab="", lwd=2)
# dev.off()


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



#timePlot(flx2, names(flx2)[-1], y.relation = 'free')

# cicle = timeVariation(flx2, names(flx2)[c(2:4)])$data$hour
# 
# p = 
#     ggplot(cicle, aes(x=hour, y=Mean, col=variable)) +
#     geom_line(size=1.2) + 
#     geom_point(size=2) +
#     ylab("W/m2") + xlab("Hora Local") + 
#     theme_bw() +
#     scale_x_continuous(breaks=seq(0,23,3)) +
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           legend.text = element_text(size=20),
#           axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.title.x = element_text(size=12,color="black"),
#           axis.title.y = element_text(size=12,color="black"),
#           panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#           panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# # calculando EVP diária
# media_dia = timeAverage(flx2, avg.time = "day", data.thresh = 0)
# media_dia$Net_Avg[media_dia$Net_Avg < 0] = NA
# media_dia_acm = timeAverage(flx2, avg.time = "day", statistic = "sum")
# media_dia$Rc.mm = media_dia_acm$Rc.mm
# 
# media_dia_acm_evp = timeAverage(selectByDate(flx2, hour=7:17), avg.time = "day", data.thresh = 0)
# media_dia_acm_evp$Evp = media_dia_acm_evp$LE * 0.01763
# media_dia = merge(media_dia, media_dia_acm_evp[,c(1,8)])
# 
# #names(media_dia)[4:8] = c("Saldo de Radiação","Chuva","Temperatura do Ar","UR","Evapotranspiração")
# a = melt(media_dia, id.vars="date")
# 
# library(grid)
# 
# grid.newpage()
# p1 = 
#     ggplot(a[a$variable %in% c("Chuva","Evapotranspiração"),]) +
#     geom_bar(aes(x=date, y=value, fill=variable), stat="identity", position="dodge") +
#     theme_bw() + ylab("mm/dia") + xlab("") +
#     facet_grid(variable ~., scales = "free", as.table = FALSE) +
#     #scale_x_datetime(breaks=date_breaks("3 days"),
#      #                limits = as.POSIXct(c("2017-05-15", "2017-06-01"), format="%Y-%m-%d",
#      #                                    tz="GMT"),
#      #                labels=date_format("%d/%b", tz="GMT")) +
#     theme(legend.position = "none",
#         legend.title = element_blank(),
#         legend.text = element_text(size=20),
#         axis.text.x = element_text(angle=0, vjust=0, size=15, colour= "black"),
#         axis.text.y = element_text(angle=0, vjust=0, size=15, colour= "black"),
#         axis.title.x = element_text(size=15,color="black"),
#         axis.title.y = element_text(size=15,color="black"),
#         strip.text = element_text(size=15),
#     panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#     panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# p11 =
#     ggplot(a[a$variable %in% c("Saldo de Radiação","Temperatura do Ar"),]) +
#     geom_line(aes(x=date, y=value, color=variable), size=1.5) +
#     geom_point(aes(x=date, y=value, color=variable), size=5) +
#     scale_color_brewer(palette = "Dark2") +
#     theme_bw() + ylab("W/m²                       °C") + xlab("") +
#     facet_grid(variable ~., scales = "free", as.table = FALSE) +
#     # scale_x_datetime(breaks=date_breaks("3 days"),
#     #                  labels=date_format("%d/%b", tz="GMT")) +
#     theme(legend.position = "none",
#           legend.title = element_blank(),
#           legend.text = element_text(size=20),
#           axis.text.x = element_text(angle=0, vjust=0, size=15, colour= "black"),
#           axis.text.y = element_text(angle=0, vjust=0, size=15, colour= "black"),
#           axis.title.x = element_text(size=15,color="black"),
#           axis.title.y = element_text(size=15,color="black"),
#           strip.text = element_text(size=15),
#           panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#           panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# p2 = 
#     ggplot(a[a$variable %in% c('Saldo de Radiação'),]) +
#     geom_line(aes(x=date, y=value, col=variable)) +
#     geom_point(aes(x=date, y=value, col=variable)) +
#     theme_bw() + ylab("W/m2") + xlab("") +
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           legend.text = element_text(size=20),
#           axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.title.x = element_text(size=12,color="black"),
#           axis.title.y = element_text(size=12,color="black"),
#           panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#           panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# p3 = 
#     ggplot(a[a$variable %in% c("Temperatura do Ar"),]) +
#     geom_line(aes(x=date, y=value, col=variable)) +
#     geom_point(aes(x=date, y=value, col=variable)) +
#     theme_bw() + ylab("°C") + xlab("") + ylim(c(14,20))+
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           legend.text = element_text(size=20),
#           axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.title.x = element_text(size=12,color="black"),
#           axis.title.y = element_text(size=12,color="black"),
#           panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#           panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# p4 = 
#     ggplot(a[a$variable %in% c("Temperatura do Ar"),]) +
#     geom_line(aes(x=date, y=value, col=variable)) +
#     geom_point(aes(x=date, y=value, col=variable)) +
#     theme_bw() + ylab("°C") + xlab("") + ylim(c(14,20))+
#     theme(legend.position = "bottom", 
#           legend.title = element_blank(),
#           legend.text = element_text(size=20),
#           axis.text.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.text.y = element_text(angle=0, vjust=0, size=14, colour= "black"),
#           axis.title.x = element_text(size=12,color="black"),
#           axis.title.y = element_text(size=12,color="black"),
#           panel.grid.major = element_line(colour = "black", linetype = "dotted",size = 0.3),
#           panel.grid.minor = element_line(colour = "black", linetype = "dotted",size = 0.3))
# 
# 
# source("~/git/lcbtools/R/plots.R")
# 
# pdf("ciclo_diurno.pdf", width=12)
#     timePlot(c20, names(c20)[-1], y.relation = 'free',main='C20-2min_2017-2018', ylab = "")
# 
#     print(p)
#     print(p1)
#     print(p2)
#     print(p3)
#     print(p11)
# dev.off()
# 
# pdf("mariane_data_chuva_evp.pdf",width=12, height = 10)
# vp.setup(2,1)
#     print(p1, vp = vp.layout(1,1))
#     print(p11, vp = vp.layout(2,1))
#     
# dev.off()
# 
# 
# timePlot(c20_m, names(c20_m)[6])






#---------------------------------
# flux, out_net

# flx2 <- merge(flux[,c(1,4,6)], out30[,c(1,2)], all = T)
# flx2$H_LE <- flx2$H + flx2$LE
# flx2$Res <- flx2$Net_Avg - flx2$H_LE
# 
# pdf('scatter_balanço.pdf', width=12)
# scatterPlot(flx2, x='H_LE', y = 'Net_Avg', linear = T)
# scatterPlot(flx2, x='H_LE', y = 'Net_Avg', linear = T, type = 'season')
# timePlot(flx2, names(flx2)[c(4,8,9)], group=F, y.relation = 'free')
# timePlot(flx2, names(flx2)[c(4,8,9)], group=T)
# dev.off()


#=============================================
# G
#=============================================

inp <- list.files('/data1/DATA/LCB/EXT/origin', pattern = "SHF", 
                  recursive=T,full.names = T)
inp <- inp[ !is.na(str_match(inp, "TOA5"))]
tt <- lapply(inp, function(.file)read.csv(.file, skip=4, na.strings="NAN",
                                   stringsAsFactors=F,header=F))
shf <- do.call(rbind, tt)
names(shf)= c("date", "V_min","TPainel","SHF1","SHF2","SHF3")
shf$date = as.POSIXct(shf$date, tz="GMT")
shf$G <- (shf$SHF1 + shf$SHF2+ shf$SHF3)/3
shf_30m <- timeAverage(shf, avg.time = '30 min')
    
write.csv(shf_30m,'/data1/DATA/LCB/EXT/STORAGE/data/I/G_30m.csv', 
          row.names = F, na= '-9999.0')
# pdf('/data3/TAREFAS/emilia/extrema_2019/G.pdf', width = 12)
# timePlot(shf, names(shf)[4:7])
# dev.off()
