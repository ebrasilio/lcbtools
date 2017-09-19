#-----------------------------------------------------------------------------------
# LCB-IAG-USP 12/2016
#-----------------------------------------------------------------------------------
library (openair)

#-----------------------------------------------------------------------------------
# deltaTConvert - apply the conversion on deltaT sensors
#               according by owner's manual
#   input:      mydt = data.frame 
#   args:       channels = names of columns to convert
#   soilTyp:    could be "organic" or "mineral"
#   relation:   could be "linear" or "plinomial"
#-----------------------------------------------------------------------------------

deltaTConvert =
    function(mydt, channels=NULL, soilType="organic", relation="linear") {
        if(is.null(channels)){
           stop("\nWARNING - \nno channels to convert!!\n")
        }
        
        aux = mydt[, which(names(mydt)%in%channels)]
        
        if(soilType == "organic"){
            if(relation == "linear"){
                aux2 = apply(aux, 2, function(x){
                    x = x / 1000
                    y = -0.121 + 0.575*x
                    ifelse(y < 0, NA, y)
                })
                
            }else if(relation=="polynomial"){
                aux2 = apply(aux, 2, function(x){
                    x <- x / 1000
                    y <- -0.023 - 0.72*x + 8.72*x^2 - 30.44*x^3 + 53.71*x^4 - 46.32*x^5 + 15.78*x^6
                    ifelse(y < 0, NA, y)
                })
            }
        }else if(soilType == "mineral"){
            if(relation == "linear"){
                aux2 = apply(aux, 2, function(x){
                    x = x / 1000
                    y = -0.146 + 0.528*x
                    ifelse(y < 0, NA, y)
                })
                
            }else if(relation=="polynomial"){
                aux2 = apply(aux, 2, function(x){
                    x <- x / 1000
                    y <- -0.057 - 0.66*x + 8*x^2 - 27.91*x^3 + 49.23*x^4 - 42.46*x^5 + 14.47*x^6
                    ifelse(y < 0, NA, y)
                })
            }
        }
        tt = data.frame(date=mydt$date, apply(aux2,2,function(x){return(round(x,4))}))
        
    return(tt)    
    }


#----------------------------------
# plotProfileRain - to plot profile variables and rain together
#   input:      mydt = data.frame
#   args:       channels = 
#   NOT FINISHED
#---------------------------------

plotProfileRain <- function(mydt, start=NULL, end=NULL){
    
    if(!is.null(start) & (!is.null(end)))mydt=selectByDate(mydt, start,end)  
    
    if(sum(names(mydt) %in% c("Rain"))){
        aa2  = melt(mydt[,c("date","Rain")], id.vars = "date")
        plRain <-
            ggplot(aa2, aes(x=date, y=value, color="black", fill="black")) + 
            geom_bar(stat="identity", position = "dodge", color="black") +
            ylab("") +
            #xlab("Dia") +
            #scale_x_datetime(date_labels = "%d-%b", date_breaks="10 day") +
            ggtitle(unique(aa2$variable)) +
            theme(plot.title=element_text(size=20, hjust = 0.5),
                  legend.position="none",#bottom",
                  legend.title=element_blank(),
                  legend.text=element_text(size = 15),
                  strip.text.x = element_text(size=15, face="bold"),
                  axis.text.x=element_text(size=20),
                  axis.text.y=element_text(size=20),
                  axis.title.x=element_text(size=20),
                  axis.title.y=element_text(size=20))
    }
    return(plot)
}





# exemplo de dados
#a = read.csv("/data1/DATA/LCB/EXT/STORAGE/data/H/merge/H04_raw.txt")
#b = deltaTConvert(a, names(a)[3:8])
#dt = b[,c(1,9:14)]

# função para plotar perfil de umidade do solo diário
# plotProfile(dt){
#     library(fields)
#     library(openair)
# 
#     days = unique(substr(dt$date,1,10))
# 
#     for(i in 1:length(days)){
#         soil = selectByDate(dt, days[i],days[i])
#         x <- soil$date
#         y <- c(10,20,30,40,60,100)
#         z <- as.matrix(soil[,c(2:7)], nrow = nrow(soil), by = row)
#         exp <- expression(m^3/m^3)
#         filled.contour(x, y, z, ylim = rev(range(y)),
#                        zlim =c(0,0.5), nlevels = 15,
#                        col = rev(tim.colors(20)), font.main=1, cex.main=1.5,
#                        main = paste0(days[i]),
#                        ylab = "Profundidade (cm)", axes = T,
#                        key.title = title(main = exp),
#                        key.axes = axis(4, seq(0, 1, by = 0.1)))
#         abline(h= y, col = "grey", lty = 3, lwd = 2)
#     }
# }
# 
# 

# 
# pdgdta <- selectByDate(pdg, year = i)
#     x <- pdgdta$date
#     y <- c(10,20,50,80,100,150,200,250)
#     z <- as.matrix(pdgdta[,c(2,3,6:11)], nrow = nrow(pdgdta), by = row)
#     exp <- expression(m^3/m^3)
# 
#     filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0),
#                    zlim = c(0, 0.3), nlevels = 12,
#                    col = rev(tim.colors(15)), font.main=1, cex.main=1.5,
#                    main = paste("Umidade do Solo - PEG (dia)\n", i),
#                    ylab = "Profundidade (cm)",
#                    key.axes = axis(4, seq(0, 0.3, by = 0.05)))
#     mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
#     abline(h= y, col = "grey", lty =3)
#     tp <- timePlot(pdgdta, "Chuva", plot.type="h", lwd=3, col=4,
#                    date.breaks = 12, ylab = "")$plot
#     tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
#                                    x=list(format="%b", cex=1.5, rot = 0)),#)#,
#                  main = paste("Precipitação Acumulada - dia\n",i))
#     timePlot(pdgdta, names(pdgdta)[2:11])
#     print(tp)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# filled.contour(time,depths,temp2, #col=(matlab.like2(28)),
#                ylab="Depth", xlab="Time",
#                plot.axes = { axis.Date(side=1,x=time,at=time,format="%b-%y"); axis(2) },
#                key.title=title(expression('  Temp ('*degree*'C)')),xaxs="i")
# 
# 
#     
# # pdf("DeltaT_Linear_calibration.pdf", width= 12)
# # for(i in 1:4){
# #     timePlot(mdataL[[i]], names(mdataL[[i]])[2:7])  
# #     soil <- mdataL[[i]][c(1,2:7)]
# #     x <- soil$date
# #     y <- c(10,20,30,40,60,100)
# #     z <- as.matrix(soil[,c(2:7)], nrow = nrow(soil), by = row)
# #     exp <- expression(m^3/m^3)
# #     
# #     filled.contour(x, y, z, ylim = rev(range(y)),
# #                    zlim = c(0, 1), nlevels = 15,
# #                    col = rev(tim.colors(20)), font.main=1, cex.main=1.5,
# #                    main = paste0("Umidade do Solo - DeltaT - IEE -  Sensor ", i,
# #                                  "\n 12 a 16/Dez"),
# #                    ylab = "Profundidade (cm)", axes = T,
# #                    key.title = title(main = exp),
# #                    key.axes = axis(4, seq(0, 1, by = 0.1)))
# #     abline(h= y, col = "grey", lty = 3, lwd = 2)
# # }
# # dev.off()
# }
# 
# 
# 
# 
# 
# 
# #--------------------------------------------------------------------------------------
# # função para plotar os dados
# {
# # rm(list = ls())
# # library(fields)
# # library(openair)
# # 
# # setwd("/data3/TAREFAS/2013_FDR/")
# # #Ban
# # #ban <- read.table("BAN_umidade_solo_dia.dat", header = F,
# # #                  colClasses = c(rep("character",4),rep("numeric",10)),
# # #                  na.strings = c("-9999.00000000","-7777.00000000"))
# # #date <- as.POSIXct(paste(ban$V1, ban$V2, ban$V3, ban$V4), 
# # #                   format = "%Y %j %H %M", tz = "GMT")
# # #ban <- data.frame(date, ban[,-c(1:4)])
# # #names(ban) <- c("date","10","20","40","70","80",
# # #                "100","150","220","293","Chuva") 
# # print(load("ban_solo.RData"))
# # 
# # 
# # 
# # 
# # 
# # pdf("ban_soil_pcp2003-2005.pdf", width= 12)
# # # Parte 1 de 2003 a 2005
# # ban <- hsoil
# # #for(i in 2003:2005){
# # 
# # bandta <- selectByDate(ban, "1/10/2003","30/10/2005")
# # x <- bandta$date
# # y <- c(10,20,40,70,100,150)
# # z <- as.matrix(bandta[,c(2:5,7,8)], nrow = nrow(bandta), by = row)
# # exp <- expression(m^3/m^3)
# # 
# # filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0), 
# #                zlim = c(0, 0.8), nlevels = 15,
# #                col = rev(tim.colors(16)), font.main=1, cex.main=1.5,
# #                main = "Umidade do Solo - Bananal (30 min )\n",
# #                ylab = "Profundidade (cm)",
# #                key.axes = axis(4, seq(0, 0.8, by = 0.1)))
# # mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
# # abline(h= y, col = "grey", lty =3)
# # tp <- timePlot(bandta, "RAIN", plot.type="h", lwd=3, col=4,
# #                date.breaks = 12, ylab = "")$plot
# # tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
# #                                x=list(format="%b", cex=1.5, rot = 0)),#)#,
# #              main = paste("Precipitação Acumulada - dia\n",i))
# # print(tp)
# # }
# # 
# # pdf("ban_soil_pcp2005-2013.pdf", width= 14)
# # # Parte 1 de 2006 a 2012
# # #for(i in 2006:2012){
# # bandta <- selectByDate(ban, "1/11/2005", "3/2/2013")
# # x <- bandta$date
# # y <- c(10,20,40,80,150,220,293)
# # z <- as.matrix(bandta[,c(2:4,6,8:10)], nrow = nrow(bandta), by = row)
# # exp <- expression(m^3/m^3)
# # 
# # filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0), 
# #                zlim = c(0, 0.8), nlevels = 15,
# #                col = rev(tim.colors(16)), font.main=1, cex.main=1.5,
# #                main = paste("Umidade do Solo - Bananal (30 min)\n"),
# #                ylab = "Profundidade (cm)",
# #                key.axes = axis(4, seq(0, 0.8, by = 0.1)))
# # mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
# # abline(h= y, col = "grey", lty =3)
# # tp <- timePlot(bandta, "RAIN", plot.type="h", lwd=3, col=4,
# #                date.breaks = 12, ylab = "mm")$plot
# # tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
# #                                x=list(format="%b%y", cex=1.5, rot = 30)),#)#,
# #              main = paste("Precipitação Acumulada - 30 min"))
# # print(tp)
# # dev.off()
# # 
# # ###########################################################################
# # # Pdg
# # pdg <- read.table("PDG_umidade_solo_dia.dat", header = F,
# #                   colClasses = c(rep("character",4),rep("numeric",11)),
# #                   na.strings = c("-9999.00000000","-7777.00000000"))
# # date <- as.POSIXct(paste(pdg$V1, pdg$V2, pdg$V3, pdg$V4), 
# #                    format = "%Y %j %H %M", tz = "GMT")
# # pdg <- data.frame(date, pdg[,-c(1:4)])
# # names(pdg) <- c("date","10","20","10","20","50","80","100",
# #                 "150","200","250","Chuva")
# # 
# # pdf("pdg_soil_pcp.pdf", width= 12)
# # 
# # for(i in 2001:2012){
# #     
# # }
# # dev.off()
# # 
# # 
# # pdf("pdg_soil50-100_pcp.pdf", width= 12)
# # 
# # for(i in 2001:2012){
# #     pdgdta <- selectByDate(pdg, year = i)
# #     x <- pdgdta$date
# #     y <- c(10,20,80,150,200,250)
# #     z <- as.matrix(pdgdta[,c(2,3,7,9:11)], nrow = nrow(pdgdta), by = row)
# #     exp <- expression(m^3/m^3)
# #     
# #     filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0), 
# #                    zlim = c(0, 0.3), nlevels = 12,
# #                    col = rev(tim.colors(15)), font.main=1, cex.main=1.5,
# #                    main = paste("Umidade do Solo - PEG (dia)\n", i),
# #                    ylab = "Profundidade (cm)",
# #                    key.axes = axis(4, seq(0, 0.3, by = 0.05)))
# #     mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
# #     abline(h= y, col = "grey", lty =3)
# #     tp <- timePlot(pdgdta, "Chuva", plot.type="h", lwd=3, col=4,
# #                    date.breaks = 12, ylab = "")$plot
# #     tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
# #                                    x=list(format="%b", cex=1.5, rot = 0)),#)#,
# #                  main = paste("Precipitação Acumulada - dia\n",i))
# #     timePlot(pdgdta, names(pdgdta)[2:11])
# #     print(tp)
# # }
# # dev.off()
# }
# 
# 
# 
# 
# #========================================================


