#-----------------------------------------------------------------------------------
# EMSB - LCB-IAG-USP 09/12/2016
# Functions to remove broken lines and convert data from Delta-D Soil Humidity 
#   equipament em Conteint of Water.
#
# INPUT: raw file from field with deltaT data
# OUTPUT: data.frame with conversions
#
# TODO: use to non homogeneous soils.
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
        aux = data.frame(mydt, aux2)
        
    return(aux)    
    }





# função para conversão
#{
#-----------------------------------------------------------------------------------------------------
# Emilia M S Brasilio
#
# 

# rm(list = ls())
# 
# library(openair)
# library(fields)
# 
# #setwd("/data3/TAREFAS/GEOSENSORES")
# 
# hidro <- c("H01", "H02", "H03", "H04","H05")
# mdataP <- list()
# mdataL <- list()
# # 
# # pdf("simple_plots.pdf", width = 12)
# for(i in 1:length(hidro)){
#     fills <- list.files(path="STORAGE/data/H/merge", 
#                         pattern = paste0(hidro[i],"_raw"), 
#                         full.names = TRUE)
#     
#     test.data  <- lapply(fills[1:length(fills)],
#                          function(.file)
#                              read.csv(.file, sep = ",", dec = ".", header = TRUE))
#     test.data <- do.call(rbind, test.data)
#     date <- data.frame(date = as.POSIXct(test.data$date, format = "%Y-%m-%d %H:%M", tz = "GMT"))
#     test.data <- data.frame(date, test.data[,-1])
# 
# #   test.data$date <- data.frame(date=as.POSIXct(test.data$date, 
# #        format = "%Y-%m-%d %H:%M", tz = "GMT"))
# 
#     
#     # acertando a sequencia temporal
#     ref <- data.frame(date=seq(min(test.data$date), max(test.data$date),by = " min"))
#     #  if(length(ref) == 0) cat(paste0(fills[i], "  ruim \n"))
#     data <- merge(ref, test.data, by = "date", all = T)
#     write.csv(data[,1:8], file = paste0("HIDRO/OUT/",hidro[i],"_SoilMoist_Origin_25Jan.csv"), row.names = F)
#     timePlot(data, names(data)[2:8], main = paste0("Saidas Originais - ", hidro[i]))
# 
#     test.data = 
#     thetaPol <- apply(data[,3:8], 2, function(x){
#         x <- x / 1000
#         y <- -0.057 - 0.66*x + 8*x^2 - 27.91*x^3 + 49.23*x^4 - 42.46*x^5 + 14.47*x^6
#         ifelse(y < 0, NA, y)
#     })
#     thetaPol <- data.frame(date = data$date, thetaPol)
# 
#     thetaLin <- apply(data[,3:8], 2, function(x){
#         x <- x / 1000
#         y <- -0.146 + 0.528*x
#         ifelse(y < 0, NA, y)
#     })
#     thetaLin <- data.frame(date = data$date, thetaLin)
#     mdataP[[i]] <- thetaPol
#     mdataL[[i]] <- thetaLin
# 
#     write.csv(mdataP[[i]], file = paste0("HIDRO/OUT/",hidro[i],"_SoilMoist_Polin_12Dez-25Jan.csv"), row.names = F)
#     write.csv(mdataL[[i]], file = paste0("HIDRO/OUT/",hidro[i],"_SoilMoist_Linear_12Dez-25Jan.csv"), row.names = F)
# }
# # dev.off()
# # 
# # 
# # # Dados negativos indicam sensor desligado:
# # dif <- list()
# # for(i in 1:length(mdataP)){
# #     dif[[i]] <- mdataL [[i]] - mdataP[[i]]  
# #     dif[[i]]$date <- mdataL[[i]]$date
# #     timePlot(dif[[i]], names(dif[[i]])[2:7], main = i)
# # }
# # 
# # 
# # 
# # pdf("DeltaT_Polinomio_calibration.pdf", width= 12)
# # for(i in 1:5){
#     timePlot(mdataP[[i]], names(mdataP[[i]])[2:7])
#     soil <- mdataP[[i]][c(1,2:7)]
#     x <- soil$date
#     y <- c(10,20,30,40,60,100)
#     z <- as.matrix(soil[,c(2:7)], nrow = nrow(soil), by = row)
#     exp <- expression(m^3/m^3)
# 
#     filled.contour(x, y, z, ylim = rev(range(y)),
#                    zlim = c(0, 1), nlevels = 15,
#                    col = rev(tim.colors(20)), font.main=1, cex.main=1.5,
#                    main = paste0("Umidade do Solo - DeltaT - IEE -  Sensor ", i,
#                                  "\n 12 a 16/Dez"),
#                    ylab = "Profundidade (cm)", axes = T,
#                    key.title = title(main = exp),
#                    key.axes = axis(4, seq(0, 1, by = 0.1)))
#     abline(h= y, col = "grey", lty = 3, lwd = 2)
# }
# dev.off()
# # 
# # 
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
# #}
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
# #     pdgdta <- selectByDate(pdg, year = i)
# #     x <- pdgdta$date
# #     y <- c(10,20,50,80,100,150,200,250)
# #     z <- as.matrix(pdgdta[,c(2,3,6:11)], nrow = nrow(pdgdta), by = row)
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