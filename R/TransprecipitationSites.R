#---------------------------------------------------------------------------
# Emilia M S Brasilio
# Script para ler dados de interceptacao, buscar dados de chuva de medidas
# de 10 min , fazer analise da qualidade dos dados e fazer o balanco da 
# Intercepta
#---------------------------------------------------------------------------

rm(list = ls ())

sitio <- "SVG"

#------------------------------------------------------------------------
# Pacotes FunC'C5es e opC'C5es

library(gdata)              # read.xls
library(openair)
library(ggplot2)
library(reshape)
library(grid)
#library(timeDate)

pathIn <- paste0("/data1/DATA/LCB/",sitio, "/INTERCEPT/DATA/")
pathOu <- paste0("/data1/DATA/LCB/",sitio, "/INTERCEPT/OUT/")

#---------------------------------------------------------------------------
# FunC'C#o para criar multi-plot
vp.setup <- function(x,y){
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(x,y)))
}

vp.layout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col =y)
}

#--------------------------------------------------------------------------
# Parte 1 - Reading Data - Colectors and Pluviometer
#--------------------------------------------------------------------------
    
    aux <- list()
    inp <- list.files(pathIn, pattern = "*.xlsx", full.names = T)
    for(i in 1:length(inp)){
        tt <- read.xls(xls = inp[i], sheet = 1, na.strings = c("nan", "NAN", "-999"))
        year <- substr(inp[i], nchar(inp[i])-8, nchar(inp[i])-5)
        if(i == 1){
            aux[[i]] <- tt[,1] 
        }
        names(tt)[-1] <- as.character(
                           as.POSIXct(paste(year, names(tt)[-1]),
                                      format = "%Y %b_%d.%H%M", tz = "GMT"))
        aux[[i+1]] <- tt[,-1]
    }
    int <- do.call(cbind, aux)
    names(int)[1] <- "Colect"
    
    
    # converting to milimiter of rain
    Inter <- data.frame(Colect = c(as.character(rep(1:21,4)),"Mastro","Average","Colectors"),
                     apply(int[,-1], 2, function(x){
                         x[!is.na(x) & x > 0] <- as.numeric(x[!is.na(x) & x > 0]) / 0.535
                         x[length(x)+1]       <- mean(x[!is.na(x[1:84]) & x[1:84] >= 0], na.rm = T)
                         x[length(x)+1]       <- 40 - sum(!is.na(x[1:84]) & x[1:84] < 0)
                         return(x)
                        } ) ) 
    
    names(Inter) <- names(int)  ## force maintein names
    
    
    # Dates to integrate
    inidt <- "2012-12-24 12:25:00"
    icoletas <- as.POSIXct(c(inidt, names(int)[-c(1,ncol(int))]), tz = "GMT") + 10*60
    fcoletas <- as.POSIXct(names(int)[-c(1,ncol(int))], tz = "GMT")

    #-----------------------
    # Tower Pluviometer rain
    tower <- read.table(paste0("/data1/BANCO/SITES/", sitio, "/svg_006_RAIN_AWS.10m"),
                        header = F, na.strings = c("-9999.00000000","-7777.00000000"))
    tower <- data.frame(date = as.POSIXct(paste(tower$V1, tower$V2, tower$V3, tower$V4), 
                       format = "%Y %j %H %M", tz = "GMT"), TowerRain = tower$V5)
    
    Rain <- data.frame(matrix(NA, nrow = 2, ncol = length(icoletas)+1))
    names(Rain) <- names(Inter)
    Rain[1,1] <- "TowerRain"
    Rain[2,1] <- "Observations"
    
    # selecionar chuva no período e acumular
    pdf(paste0(pathOu, "SVG_chuvaTower_ck.pdf"), width = 12)
    for( i in 1:length(icoletas)){
        aux <- selectByDate(tower,icoletas[i], fcoletas[i])
        if(nrow(aux) > 0){
        Rain[1,i+1]<- sum(selectByDate(tower,icoletas[i], fcoletas[i])[,2], na.rm = T) 
        Rain[2,i+1]<- sum(is.na(selectByDate(tower,icoletas[i], fcoletas[i])[,2]))
        timePlot(aux, names(aux)[2], plot.type = "h", main = paste(icoletas[i],"\n",
                                                    fcoletas[i]), ylab = "", xlab = "")
        }
    }
    dev.off()

#----------------------------------------------
# Raw Data 
    tt <- rbind(Inter, Rain)

# ------------------------------------------------------------------------------
# removendo outliers e valores negativos
# Boxplots
#    pdf(paste0(pathOu,"/Boxplot_svg.pdf"))
#    for(k in 2:ncol(tt)){
#        boxplot(tt[1:84,k], xlab = names(tt)[k])
#    }
#    dev.off()
    tt1 <- data.frame(Coletor = tt[1:84,1], 
        apply(tt[1:84,-1], 2, function(x){
            x[x < 0] <- NA
            q <- quantile(x, na.rm = T)
            min <- q[2] - (q[4] - q[2]) * 1.5
            max <- q[4] + (q[4] - q[2]) * 1.5
            x[x > max] <- NA
            x[x < min] <- NA
            return(x)
        }))
    names(tt1) <- names(tt)
    tt1 <- rbind(tt1, tt[85:nrow(tt),])
    
#----------------------------------
    tt2 <- t(tt1[,-1])# excluir linhas onde media coletores for zero
    names(tt2) <- as.character(tt1[,1])
    
    tt2 <- tt2[-which(apply(tt2, 1, function(x){
        x[x < 0] <- NA
        return(sum(x[1:84], na.rm = T))}) == 0),]

    # desvio padrão das coletas
    tt4 <- apply(tt2, 1, function(x){
        x [x < 0] <- NA
        x <- sd(x, na.rm = T)
        return(x) } )

    plot(tt2[,86] ~ tt2[,88], xlab = "Torre", ylab = "Coletores")
    lm.col <- lm(tt2[,86]~tt2[,88])
    abline(lm.col, col =2)
    mtext(paste0("Coletor = Torre * ", round(coef(lm.col)[2],2)," + ", round(coef(lm.col)[1],2)), side  = 3)
    abline(c(1,1), col = 3)
    
#------------------------------------------------------------------------------------------------
# Figuras
    
    pdf(paste0(pathOu, "SVG_Ago2015_ColectsV2.pdf"), width = 12)
    for(i in 2:ncol(tt1)){
        lim = c(0, max(150,tt[1:87,i], na.rm = T))
        yat <- seq(lim[1],lim[2], 25)
        plot(tt1[1:84,i], ylim = lim, pch = 20, ylab = "mm", xlab = "", yaxt="n", xaxt = "n")
            axis(1, at = seq(0,90,10),lwd =2, cex.axis = 2)
            axis(2, at = yat, las = 2,lwd =2, cex.axis = 2)
            box(lwd=2)
            mtext(paste(names(tt)[i],paste("\n PluvTorre = ", round(tt1[88,i],2),
                                            " Coletor do Mastro=  ", round(tt1[85,i],2),
                                            " Media Coletores= ", round(tt1[86,i],2),
                                            "\n Falhas: Torre= ",  round(tt1[89,i],0),
                                            " Colectores= ", 40 - round(tt1[87,i],0),
                                            "Desvio-Padrão=", round(tt4[i],1))), side=3, cex = 1)
           
        abline(h = tt[85,i], col = "green", lwd = 1) # Coletor do mastro
        abline(h = tt[88,i], col = "red", lwd = 1)   # Pluviomêtro da Torre
        abline(h = tt[86,i], col = 1, lwd =1, lty = 2)
        legend("topleft", legend = c("PluvTorre","ColecMastro", "Media Coletores"), 
                col = c("red","green", "black"), pch = 20, cex = 1.5)
        }
    dev.off()


x1 <- as.numeric(t(tt1[which(tt$Colect == "Mastro"),]))
x2 <- as.numeric(t(tt1[which(tt$Colect == "Average"),]))
x3 <- as.numeric(t(tt1[which(tt$Colect == "TowerRain"),]))

pdf(paste0(pathOu,"SVG_geralV2.pdf"),width = 12)
plot(x2, col = 1, pch = 20, type = "b")
lines(x1, col = 2, pch = 20, type = "b")
lines(x3, col = 3, pch = 20, type = "b")
legend("topright",legend = c("MediaColetores", "PluviometroTorre", "ColetorTorre"),col = c(1,3,2), pch = 20)
dev.off()

# onde Coletor > Torre ?
pos <- which(tt[86,] > tt[88,])
names(tt)[pos-1]



#-----------------------------------------------------
# Stats
# 1- histograma dos coletores
# cada coluna representa a medida de cada um dos coletores
# e cada linha é uma coleta


histdt <- melt(tt2[,1:84])
histdt$value[which(histdt$value < 0)] <- NA
names(histdt)[2] <- "Coletores"

## histograma de todos os dados
ph_all <- 
    ggplot(histdt, aes(x = value)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "forestgreen") + 
    geom_density(alpha = 0.3, color = "black", fill = "white") + 
    theme( axis.text.y = element_text(face="bold", size=20),
           axis.text.x  = element_text(face="bold", size=20))

hist <- list()
for(i in as.numeric(unique(histdt$Coletores))){
    hist[[i]] <- 
        ggplot(histdt[histdt$Coletores == i,], aes(x = value)) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "forestgreen") + 
        geom_density(alpha = 0.3, color = "black", fill = "white") + 
        ggtitle(paste("Todos os Eventos Medidos - Coletor ", i)) +
        ggtitle(paste("Col ", i, " Nº=", 
                      sum(!is.na(histdt[histdt$Coletores == i,"value"]))))
}

## histograma da diferença
# diferenca normalizada
tt3 <- apply( tt2, 1, function(x){
                        x [x < 0] <- NA
                        x <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
                        return(x) } )

histdt <- melt(tt3)
names(histdt)[2] <- "Coletores"

ph_diffN <- ggplot(histdt, aes(x = value)) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "forestgreen") + 
        geom_density(alpha = 0.3, color = "black", fill = "white") + 
        theme( axis.text.y = element_text(face="bold", size=20),
           axis.text.x  = element_text(face="bold", size=20))

DNhist <- list()
for(i in as.numeric(unique(histdt$Coletores))){
    DNhist[[i]] <- 
        ggplot(histdt[histdt$Coletores == i,], aes(x = value)) +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "forestgreen") + 
        geom_density(alpha = 0.4, lwd = 1, fill = "white") +
        xlab("") + xlim(c(-5,5)) +
        ggtitle(paste("Col ", i, " Nº=", 
                      sum(!is.na(histdt[histdt$Coletores == i,"value"]))))
        
}

#------------------------------------------------------------------------
pdf(paste0(pathOu,"SVG_DN_hist.pdf"), width=14)
print(ph_all)
vp.setup(3,7)    
print(hist[[1]], vp = vp.layout(1,1))
print(hist[[2]], vp = vp.layout(2,1))
print(hist[[3]], vp = vp.layout(3,1))
print(hist[[4]], vp = vp.layout(1,2))
print(hist[[5]], vp = vp.layout(2,2))
print(hist[[6]], vp = vp.layout(3,2))
print(hist[[7]], vp = vp.layout(1,3))
print(hist[[8]], vp = vp.layout(2,3))
print(hist[[9]], vp = vp.layout(3,3))
print(hist[[10]], vp = vp.layout(1,4))
print(hist[[11]], vp = vp.layout(2,4))
print(hist[[12]], vp = vp.layout(3,4))
print(hist[[13]], vp = vp.layout(1,5))
print(hist[[14]], vp = vp.layout(2,5))
print(hist[[15]], vp = vp.layout(3,5))
print(hist[[16]], vp = vp.layout(1,6))
print(hist[[17]], vp = vp.layout(2,6))
print(hist[[18]], vp = vp.layout(3,6))
print(hist[[19]], vp = vp.layout(1,7))
print(hist[[20]], vp = vp.layout(2,7))
print(hist[[21]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(hist[[22]], vp = vp.layout(1,1))
print(hist[[23]], vp = vp.layout(2,1))
print(hist[[24]], vp = vp.layout(3,1))
print(hist[[25]], vp = vp.layout(1,2))
print(hist[[26]], vp = vp.layout(2,2))
print(hist[[27]], vp = vp.layout(3,2))
print(hist[[28]], vp = vp.layout(1,3))
print(hist[[29]], vp = vp.layout(2,3))
print(hist[[30]], vp = vp.layout(3,3))
print(hist[[31]], vp = vp.layout(1,4))
print(hist[[32]], vp = vp.layout(2,4))
print(hist[[33]], vp =vp.layout(3,4))
print(hist[[34]], vp = vp.layout(1,5))
print(hist[[35]], vp = vp.layout(2,5))
print(hist[[36]], vp = vp.layout(3,5))
print(hist[[37]], vp = vp.layout(1,6))
print(hist[[38]], vp = vp.layout(2,6))
print(hist[[39]], vp = vp.layout(3,6))
print(hist[[40]], vp = vp.layout(1,7))
print(hist[[41]], vp = vp.layout(2,7))
print(hist[[42]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(hist[[43]], vp = vp.layout(1,1))
print(hist[[44]], vp = vp.layout(2,1))
print(hist[[45]], vp = vp.layout(3,1))
print(hist[[46]], vp = vp.layout(1,2))
print(hist[[47]], vp = vp.layout(2,2))
print(hist[[48]], vp = vp.layout(3,2))
print(hist[[49]], vp = vp.layout(1,3))
print(hist[[50]], vp = vp.layout(2,3))
print(hist[[51]], vp = vp.layout(3,3))
print(hist[[52]], vp = vp.layout(1,4))
print(hist[[53]], vp = vp.layout(2,4))
print(hist[[54]], vp =vp.layout(3,4))
print(hist[[55]], vp = vp.layout(1,5))
print(hist[[56]], vp = vp.layout(2,5))
print(hist[[57]], vp = vp.layout(3,5))
print(hist[[58]], vp = vp.layout(1,6))
print(hist[[59]], vp = vp.layout(2,6))
print(hist[[60]], vp = vp.layout(3,6))
print(hist[[61]], vp = vp.layout(1,7))
print(hist[[62]], vp = vp.layout(2,7))
print(hist[[63]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(hist[[64]], vp = vp.layout(1,1))
print(hist[[65]], vp = vp.layout(2,1))
print(hist[[66]], vp = vp.layout(3,1))
print(hist[[67]], vp = vp.layout(1,2))
print(hist[[68]], vp = vp.layout(2,2))
print(hist[[69]], vp = vp.layout(3,2))
print(hist[[70]], vp = vp.layout(1,3))
print(hist[[71]], vp = vp.layout(2,3))
print(hist[[72]], vp = vp.layout(3,3))
print(hist[[73]], vp = vp.layout(1,4))
print(hist[[74]], vp = vp.layout(2,4))
print(hist[[75]], vp =vp.layout(3,4))
print(hist[[76]], vp = vp.layout(1,5))
print(hist[[77]], vp = vp.layout(2,5))
print(hist[[78]], vp = vp.layout(3,5))
print(hist[[79]], vp = vp.layout(1,6))
print(hist[[80]], vp = vp.layout(2,6))
print(hist[[81]], vp = vp.layout(3,6))
print(hist[[82]], vp = vp.layout(1,7))
print(hist[[83]], vp = vp.layout(2,7))
print(hist[[84]], vp = vp.layout(3,7))


print(ph_diffN)
vp.setup(3,7)    
print(DNhist[[1]], vp = vp.layout(1,1))
print(DNhist[[2]], vp = vp.layout(2,1))
print(DNhist[[3]], vp = vp.layout(3,1))
print(DNhist[[4]], vp = vp.layout(1,2))
print(DNhist[[5]], vp = vp.layout(2,2))
print(DNhist[[6]], vp = vp.layout(3,2))
print(DNhist[[7]], vp = vp.layout(1,3))
print(DNhist[[8]], vp = vp.layout(2,3))
print(DNhist[[9]], vp = vp.layout(3,3))
print(DNhist[[10]], vp = vp.layout(1,4))
print(DNhist[[11]], vp = vp.layout(2,4))
print(DNhist[[12]], vp = vp.layout(3,4))
print(DNhist[[13]], vp = vp.layout(1,5))
print(DNhist[[14]], vp = vp.layout(2,5))
print(DNhist[[15]], vp = vp.layout(3,5))
print(DNhist[[16]], vp = vp.layout(1,6))
print(DNhist[[17]], vp = vp.layout(2,6))
print(DNhist[[18]], vp = vp.layout(3,6))
print(DNhist[[19]], vp = vp.layout(1,7))
print(DNhist[[20]], vp = vp.layout(2,7))
print(DNhist[[21]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[22]], vp = vp.layout(1,1))
print(DNhist[[23]], vp = vp.layout(2,1))
print(DNhist[[24]], vp = vp.layout(3,1))
print(DNhist[[25]], vp = vp.layout(1,2))
print(DNhist[[26]], vp = vp.layout(2,2))
print(DNhist[[27]], vp = vp.layout(3,2))
print(DNhist[[28]], vp = vp.layout(1,3))
print(DNhist[[29]], vp = vp.layout(2,3))
print(DNhist[[30]], vp = vp.layout(3,3))
print(DNhist[[31]], vp = vp.layout(1,4))
print(DNhist[[32]], vp = vp.layout(2,4))
print(DNhist[[33]], vp =vp.layout(3,4))
print(DNhist[[34]], vp = vp.layout(1,5))
print(DNhist[[35]], vp = vp.layout(2,5))
print(DNhist[[36]], vp = vp.layout(3,5))
print(DNhist[[37]], vp = vp.layout(1,6))
print(DNhist[[38]], vp = vp.layout(2,6))
print(DNhist[[39]], vp = vp.layout(3,6))
print(DNhist[[40]], vp = vp.layout(1,7))
print(DNhist[[41]], vp = vp.layout(2,7))
print(DNhist[[42]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[43]], vp = vp.layout(1,1))
print(DNhist[[44]], vp = vp.layout(2,1))
print(DNhist[[45]], vp = vp.layout(3,1))
print(DNhist[[46]], vp = vp.layout(1,2))
print(DNhist[[47]], vp = vp.layout(2,2))
print(DNhist[[48]], vp = vp.layout(3,2))
print(DNhist[[49]], vp = vp.layout(1,3))
print(DNhist[[50]], vp = vp.layout(2,3))
print(DNhist[[51]], vp = vp.layout(3,3))
print(DNhist[[52]], vp = vp.layout(1,4))
print(DNhist[[53]], vp = vp.layout(2,4))
print(DNhist[[54]], vp =vp.layout(3,4))
print(DNhist[[55]], vp = vp.layout(1,5))
print(DNhist[[56]], vp = vp.layout(2,5))
print(DNhist[[57]], vp = vp.layout(3,5))
print(DNhist[[58]], vp = vp.layout(1,6))
print(DNhist[[59]], vp = vp.layout(2,6))
print(DNhist[[60]], vp = vp.layout(3,6))
print(DNhist[[61]], vp = vp.layout(1,7))
print(DNhist[[62]], vp = vp.layout(2,7))
print(DNhist[[63]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[64]], vp = vp.layout(1,1))
print(DNhist[[65]], vp = vp.layout(2,1))
print(DNhist[[66]], vp = vp.layout(3,1))
print(DNhist[[67]], vp = vp.layout(1,2))
print(DNhist[[68]], vp = vp.layout(2,2))
print(DNhist[[69]], vp = vp.layout(3,2))
print(DNhist[[70]], vp = vp.layout(1,3))
print(DNhist[[71]], vp = vp.layout(2,3))
print(DNhist[[72]], vp = vp.layout(3,3))
print(DNhist[[73]], vp = vp.layout(1,4))
print(DNhist[[74]], vp = vp.layout(2,4))
print(DNhist[[75]], vp =vp.layout(3,4))
print(DNhist[[76]], vp = vp.layout(1,5))
print(DNhist[[77]], vp = vp.layout(2,5))
print(DNhist[[78]], vp = vp.layout(3,5))
print(DNhist[[79]], vp = vp.layout(1,6))
print(DNhist[[80]], vp = vp.layout(2,6))
print(DNhist[[81]], vp = vp.layout(3,6))
print(DNhist[[82]], vp = vp.layout(1,7))
print(DNhist[[83]], vp = vp.layout(2,7))
print(DNhist[[84]], vp = vp.layout(3,7))
dev.off()
























# numero de vezes em que o coletor foi utilizado
tt2[,87]<-apply(tt2,1, function(x)sum(!is.na(x)))

# desvio em relação a media
#tt3 <- apply(tt[1:84,-1], 2, function(x){
#                         as.numeric(x) - mean(x)
#})

for(i in 1:84){
    tt3 <- tt[i,j] - tt[88,j]}






t((tt[c(2,86,88),]))

tt








plot( data.frame(tt[which(tt$Colect == "Average"),] ) )
    


86,
    88
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #------------------------------------------------------------------------------------------
# Parte 3 - VisC#o Geral de todos os dados

date <- as.POSIXct(names(tt[-1]), tz = "GMT")
dtFormated <- data.frame(vector(length = 4)) #, nrow = ncol(alldt)*nrow(alldt)))
dt <- list() 

for(i in 2:ncol(tt)){
    for(j in c(1:(nrow(tt)-3),88)){
        dtFormated[1] <- as.POSIXct(date[i-1], format = "%Y-%m-%d %H:%M%:%S", tz = "GMT")  # colect date
        dtFormated[2] <- tt[j, i]
        dtFormated[3] <- c(1:21,1:21,1:21,1:21, "Mastro", "AVerage", "Torre")[j]
        dtFormated[4] <- c(rep(1,21), rep(2,21), rep(3,21), rep(4,21),"Vertedor", "Torre", "Mastro")[j]
        if(j == 1){
            aux <- dtFormated[1,]
        }else {
            aux <- rbind(aux, dtFormated[1,])
        }
    } # end of colect
    dt[[i]] <- aux
}
dtFormated <- do.call( rbind, dt)
dt_all <- dtFormated
names(dt_all) <- c("date", "value", "Colector","Line")

dtVert  <- dt_all[dt_all$Colector == "Vertedor",]
dtTorre <- dt_all[dt_all$Colector == "Torre",]
dtMastro<- dt_all[dt_all$Colector == "Mastro",]
dtCol <- dt_all[(dt_all$Colector != "Vertedor" | dt_all$Colector != "Torre" | dt_all$Colector != "Mastro"),]

#dtPEG$value <- as.numeric(as.character(dtPEG$value))
#dtCol$value <- as.numeric(as.character(dtCol$value))

#dtCol$value[which(dtCol$value > 300)] <- dtCol$value[which(dtCol$value > 300)]/10 # 878 erro de medicao
dtCol$date   <- as.character(dtCol$date)
dtVert$date  <- as.character(dtVert$date)
dtTorre$date <- as.character(dtTorre$date)
dtMastro$date <- as.character(dtMastro$date)

# selecionar dados a partir de 02/10/2014, primeira coleta do mastro
dtCol  <- selectByDate(dtCol, "15/10/2014","24/1/2015")
dtCol$value[which(dtCol$value < 0)] <- NA

dtVert <- selectByDate(dtVert, "15/10/2014","24/1/2015")
dtTorre <- selectByDate(dtTorre, "15/10/2014","24/1/2015")
dtMastro <- selectByDate(dtMastro, "15/10/2014","24/1/2015")

# plot
p <- ggplot(data = dtCol, aes(x=date, y = value))
p1 <- 
    p +
    layer(data = dtCol, maping = aes(x = date, y = value), 
          geom = "point", width = 1) +
    #scale_y_discrete(limits = c(0,100))
    theme(axis.title.x = element_text(face = "bold", size = 15),
          axis.text.x  = element_text(angle = 90, size = 12)) 

p2 <- 
    p1 +
    geom_point (data = dtVert, aes(x= date, y= value), colour = "red", size = 2) +
    geom_point (data = dtTorre, aes(x= date, y= value), colour = "green", size = 2)+
    geom_point (data = dtMastro, aes(x= date, y= value), colour = "blue", size = 2)


pdf("PEG/OUT/All_colectsPEG_2014-2015-V05.pdf")
   print(p2)
dev.off()




#------------------------------------------------------------------------------------------------
# ANALISES - REGRESSCO E HISTOGRAMAS
#--------------------------------------------------------------------------------------------
# Tabelas com valores mC)dios dos coletores para plots

# mC)dia de cada coleta
T_alldt <- data.frame(t(alldt[1:84,]))
names(T_alldt) <- as.character(1:84)
T_alldt$Mean <- apply(T_alldt, 1, mean)
T_alldt$sd <- apply(T_alldt, 1, sd)

# acrescentar torre e vertedor e mastro
# TABELA BCSICAS COM TODAS AS COLETAS
T_alldt <- data.frame(T_alldt, data.frame(t(alldt[85:91,])))
#names(T_alldt)[1:91] <- as.character(1:91)

# excluir eventos nulos - TABELAS COM CHUVAS NOS COLETORES
# Histogramas

T_alldt2 <- T_alldt[which(T_alldt$Mean != 0),]
histdt <- melt(T_alldt2)
names(histdt)[1] <- "Coletores"


## histograma de todos os dados
ph_all <- ggplot(histdt, aes(x = value)) +
          geom_histogram() + 
          theme( axis.text.y = element_text(face="bold", size=20),
                 axis.text.x  = element_text(face="bold", size=20))

hist <- list()
for(i in as.numeric(unique(histdt$Coletores))){
    hist[[i]] <- 
        ggplot(histdt[histdt$Coletores == i,], aes(x = value)) +
        geom_histogram() + xlab("") + 
        ggtitle(paste("Todos os Eventos Medidos - Coletor ", i))
}


#-----------------------------------------------------------------
# Tabela das diferenC'as:

T_diff <- data.frame(t(apply(T_alldt2, 1, function(x){
                x <- (x[1:84] - x[85])
                return(x)          })))
names(T_diff) <- substr(names(T_diff),2,nchar(names(T_diff)))
histdt <- melt(T_diff)
names(histdt)[1] <- "Coletores"


ph_diff <- ggplot(histdt, aes(x = value)) +
geom_histogram(aes(y = ..density..),bindwith = 0.5 ) + 
  theme( axis.text.y = element_text(face="bold", size=20),
         axis.text.x  = element_text(face="bold", size=20))



Dhist <- list()
for(i in as.numeric(unique(histdt$Coletores))){
    Dhist[[i]] <- 
        ggplot(histdt[histdt$Coletores == i,], aes(x = value)) +
        geom_histogram() + xlab("") + 
        ggtitle(paste("Medidas - M?dia - Coletor ", i))
}


# diferenC'a normalizada
T_diffNorm <- data.frame(t(apply(T_alldt2, 1, function(x){
                                   x <- (x[1:84] - x[85])/x[86]
                                   return(x)          })))
names(T_diffNorm) <- as.character(1:84)
histdt <- melt(T_diffNorm)
names(histdt)[1] <- "Coletores"

ph_diffN <- ggplot(histdt, aes(x = value)) +
  geom_histogram() + 
  theme( axis.text.y = element_text(face="bold", size=20),
         axis.text.x  = element_text(face="bold", size=20))





DNhist <- list()
for(i in as.numeric(unique(histdt$Coletores))){
    DNhist[[i]] <- 
        ggplot(histdt[histdt$Coletores == i,], aes(x = value)) +
        geom_histogram() + xlab("") + xlim(c(-5,5))+
        ggtitle(paste("Coletor ", i))
}


# histograma de valores
pdf("PEG/OUT/histogramas.pdf")
for(i in 1:84){
    print(hist[[i]])
}
for(i in 1:84){
    print(Dhist[[i]])   
}
for(i in 1:84){
    print(DNhist[[i]])
}
    dev.off()

pdf("SVG/OUT/DNhist.pdf", width=14)
print(ph_all)
print(ph_diff)
print(ph_diffN)
vp.setup(3,7)    
print(DNhist[[1]], vp = vp.layout(1,1))
print(DNhist[[2]], vp = vp.layout(2,1))
print(DNhist[[3]], vp = vp.layout(3,1))
print(DNhist[[4]], vp = vp.layout(1,2))
print(DNhist[[5]], vp = vp.layout(2,2))
print(DNhist[[6]], vp = vp.layout(3,2))
print(DNhist[[7]], vp = vp.layout(1,3))
print(DNhist[[8]], vp = vp.layout(2,3))
print(DNhist[[9]], vp = vp.layout(3,3))
print(DNhist[[10]], vp = vp.layout(1,4))
print(DNhist[[11]], vp = vp.layout(2,4))
print(DNhist[[12]], vp = vp.layout(3,4))
print(DNhist[[13]], vp = vp.layout(1,5))
print(DNhist[[14]], vp = vp.layout(2,5))
print(DNhist[[15]], vp = vp.layout(3,5))
print(DNhist[[16]], vp = vp.layout(1,6))
print(DNhist[[17]], vp = vp.layout(2,6))
print(DNhist[[18]], vp = vp.layout(3,6))
print(DNhist[[19]], vp = vp.layout(1,7))
print(DNhist[[20]], vp = vp.layout(2,7))
print(DNhist[[21]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[22]], vp = vp.layout(1,1))
print(DNhist[[23]], vp = vp.layout(2,1))
print(DNhist[[24]], vp = vp.layout(3,1))
print(DNhist[[25]], vp = vp.layout(1,2))
print(DNhist[[26]], vp = vp.layout(2,2))
print(DNhist[[27]], vp = vp.layout(3,2))
print(DNhist[[28]], vp = vp.layout(1,3))
print(DNhist[[29]], vp = vp.layout(2,3))
print(DNhist[[30]], vp = vp.layout(3,3))
print(DNhist[[31]], vp = vp.layout(1,4))
print(DNhist[[32]], vp = vp.layout(2,4))
print(DNhist[[33]], vp =vp.layout(3,4))
print(DNhist[[34]], vp = vp.layout(1,5))
print(DNhist[[35]], vp = vp.layout(2,5))
print(DNhist[[36]], vp = vp.layout(3,5))
print(DNhist[[37]], vp = vp.layout(1,6))
print(DNhist[[38]], vp = vp.layout(2,6))
print(DNhist[[39]], vp = vp.layout(3,6))
print(DNhist[[40]], vp = vp.layout(1,7))
print(DNhist[[41]], vp = vp.layout(2,7))
print(DNhist[[42]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[43]], vp = vp.layout(1,1))
print(DNhist[[44]], vp = vp.layout(2,1))
print(DNhist[[45]], vp = vp.layout(3,1))
print(DNhist[[46]], vp = vp.layout(1,2))
print(DNhist[[47]], vp = vp.layout(2,2))
print(DNhist[[48]], vp = vp.layout(3,2))
print(DNhist[[49]], vp = vp.layout(1,3))
print(DNhist[[50]], vp = vp.layout(2,3))
print(DNhist[[51]], vp = vp.layout(3,3))
print(DNhist[[52]], vp = vp.layout(1,4))
print(DNhist[[53]], vp = vp.layout(2,4))
print(DNhist[[54]], vp =vp.layout(3,4))
print(DNhist[[55]], vp = vp.layout(1,5))
print(DNhist[[56]], vp = vp.layout(2,5))
print(DNhist[[57]], vp = vp.layout(3,5))
print(DNhist[[58]], vp = vp.layout(1,6))
print(DNhist[[59]], vp = vp.layout(2,6))
print(DNhist[[60]], vp = vp.layout(3,6))
print(DNhist[[61]], vp = vp.layout(1,7))
print(DNhist[[62]], vp = vp.layout(2,7))
print(DNhist[[63]], vp = vp.layout(3,7))
vp.setup(3,7)    
print(DNhist[[64]], vp = vp.layout(1,1))
print(DNhist[[65]], vp = vp.layout(2,1))
print(DNhist[[66]], vp = vp.layout(3,1))
print(DNhist[[67]], vp = vp.layout(1,2))
print(DNhist[[68]], vp = vp.layout(2,2))
print(DNhist[[69]], vp = vp.layout(3,2))
print(DNhist[[70]], vp = vp.layout(1,3))
print(DNhist[[71]], vp = vp.layout(2,3))
print(DNhist[[72]], vp = vp.layout(3,3))
print(DNhist[[73]], vp = vp.layout(1,4))
print(DNhist[[74]], vp = vp.layout(2,4))
print(DNhist[[75]], vp =vp.layout(3,4))
print(DNhist[[76]], vp = vp.layout(1,5))
print(DNhist[[77]], vp = vp.layout(2,5))
print(DNhist[[78]], vp = vp.layout(3,5))
print(DNhist[[79]], vp = vp.layout(1,6))
print(DNhist[[80]], vp = vp.layout(2,6))
print(DNhist[[81]], vp = vp.layout(3,6))
print(DNhist[[82]], vp = vp.layout(1,7))
print(DNhist[[83]], vp = vp.layout(2,7))
print(DNhist[[84]], vp = vp.layout(3,7))

dev.off()
#-------------------------------------------------------------------------
# como exportar tabelas em excell
#wb <- loadWorkbook("PEG/OUT/t_alldt.xlsx", create = TRUE)
#createSheet(wb, name = "Coletores")
#writeWorksheet(wb, T_alldt, sheet= "Coletores")
#saveWorkbook(wb)


#-------------------------------------------------------------------------------
# Todos os dados de Todos os coletores

T_alldt2 <- T_alldt[which(T_alldt$Mean != 0),1:84]
histdt <- melt(T_alldt2)

ph_all <- ggplot(histdt, aes(x = value)) +
  geom_histogram(aes(y = ..density..),bindwith = 0.5 ) 

# ph_diff <- 
# 
# ggplot(hdtCol, aes(x = date, y = value)) +
#   geom_histogram()



# Regressao Vertedor e Coletores
# Regressao Torre e Coletores
# Regressao Mastro e Coletores
# Regressao Vertedor e Torre
# RegressC#o Vertedor X Torre
# RegressC#o Vertedor X Mastro
# tabela Mastro X Coletor Mastro


pdf("PEG/OUT/Regressao2.pdf")

# excluir eventos com falha de dados na Torre nos horC!rios de chuva
# Vertedor X coletores
posT <- which(T_alldt$Mean == 0) # excluidos 45 eventos
mean_col2 <- T_alldt[-posT,]
names(mean_col2)[87] <- "Torre"
scatterPlot(mean_col2, x = "Torre", y = "Mean", pch = 20, ylab = "Coletores",
            mod.line = TRUE, linear = TRUE, xlim = c(-10,130), ylim = c(-10,130))

# Torre X Coletores # excluido 65 eventos
posT <- c(63:66, 69:70, 84, 89, 92, 97:101, which(T_alldt$Mean == 0 | T_alldt$Tower1 == 0))
mean_col2 <- T_alldt[-posT,]
lm(mean_col2$Mean ~ mean_col2$Tower1)
scatterPlot(mean_col2, x = "Tower1", y = "Mean", pch = 20, ylab = "Coletores",
                              mod.line = TRUE, linear = TRUE)

# Vertedor x Torre
scatterPlot(mean_col2, x = "Tower1", y = "Vert", pch = 20, ylab = "Vertedor",
            mod.line = TRUE, linear = TRUE)


# Vertedor x Torre
scatterPlot(mean_col2, x = "Tower1", y = "Mastro", pch = 20, ylab = "Mastro",
            mod.line = TRUE, linear = TRUE)

dev.off()                  

#--------------------------------------------------------------------------------------------
# Acumulados ano hidrolC3gico
# Todas as coletas (nC#o exclui onde hC! falhas da torre ainda)

Total1 <- data.frame(date = rownames(T_alldt), T_alldt[,c(85,87,88,89,90)])
rownames(Total1) <- seq(1, nrow(Total1),1)
Total1$date <- as.POSIXct(Total1$date, format = whichFormat(Total1$date), tz = "GMT")
yr <- as.numeric(unique(substr(Total1$date,1,4)))

Hacm <- list()
for(i in 1:length(yr)){
    aux <- selectByDate(Total1, paste0("1/10/",yr[i]-1), paste0("30/9/",yr[i]))
    aux2 <- data.frame(date = aux$date, 
               apply(aux[,-1], 2, function(x){
                   x[is.na(x)] <- 0
                   return(x)}))
    Hacm[[i]] <- data.frame(date = aux2$date, 
                            apply(aux2[,-1], 2, cumsum))
}
Hacm <- do.call(rbind,Hacm)
pdf("PEG/OUT/acm_hidrologico_peg_2013-2015.pdf", width =12)
timePlot(Hacm, names(Hacm)[-1], group = T, lty = 1, pch = c(18,18,18,18,18), auto.text = F,
         xlab = "", ylab = "", main = "Acumulado Ano HidrolC3gico - Dados Semanais")
dev.off()

#-------------------------------------------------------------------------------------
# Dados horC!rios do sib

inp_sib <- read.table("/data3/TAREFAS/SIB2/PEG/SIB2/data2", header = T, 
                      colClasses = c("character", rep("numeric",14)))
date <- as.POSIXct(inp_sib$time, format = "%y%m%d%H", tz = "GMT")
inp_sib <- data.frame(date, inp_sib[,-1])
Chuva <- merge(selectByDate(inp_sib, year = 2001:2012)[,c(1,7)] , bk_RaiN, all = T)


# Plots de ano acumulado para cada uma das variC!veis, 
yr <- as.numeric(unique(substr(Chuva$date,1,4)))

Hacm <- list()
for(i in 1:length(yr)){
    aux <- selectByDate(Chuva, paste0("1/10/",yr[i]-1), paste0("30/9/",yr[i]))
    aux2 <- data.frame(date = aux$date, 
                       apply(aux[,-1], 2, function(x){
                           x[is.na(x)] <- 0
                           return(x)}))
    Hacm[[i]] <- data.frame(date = aux2$date, 
                            apply(aux2[,-1], 2, cumsum))
}
Hacm <- do.call(rbind,Hacm)

pdf("PEG/OUT/acm_hidrologico_2000-2015.pdf", width =12)
    for(i in seq(1,length(yr),2)){
        timePlot(Hacm, names(Hacm)[-1], group = T, lty = 1.3, pch = rep(".", 4), 
                 auto.text = F, ylim = c(0.5, 2000),
                 xlab = "", ylab = "", main = "Acumulado Ano HidrolC3gico")
        timePlot(selectByDate(Hacm, year = yr[i]:yr[i+2]), names(Hacm)[-1], group = T, lty = 1, 
                 #pch = c(18,18,18,18), 
                 auto.text = F,
                 xlab = "", ylab = "", main = "Acumulado Ano HidrolC3gico")
        
    }
    dev.off()

#
plot(Chuva$Rain ~ Chuva$date, type = "h")
lines(Chuva$Torre ~ Chuva$date, type = "h", col = 2)

par(new = TRUE, las = 1)
plot(rev(Chuva$dif) ~ Chuva$date, type = "h", col = 3, 
     ylim = rev(range(Chuva$dif))) 
axis(side=4, at = pretty(range(rev(Chuva$dif)))
mtext("z", side=4, line=3)


lines(Chuva$Torre ~ Chuva$date, type = "h", col = 2)

timePlot(Chuva, names(Chuva)[c(2,4,6)], group = T, lty = 1)
