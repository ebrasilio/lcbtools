# - arrunmando figuras dos sitios
#

library(openair)
library(reshape)
library(ggplot2)
library(scales)


sitio <- "SVG/"
path <- "/data1/BANCO/SITES/"


# médias diárias 
inp <- c("svg_020_T_AR_HMP.1d", "svg_021_UR_HMP.1d", "svg_033_um_esp.1d","svg_032_press_vapor.1d",
       "svg_002_RG_IN.1d", "svg_003_RG_OUT.1d", "svg_007_ATMOSPHERIC_PRESS.1d",
       "svg_007_PRESS_AWS.1d", "svg_006_RAIN_AWS.1d","svg_052_VAZAO_SOLINST.1d")

test.data  <- lapply(inp, 
                     function(.file) 
                         read.table(paste0(path, sitio, .file, sep = ""),
                                    stringsAsFactors = F,
                                    na.strings = c("-7777.00000000","-9999.00000000"),
                                    header = FALSE, colClasses = c(rep("character",4),"numeric")
                         )
)

test.data <- do.call(cbind, test.data)
mdata <- data.frame(test.data[,c(1:5,seq(10,ncol(test.data), 5))])
date  <- as.POSIXct(paste(mdata$V1, mdata$V2, mdata$V3, mdata$V4), 
                    format = "%Y %j %H %M", tz  = "GMT")
mdata <- data.frame(date = date, mdata[, -c(1:4)])
names(mdata) <- c("date", "Temp_Ar(°C)", "UR(%)", "Q(g/Kg)", "e(kPa)", "Ki(W/m2)",
                  "Kr(W/m2)", "Press(kPa)", "Pressure(kPa)","Chuva(mm)", "Vazão(m3/s)")

timePlot(selectByDate(mdata, year = 2008:2015), names(mdata)[-1])

mdata$`Press(kPa)`[ which(mdata$`Press(kPa)` < 88.5) ] <- NA
mdata$`Press(kPa)`[ which(mdata$`Press(kPa)` > 92) ] <- NA

mdata$`Pressure(kPa)`[ which(mdata$`Pressure(kPa)` < 88.4) ] <- NA
mdata$`Pressure(kPa)`[ which(mdata$`Pressure(kPa)` > 92) ] <- NA

## 2010:2013

pdf("/data3/figuras_aula_instrumentos/2015/figurasdiarias_svg.pdf", width = 20)
tt <- mdata$date[which(substr(mdata$date,9,10) == "01")][c(T,F,F,F,F,F)]
for(i in 2:8){
    aux <- mdata[,c(1,i)]
    aux <- selectByDate(aux, year = 2010:2013)
    aux <- melt(aux, id.vars = "date")
    
    p <- ggplot(aux, aes(x = date, y = value)) +
         geom_point(shape = 16, size = 4) + 
         geom_line(size = 1.5 ) +
         ylab ("") + xlab("") +
         ggtitle(paste0("Média Diária - ", names(mdata)[i], " - São Luiz do Paraitinga-SP"))+
              theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(size=20),
              axis.text.y  = element_text(size=20),
              plot.title = element_text(size=20)) + 
         scale_x_datetime(breaks = tt, labels = date_format("%b/%Y"))
    print(p)
}
## chuva e vazão
i <- 10
    aux <- mdata[, c(1,10)]
    aux <- selectByDate(aux, year = 2010:2013)
    aux <- melt(aux, id.vars = "date")
    p <- ggplot(aux, aes(x = date, y = value, colour = variable)) +
         geom_bar(stat = "identity") + 
         ylab ("") + xlab("") +
         ggtitle(paste0("Acumulado Diário - ", names(mdata)[i], " - São Luiz do Paraitinga-SP"))+
         theme(axis.title.x = element_text(face="bold", size=20),
              axis.text.x  = element_text(size=20),
              axis.text.y  = element_text(size=20),
              plot.title = element_text(size=20)) + 
              theme(legend.position="none")+ 
         scale_x_datetime(breaks = tt, labels = date_format("%b/%Y")) 
         
    print(p)
         
dev.off()


## Selecionar 






# ajuste para legenda manual
tt <- dt1[ dt1$date == "2000-12-01",]

 +
    geom_text(data = tt, aes(label = variable), hjust = -0.25)
