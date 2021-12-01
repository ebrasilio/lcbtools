# ler dados de perfil, interpolar para fazer  figura típica, plotar 
# ou não outra 
# variável como chuva ou temperatura

source("/home/emilia/git/lcbtools/R/readLcbData.R")
inp <- list.files("/data1/BANCO/SITES/PDG1/",pattern = "30m")[c(13,17,15,19,21,23,25,27,29,31,48,49)]
a <- readLcbData(inp, "pdg")


mydata <- a
colunas_perfil <- c(11,13,15:20)
coluna_var <- 9

perfil <- function(mydata, colunas_perfil = c(2:11), 
                   z_perfil = c(0.1, 0.2, 0.5, 0.8, 1.0, 1.5, 2.0, 2.5),
                   coluna_var = NULL){
  
  library (fields)
  library(RColorBrewer)
  library(openair)
  
  pdf("teste.pdf", width = 15)
  for(i in 2000:2019){
    my_dt <- selectByDate(mydata[,c(1, colunas_perfil)], year=i)
    if (sum(!is.na(my_dt[,2])) > 0 ){
      #if(!is.null(coluna_var))var <- mydata[,c(1,coluna_var)]
      
      names(my_dt)[-1] <- as.character(z_perfil)
      #x <- 1:nrow(my_dt)
      x <- my_dt$date
      y <- z_perfil
      z <- as.matrix(my_dt[,2:ncol(my_dt)])
      pall <- tim.colors(16)
      
      timePlot(my_dt, names(my_dt)[-1], group=T, main=i, ylab = "", key.columns = 4)
      filled.contour(x, y, z, ylim = c(max(y, na.rm=T)+0.1, 0), 
                     col = rev(tim.colors(12)), nlevels = 9, 
                     font.main=1, cex.main=1.5,
                     ylab = "Profundidade (m)",
                     key.axes = axis(4, seq(0, round(max(z, na.rm=T),2), by = 0.01)),
                     main = i)
      mtext(text = "VWC (%)", side = 3, line = 0, adj= 1, cex = 1.3)
      abline(h= y, col = "black", lty =3)
    }
  } # for
  dev.off()
  
}
# exp <- expression(m^3/m^3)
# filled.contour(x, y, z, ylim = rev(range(y)), #ylim = c(150,0), 
#                zlim = c(0, 0.8), nlevels = 15,
#                col = rev(tim.colors(16)), font.main=1, cex.main=1.5,
#                main = "Umidade do Solo - Bananal (30 min )\n",
#                ylab = "Profundidade (cm)",
#                key.axes = axis(4, seq(0, 0.8, by = 0.1)))
# mtext(text = exp, side = 3, line = 0, adj= 1, cex = 1.3)
# abline(h= y, col = "grey", lty =3)
# tp <- timePlot(bandta, "RAIN", plot.type="h", lwd=3, col=4,
#                date.breaks = 12, ylab = "")$plot
# tp <- update(tp, scales = list(y=list(relations="free",cex=2,tick.number=10),          # ignora relations="free" e prevalece "ylim"
#                                x=list(format="%b", cex=1.5, rot = 0)),#)#,
#              main = paste("Precipitação Acumulada - dia\n",i))
