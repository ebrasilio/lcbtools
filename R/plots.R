#----------------------------------------------------------------------------
# Plots do Relatório Anual 2015
#
#----------------------------------------------------------------------------
#rm(list=ls())

library(xlsx)
library(openair)
library(ggplot2)
library(reshape2)
library(grid)

#===========================================================================================
# To make multiplot panels
# vp.setup(3,7)    
# print(DNhist[[1]], vp = vp.layout(1,1))
# Its not mine 
#==========================================================================================

vp.setup <- function(x,y){
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(x,y)))
}

vp.layout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col =y)
}

#---------------------------------------------------------------------------------



#------------------------------------------------------
# A partir de um data frame plotar ciclo anual médio (media mensal) 
# com barras de erros.
#  Input: data-frame
#         vars: vetor das variáveis a processas
#         ylab: label para o eixo y
#   Output: figura 
#
# TODO: acertar para aceitar opção group e fazer os facets
#------------------------------------------------------
# mydata = dt_mes; vars = names(mydata)[2]; ylab=NULL

anual_ciclo_lines <- 
    function(mydata, vars=NULL, ylab=NULL, group=FALSE)
    {
        if(is.null(vars)){
            stop("need vars arguments")
        } else{
            mydt = timeVariation(mydata, vars)$data$month
            mydt$mnth = as.POSIXct(
                paste0("01-", ifelse(nchar(mydt$mnth)==1,"0",""),mydt$mnth,"-2000"),
                                   format = "%d-%m-%Y", tz = "GMT")
            mydt$mn = substr(strftime(mydt$mnth, format="%b"),1,1)
        }
        
        if(is.null(ylab)){
            ylab = ""
        } else{
            ylab = ylab
        }
        
        pd <- position_dodge(0.1)
        plot = 
            ggplot(mydt, aes(x=mnth, y = Mean, col=variable)) + 
            geom_errorbar(aes(ymin=Lower,ymax=Upper), 
                          color="black", position=pd) +
            geom_line(size=1.3, position = pd) + 
            geom_point(size=2.5,position=pd) +
            ylab(ylab) + 
            #scale_x_continuous(name = "Month", breaks=seq(0,12,1)) +
            scale_x_datetime(date_labels = mydt$mn, date_breaks="months") +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  legend.text=element_text(size = 15),
                  strip.text.x = element_text(size=15, face="bold"),
                  axis.text.x=element_text(size=20),
                  axis.text.y=element_text(size=20),
                  axis.title.x=element_blank(),
                  axis.title.y=element_text(size=20))
        return(plot)
    }

anual_ciclo_bars <- 
    function(mydata, vars = NULL, group = FALSE, ylab=NULL)
    {
        
        if(is.null(vars)){
            stop("need vars arguments")
        } else{
            mydt = timeVariation(mydata, vars)$data$month
        }
        
        if(is.null(ylab)){
            ylab = ""
        } else{
            ylab = ylab
        }
        plot = 
            ggplot(mydt, aes(x=mnth, y = Mean, fill=variable)) + 
            geom_bar(stat="identity", position=position_dodge()) + 
            geom_errorbar(aes(ymin=Lower,ymax=Upper), 
                          width=0.2, color="black", position = position_dodge(0.9)) +
            ylab(ylab) + 
            scale_x_continuous(name = "Month", breaks=seq(0,12,1)) +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  legend.text=element_text(size = 15),
                  strip.text.x = element_text(size=15, face="bold"),
                  axis.text.x=element_text(size=20),
                  axis.text.y=element_text(size=20),
                  axis.title.x=element_text(size=20),
                  axis.title.y=element_text(size=20))
        return(plot)
    }


# only bar plot without errorbars
# TOFIX
bar_plot <-     
    function(mydata, avg.time = NULL,vars = NULL, group = FALSE, ylab=NULL)
    {
        
        if(is.null(vars)){
            stop("need vars arguments")
        } else{
            mydt = melt(mydata[,c("date",vars)], id.vars="date")
        }
        if(is.null(ylab)){
            ylab = ""
        } else{
            ylab = ylab
        }
        #        if(!is.null(ylab)){
        #     plot = 
        #         ggplot(mydt, aes(x=date, y=value, fill=variable)) + 
        #         geom_bar(stat="identity", position="dodge") +
        #         #scale_x_datetime(date_labels = "%b-%y", date_breaks="6 months") + 
        #         ylab(ylab) +
        #         theme(legend.position="bottom",
        #               legend.title=element_blank(),
        #               legend.text=element_text(size = 15),
        #               strip.text.x = element_text(size=15, face="bold"),
        #               axis.text.x=element_text(size=20, angle=0),
        #               axis.text.y=element_text(size=20),
        #               axis.title.x=element_text(size=20),
        #               axis.title.y=element_text(size=20))
        # } else{
        plot = 
            ggplot(mydt, aes(x=date, y=value, fill=variable)) + 
            geom_bar(stat="identity", position="dodge") +
            scale_x_datetime(date_labels = "%b-%y", date_breaks="6 months") + 
            scale_fill_manual(values=c("salmon","orange","black")) + 
            ylab(ylab) +
            theme(legend.position="bottom",
                  legend.title=element_blank(),
                  legend.text=element_text(size = 15),
                  strip.text.x = element_text(size=15, face="bold"),
                  axis.text.x=element_text(size=20, angle=0),
                  axis.text.y=element_text(size=20),
                  axis.title.x=element_text(size=20),
                  axis.title.y=element_text(size=20))
        #}
        
        
        return(plot)           
    }   

##year
# mydt = year
# mydt$value = round(mydt$value,0)
# ppy =
#     ggplot(mydt, aes(x=date, y=value, fill=variable)) +
#     geom_bar(stat="identity", position="dodge") +
#     geom_text(aes(label = value),position=position_dodge(width=1),
#               vjust=0, size=8) +
#     scale_fill_manual(values=c("salmon","orange","black")) +
#     ggtitle("")+
#     ylim(c(0,2100))+
#     ylab("mm/year") +
#     theme(legend.position="bottom",
#           legend.title=element_blank(),
#           legend.text=element_text(size = 15),
#           strip.text.x = element_text(size=15, face="bold"),
#           axis.text.x=element_text(size=20, angle=0),
#           axis.text.y=element_text(size=20),
#           axis.title.x=element_blank(),
#           axis.title.y=element_text(size=20))

eq = TRUE
#=====================================================
# função plot básica
plot_mnt <- function(aux, titulo, laby, ylim) {
  f4 <- 
    ggplot(aux, aes(x=date, y=value, group=variable,color = variable)) + 
    geom_line(size=1.5, aes(linetype=variable)) +
    ylim(ylim) + 
    ylab(laby) +
    geom_point (size=4,aes(shape=variable)) + 
    xlab("Mês") +
    scale_shape_manual(values=c(19,20,6,6,6,6)) +
    scale_linetype_manual(values=c(1,1,1,1,1,1)) + 
    scale_x_date(date_labels = "%b", date_breaks="month") +
    ggtitle(titulo) +
    theme(#legend.justification=c(0,1),
      # #legend.position=c(0,1),
      plot.title=element_text(size=20),
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text=element_text(size = 15),
      #plot.title=element_text(size=30,vjust=1),
      axis.text.x=element_text(size=20),
      axis.text.y=element_text(size=20),
      axis.title.x=element_text(size=20),
      axis.title.y=element_text(size=20))
  return(f4)
}


plot_yr <- function(aux, titulo, laby, ylim, lm=TRUE) {
  if(lm){
    modelo = lm(aux$Year ~ aux$Time)
    coeficientes <- modelo$coefficients 
    texto <- sprintf('y = %.2f + %.2fx, r² = %.2f', 
                     coeficientes[1], coeficientes[2], summary(modelo)$r.squared)  
    p <- 
      ggplot(aux, aes(x=Time, y=Year)) +
      geom_smooth(method = "lm", se=FALSE, color="red",size=2, formula = y ~ x) +
      geom_line(size = 1.8) + geom_point(size=3) +
      geom_text(aes(x=min(aux$Time), y=ylim[2],#max(aux$Year),
                    label=texto), 
                size=7,hjust=0, vjust=1) +
      ggtitle(titulo) + ylab(laby) + 
      xlab("Ano") + ylim(ylim) +
      scale_x_continuous(breaks=seq(1930,2015, by=10)) +
      theme(plot.title=element_text(size=20),
            legend.position="bottom",
            legend.title=element_blank(),
            legend.text=element_text(size = 15),
            #plot.title=element_text(size=30,vjust=1),
            axis.text.x=element_text(size=20),
            axis.text.y=element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20))
  } else {
    p <- 
      ggplot(aux, aes(x=Time, y=Year)) +
      geom_line(size = 1.8) + geom_point(size=3) +
      #geom_smooth(method=lm, se=FALSE) +
      ggtitle(titulo) + ylab(laby) + 
      xlab("Ano") + ylim(ylim) +
      scale_x_continuous(breaks=seq(1930,2015, by=10)) +
      theme(plot.title=element_text(size=20),
            legend.position="bottom",
            legend.title=element_blank(),
            legend.text=element_text(size = 15),
            #plot.title=element_text(size=30,vjust=1),
            axis.text.x=element_text(size=20),
            axis.text.y=element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20))
  }

  return(p)
}



#=====================================================
# Figuras do relatorio climatologico anual da estação 
# meteorológica da Água Funda - IAG
#=====================================================

# # figura 4 (pg 21) normais e media longo prazo - temperatura 
# #-----------------------------------------------------------------------------
# {
#   fig4 = read.xlsx("../dados/TemperaturaGraficosBarras.xls",sheetIndex = 1,
#                    startRow = 86, endRow = 88, header=F)
#   # names(fig4)[1:13] = c("Time","JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET",
#   #                        "OUT","NOV","DEZ")
#   names(fig4)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   pos = which(!is.na(fig4$Time))
#   fig4 = data.frame(t(fig4[pos,2:13]))
#   fig4 = data.frame(date=as.Date(row.names(fig4),format="%d/%m/%Y"),fig4)
#   
#   # fig4 = data.frame(mnt=as.POSIXct(row.names(fig4), format="%d/%m/%Y", tz="GMT"), 
#   #                   fig4)
#   
#   names(fig4)[-1] = c("Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015","Média_Climatológica_1933-2015")
#   aux = melt(fig4, id.vars = "date")
#   
#   p4 = plot_mnt(aux, titulo ="Temperatura Média Mensal do Ar - EM-IAG-USP", 
#                 laby = "Temperatura °C",ylim=c(10,25))
# }
# 
# #-----------------------------------------------------------------------
# #figura 6 (pg 26) normais e media longo prazo  - minima
# {
#   fig6 = read.xlsx("../dados/TemperaturaGraficosBarras.xls",sheetIndex= 5,
#                    startRow= 86, endRow=88, header=F)
#   names(fig6)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   namesCol = as.character(fig6$Time[!is.na(fig6$Time)])
#   pos = which(!is.na(fig6$Time))
#   fig6 = data.frame(t(fig6[pos,2:13]))
#   fig6 = data.frame(date=as.Date(row.names(fig6),format="%d/%m/%Y"), fig6)
#   names(fig6)[-1] = c("Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015")
#   aux = melt(fig6, id.vars = "date")
#   p6 <- plot_mnt(aux, 
#                  titulo ="Temperatura Média Mínima Mensal do Ar e Mínima Absoluta - EM-IAG-USP",
#                  laby="Temperatura °C", ylim=c(5,20))
#   
# }
# 
# #-----------------------------------------------------------------------
# #figura 7 (pg 29) media mensal
# {
#   fig4 = read.xlsx("../dados/TemperaturaGraficosBarras.xls",sheetIndex= 1,
#                    startRow= 2, endRow=84, header=F)
#   names(fig4)[c(1,14)] = c("Time","Year")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   p7 = plot_yr(fig4, titulo="Temperatura Média - 1933-2015 - EM-IAG-USP",
#               ylim=c(15,22),  laby="Temperatura ºC", lm=eq)
# }
# 
# #-----------------------------------------------------------------------
# #figura 8 (pg 29) media maxima e media minima
# {
#   fig4 = read.xlsx("../dados/TemperaturaGraficosBarras.xls",sheetIndex= 3,
#                    startRow= 2, endRow=84, header=F)
#   names(fig4)[c(1,14)] = c("Time","Year")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   p8a = plot_yr(fig4, titulo="Temperatura Média das Máximas - Anual - 1933-2015 - EM-IAG-USP",
#                ylim=c(22,28),  laby="Temperatura ºC",lm=eq)
#   
#   
#   fig4 = read.xlsx("../dados/TemperaturaGraficosBarras.xls",sheetIndex= 5,
#                    startRow= 2, endRow=84, header=F)
#   names(fig4)[c(1,14)] = c("Time","Year")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   p8b = plot_yr(fig4, titulo="Temperatura Média das Mínimas - Anual - 1933-2015 - EM-IAG-USP",
#                 ylim=c(11,18),  laby="Temperatura ºC",lm=eq)
# }
# 
# #-----------------------------------------------------------------------
# #figura 9 (pg 31) normais e media longo prazo 
# {
#   fig4 = read.xlsx("../dados/Precipit.xls",sheetIndex= 1,
#                    startRow=89, endRow=91, header=F)
#   names(fig4)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   pos = which(!is.na(fig4$Time))
#   fig4 = data.frame(t(fig4[pos,2:13]))
#   fig4 = data.frame(date=as.Date(row.names(fig4),format="%d/%m/%Y"),fig4)
#   names(fig4)[-1] = c("Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015")
#   aux = melt(fig4, id.vars = "date")
#   p9 = plot_mnt(aux, titulo ="Precipitação Mensal - EM-IAG-USP", 
#                 laby = "Precipitação Mensal mm",ylim=c(10,300))
#   
#   
# }
# 
# #-----------------------------------------------------------------------
# #figura 13 (pg 35)  normais e media longo prazo
# {
#   fig4 = read.xlsx("../dados/Precipit.xls",sheetIndex= 2,
#                    startRow=88, endRow=91, header=F)
#   names(fig4)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   namesCol = as.character(fig4$Time[!is.na(fig4$Time)])
#   pos = which(!is.na(fig4$Time))
#   fig4 = data.frame(t(fig4[pos,2:13]))
#   fig4 = data.frame(date=as.Date(row.names(fig4),format="%d/%m/%Y"),fig4)
#   names(fig4)[-1] = c("Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015")
#   aux = melt(fig4, id.vars = "date")
#   p13 = plot_mnt(aux, titulo ="Número de dias com Precipitação - EM-IAG-USP", 
#                 laby = "Número de dias",ylim=c(5,25))
# }
# 
# #-----------------------------------------------------------------------
# #figura 14 (pg 36)  normais e media longo prazo
# {
#   tab1 = read.xlsx("../dados/Precipit.xls",sheetIndex= 1,
#                    startRow=87, endRow=91, header=F)
#   names(tab1)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab1$Time))
#   tab1 = data.frame(t(tab1[pos,2:13]))
#   tab1 = data.frame(date=as.Date(row.names(tab1),format="%d/%m/%Y"),tab1)
#   names(tab1)[-1] = c("Média_Climatológica_1933-2015","Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015")
#   
#   tab2 = read.xlsx("../dados/Precipit.xls",sheetIndex= 2,
#                    startRow=87, endRow=91, header=F)
#   names(tab2)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab2$Time))
#   tab2 = data.frame(t(tab2[pos,2:13]))
#   tab2 = data.frame(date=as.Date(row.names(tab2),format="%d/%m/%Y"),tab2)
#   names(tab2)[-1] = c("Média_Climatológica_1933-2015","Normal_1933-1960","Normal_1961-1990",
#                       "Média_1991-2015")
#   med = data.frame(Time=tab1$date, tab1[,-1] / tab2[,-1])
#   names(med) = names(tab2)
#   
#   aux = melt(med, id.vars = "date")
#   p14 = plot_mnt(aux, titulo ="Precipitação Média Diária - EM-IAG-USP", 
#                  laby = "Precipitação Média mm/dia",ylim=c(0,20))
# }
# 
# #-----------------------------------------------------------------------
# #figura 15 e 16 (pg 35)  valor anual
# # precipitação
# {
#   tab1 = read.xlsx("../dados/Precipit.xls",sheetIndex= 1,
#                    startRow=2, endRow=84, header=F)
#   names(tab1)[c(1,15)] = c("Time","Year")
#   p15 = plot_yr(tab1, titulo ="Precipitação Total Anual 1933-2015 - EM-IAG-USP", 
#                  laby = "Precipitação mm/ano",ylim=c(600,2300),lm=eq)
#   
#   
#   tab2 = read.xlsx("../dados/Precipit.xls",sheetIndex= 2,
#                    startRow=2, endRow=84, header=F)
#   tab2 = data.frame(Time=tab2$X1, Year=apply(tab2[,-1],1,sum))
#   #names(tab1)[c(1,15)] = c("Time","Year")
#   p16 = plot_yr(tab2, titulo ="Número Total de dias com Precipitação 1933-2015 - EM-IAG-USP", 
#                 laby = "Número de dias",ylim=c(100,300),lm=eq)
# }
# 
# #-----------------------------------------------------------------------
# #figura 17 (pg 40)  normais e media longo prazo
# # ur
# {
#   tab2 = read.xlsx("../dados/Umidade.xls",sheetIndex= 5,
#                    startRow=86, endRow=87, header=F)
#   names(tab2)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab2$Time))
#   tab2 = data.frame(t(tab2[pos,2:13]))
#   tab2 = data.frame(date=as.Date(row.names(tab2),format="%d/%m/%Y"),tab2)
#   names(tab2)[-1] = c("Normal_1961-1990","Média_1991-2015")
#   aux = melt(tab2, id.vars = "date")
#   p17 = plot_mnt(aux, titulo ="Umidade Relativa Média Mensal - EM-IAG-USP", 
#                  laby = "Umidade Relativa %",ylim=c(72,88))
# }
# 
# #-----------------------------------------------------------------------
# # figura 25 (pg 51)  normais e media longo prazo
# # figura 26 (pg 52)  valor anual  
# # garoa
# {
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 1,
#                    startRow=86, endRow=88, header=F)
#   names(tab2)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab2$Time))
#   tab2 = data.frame(t(tab2[pos,2:13]))
#   tab2 = data.frame(date=as.Date(row.names(tab2),format="%d/%m/%Y"),tab2)
#   names(tab2)[-1] = c("Normal_1933-1960","Normal_1961-1990","Média_1991-2015")
#                       
#   aux = melt(tab2, id.vars = "date")
#   p25 = plot_mnt(aux, titulo ="Ocorrência de Garoa - EM-IAG-USP", 
#                  laby = "Número de Dias",ylim=c(2,12))
#   
#   # figura 26 (pg 52)  valor anual  
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 1,
#                    startRow=2, endRow=84, header=F)
#   names(tab2)[c(1,15)] = c("Time","Year")
#   p26 = plot_yr(tab2, titulo ="Total Anual de dias com garoa 1933-2015 - EM-IAG-USP", 
#                 laby = "Número de Dias",ylim=c(0,200),lm=eq)
# }
# 
# #-----------------------------------------------------------------------
# # figura 27 (pg 53)  normais e media longo prazo
# # figura 28 (pg 53)  valor anual
# # orvalho
# {
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 4,
#                    startRow=61, endRow=63, header=F)
#   names(tab2)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab2$Time))
#   tab2 = data.frame(t(tab2[pos,2:13]))
#   tab2 = data.frame(date=as.Date(row.names(tab2),format="%d/%m/%Y"),tab2)
#   names(tab2)[-1] = c("Normal_1961-1990","Média_1991-2015",
#                       "Média_Climatológica_1957-2015")
#   aux = melt(tab2, id.vars = "date")
#   p27 = plot_mnt(aux, titulo ="Ocorrência de Orvalho - EM-IAG-USP", 
#                  laby = "Número de Dias",ylim=c(5,30))
#   
#   # figura 28 (pg 53)  valor anual  
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 4,
#                    startRow=2, endRow=59, header=F)
#   names(tab2)[c(1,14)] = c("Time","Year")
#   p28 = plot_yr(tab2, titulo ="Total Anual de dias com garoa 1933-2015 - EM-IAG-USP", 
#                 laby = "Número de Dias",ylim=c(0,250),lm=eq)
# }
# 
# 
# #-----------------------------------------------------------------------
# # figura 29 (pg 54)  normais e media longo prazo
# # figura 30 (pg 55)  valor anual
# # nevoeiro
# {
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 7,
#                    startRow=86, endRow=89, header=F)
#   names(tab2)[1:13] = c("Time","01/01/2000","01/02/2000","01/03/2000","01/04/2000",
#                         "01/05/2000","01/06/2000","01/07/2000","01/08/2000",
#                         "01/09/2000","01/10/2000","01/11/2000","01/12/2000")
#   pos = which(!is.na(tab2$Time))
#   tab2 = data.frame(t(tab2[pos,2:13]))
#   tab2 = data.frame(date=as.Date(row.names(tab2),format="%d/%m/%Y"),tab2)
#   names(tab2)[-1] = c("Normal_1933-1960","Normal_1961-1990","Média_1991-2015",
#                       "Média_Climatológica_1933-2015")
#   aux = melt(tab2, id.vars = "date")
#   p29 = plot_mnt(aux, titulo ="Ocorrência de Nevoeiro - EM-IAG-USP", 
#                  laby = "Número de Dias",ylim=c(0,20))
#   
#   # figura 30 (pg 55)  valor anual  
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 7,
#                    startRow=2, endRow=84, header=F)
#   names(tab2)[c(1,14)] = c("Time","Year")
#   p30 = plot_yr(tab2, titulo ="Total Anual de dias com Nevoeiro 1933-2015 - EM-IAG-USP", 
#                 laby = "Número de Dias",ylim=c(0,250),lm=eq)
# }
# 
# #-----------------------------------------------------------------------
# # figura 32 (pg 56)  valor anual
# # trovoada
# {
#   # figura 32 (pg 56)  valor anual  
#   tab2 = read.xlsx("../dados/Fenomenos.xls",sheetIndex= 10,
#                    startRow=2, endRow=59, header=F)
#   names(tab2)[c(1,14)] = c("Time","Year")
#   p32 = plot_yr(tab2, titulo ="Total Anual de dias com Trovoadas 1958-2015 - EM-IAG-USP", 
#                 laby = "Número de Dias",ylim=c(20,130),lm=eq)
# }
# 
# # pdf("../out/plotsV2.pdf", width=12)
# #   print(p4)
# #   print(p6)
# #   print(p7)
# #   print(p8a)
# #   print(p8b)
# #   print(p9)
# #   print(p13)
# #   print(p14)
# #   print(p15)
# #   print(p16)
# #   print(p17)
# #   print(p25)
# #   print(p26)
# #   print(p27)
# #   print(p28)
# #   print(p29)
# #   print(p30)
# #   print(p32)
# # 
# # dev.off()


#----------------------------------------------------------------------
plot_wind_vector <- function(mydata, day_ini=NULL, day_end=NULL, format = "uv",
                             title = NULL){
    # format uv = na tabela temos u e v já calculados
    #        wswd = na tabela temos Vh e direção
    # TODO incluir opção para fazer as medias por ciclos chamando a função Make....
    
    if(!is.null(day_ini)){
        auxWd = selectByDate(mydata, day_ini, day_end)
    } else {
        auxWd = mydata
    }
    
    if(format != "uv"){
        auxWd$u <- (-1 * auxWd$ws) * sin((auxWd$wd) * pi / 180.0)
        auxWd$v <- (-1 * auxWd$ws) * cos((auxWd$wd) * pi / 180.0)
    }
    dw = auxWd[,names(auxWd) %in% c("date","u", "v")]
    dw$date = as.POSIXct(paste0("2000-01-01 ",dw$date), format="%Y-%m-%d %H", tz="GMT")
    
    dwd_p <-
        ggplot(data=dw, aes(x=date, y=0)) +
        theme(plot.margin=unit(c(0,0.8,0.5,0.9),'lines')) +
        geom_segment(aes(xend=date+u*3600, yend=v),
                     arrow=arrow(length=unit(0.25,'cm')), size = 1)+ ylim(c(-5,5)) +
        scale_x_datetime(date_breaks = "2 hour", date_labels = "%H") +
        ylab("Vento m/s")+
        theme(axis.title.x = element_text(angle=0, vjust=0, size=14, colour= "black"),
              axis.title.y = element_blank(),#element_text(size=14, colour= "black"),
              axis.text.x  = element_text(angle=0, vjust=0, size=14, colour= "black"),
              axis.text.y  = element_text(angle=0, vjust=0, size=14, colour= "black")) +
        ggtitle(title)
    
    return(dwd_p)
    
}
