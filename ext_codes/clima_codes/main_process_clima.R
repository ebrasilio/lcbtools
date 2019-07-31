#------------------------------------------------------------------------
# EMSB - Extrema
# Função que separa cada varivel em uma tabela distinta
#-----------------------------------------------------------------------


rm(list=ls())
library(openair)

#----------------------------------------------------------------------------------
setwd("/data1/DATA/LCB/EXT/STORAGE/codes/clima_codes")
dir_out <- '/data1/DATA/LCB/EXT/STORAGE/data/C/box/'

# Função para separar cada variavel em um arquivo diferente
separa_vars <- function(avg.time=NULL, out){
  
  # parâmetros iniciais
  if(is.null(avg.time)){
    inp.files = list.files('/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample',
                           pattern='clear_merge.txt', full.names=T)  
  }else if(avg.time=='hour'){
    inp.files = list.files('/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample',
                           pattern='clear_merge_H.txt', full.names=T)  
  }else if(avg.time=='day'){
    inp.files = list.files('/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample',
                           pattern='clear_merge_D.txt', full.names=T) 
  }

  vars = c("Dm.G", "Pa.H", "Rc.mm", "Sm.m.s", "Ta.C", "Ua..")
  varsc = c("WindDir", "Press", "Pcp", "WindSpeed", "Tar", "UR")
  
  for(j in 1:length(vars)){
    for(i in 1:length(inp.files)){
      cat("\n Var: ",vars[j], "\t File: ",substr(inp.files[i],51,53))
      a <- read.csv(inp.files[i])  
      a <- data.frame(date=as.POSIXct(a$X, tz="GMT"), a[,-1])
      if(i == 1){
        dt <- a[, which(c(names(a) %in% c("date", vars[j])))]
        names(dt)[i+1] <- substr(inp.files[i], 51,53)
      }else{
        dt <- merge(dt, a[, which(c(names(a) %in% c("date", vars[j])))],
                    all=T, by='date')
        names(dt)[i+1] <- substr(inp.files[i], 51,53)
      }
      rm(a)  
    }
  
  # acabou de juntar os dados de uma variável, escreve
    if(is.null(avg.time)){
      write.table(dt, paste0(out,'EXT_',varsc[j],'.txt'),   row.names = F, sep=',')
    }else if(avg.time=='hour'){
      write.table(dt, paste0(out,'EXT_',varsc[j],'_H.txt'),   row.names = F, sep=',')
    }else if(avg.time=='day'){
      write.table(dt, paste0(out,'EXT_',varsc[j],'_D.txt'),   row.names = F, sep=',')
    }
    rm(dt)
  }
}


# Função para plotar as variáveis
# var = Tar, UR, Wind_Dir, WindSpeed, Pcp, Press

plot_vars <- function(var="Tar", avg.time=NULL){

  # parâmetros iniciais
  inp.files <- list.files('/data1/DATA/LCB/EXT/STORAGE/data/C/box',
                          pattern=var, full.names=T)
  if(is.null(avg.time)){
    inp.files <- inp.files[
      !(substr(inp.files, nchar(inp.files) - 5, nchar(inp.files)) %in% c('_D.txt', "_H.txt"))]
  }else if(avg.time=='day'){
    inp.files <- inp.files[substr(inp.files, nchar(inp.files) - 5, nchar(inp.files)) == '_D.txt']
  }else if(avg.time=='hour'){
    inp.files <- inp.files[substr(inp.files, nchar(inp.files) - 5, nchar(inp.files)) == '_H.txt']
  }
  
  # ler e plotar 
  a <- read.csv(inp.files)
  a$date <- as.POSIXct(a$date, tz="GMT")
  if(is.null(avg.time)){
    pdf(paste0('out/Ext_', var, '.pdf'), width=12) 
    for(i in 2014:2019){
      timePlot(selectByDate(a, year=i), names(a)[-1], key.columns = 8, 
               y.relation = "free", ylab="", xlab = "", main=i, avg.time='30 min')  
    }
    dev.off()
    
  }else if(avg.time=='day'){
    pdf(paste0('out/Ext_', var, '_D.pdf'), width=12) 
    timePlot(selectByDate(a, year=2016:2019), names(a)[-1], key.columns = 8,
             y.relation = "free", ylab="", xlab = "")
    dev.off()
    
  }else if(avg.time=='hour'){
    pdf(paste0('out/Ext_', var, '_H.pdf'), width=12)  
    timePlot(selectByDate(a, year=2016:2019), names(a)[-1], key.columns = 8,
             y.relation = "free", ylab="", xlab = "")
    dev.off()
  }
}
  

#--------------------------------------------------------------------------------
separa_vars(out = dir_out)
separa_vars(avg.time='hour', out = dir_out)
separa_vars(avg.time='day', out = dir_out)

# plots

plot_vars(var='Tar')
plot_vars(var='UR')
plot_vars(var='WindDir')
plot_vars(var='WindSpeed')
plot_vars(var='Pcp')
plot_vars(var='Press')

