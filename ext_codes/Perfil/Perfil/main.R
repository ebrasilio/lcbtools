#=================================================================================#
# Ler dados do Perfil, ordená-los e gerar figuras diagnosticas                    #
#                                                                                 #
#=================================================================================#

rm(list=ls())

library(stringr)
library(openair)
library(reshape2)
library(ggplot2)
source('functions.R')                                                    # read_wxt


# inicio dados medidos a cada 5 min - 1ª coleta
wxt1 <- read_wxt(sensor = 1,
          inp = '/data1/DATA/LCB/EXT/origin/2017/26017/EP/TOA5_50318.WXT_1_m5.dat')
wxt2 <- read_wxt(sensor = 2,
          inp = '/data1/DATA/LCB/EXT/origin/2017/26017/EP/TOA5_50318.WXT_2_m5.dat')
wxt3 <- read_wxt(sensor = 3,
          inp = '/data1/DATA/LCB/EXT/origin/2017/26017/EP/TOA5_50318.WXT_3_m5.dat')
wxt4 <- read_wxt(sensor = 4,
          inp = '/data1/DATA/LCB/EXT/origin/2017/26017/EP/TOA5_50318.WXT_4_m5.dat')


# outras coletas 2 min - remover dados da primeira coleta.
# wxt1
inp <- list.files('/data1/DATA/LCB/EXT/origin', recursive=T,
                  pattern="WXT520_1", full.names = T)
inp <- inp[ str_detect(inp, "TOA5")] [-1]
wxt1_a <- read_wxt(inp, 1)

# wxt_2
inp <- list.files('/data1/DATA/LCB/EXT/origin', recursive=T,
                  pattern="WXT520_2", full.names = T)
inp <- inp[ str_detect(inp, "TOA5")] [-1]
wxt2_a <- read_wxt(inp,2)

# wxt_3
inp <- list.files('/data1/DATA/LCB/EXT/origin', recursive=T,
                  pattern="WXT520_3", full.names = T)
inp <- inp[ str_detect(inp, "TOA5")] [-1]
wxt3_a <- read_wxt(inp,3)

# wxt_4
inp <- list.files('/data1/DATA/LCB/EXT/origin', recursive=T,
                  pattern="WXT520_4", full.names = T)
inp <- inp[ str_detect(inp, "TOA5")] [-1]
wxt4_a <- read_wxt(inp,4)

#=======================================
# juntar todos os dados e fazer média de 10 minutos
#=======================================
wxt1 <- my_merge(mydt1 = wxt1, mydt2 = wxt1_a)
wxt2 <- my_merge(mydt1 = wxt2, mydt2 = wxt2_a)
wxt3 <- my_merge(mydt1 = wxt3, mydt2 = wxt3_a)
wxt4 <- my_merge(mydt1 = wxt4, mydt2 = wxt4_a)

#=======================================
# saida
#=======================================
out <- '/data1/DATA/LCB/EXT/STORAGE/data/P/'

var <- c("Ta","Ua","Pa")
for(i in 1:length(var)){
  vars1 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars2 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars3 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  vars4 <- c(1,which(!is.na(str_match(names(wxt1), var[i]))))
  
  mydt <- merge(wxt1[,vars1], wxt2[,vars2])
  mydt <- merge(mydt, wxt3[,vars3])
  mydt <- merge(mydt, wxt4[,vars4])
  
  write.csv(mydt, paste0(out,var[i],'_Extrema_Perfil.csv'),
            row.names = F, quote = T)
}

#========================================
# Fazer figuras por variável
# "date"    "Dn_0.5m" "Dm_0.5m" "Dx_0.5m" "Sn_0.5m" "Sm_0.5m" "Sx_0.5m" "Ta_0.5m" "Tp_0.5m"
# "Ua_0.5m" "Pa_0.5m" "Rc_0.5m" "Vs_0.5m" "Vh"
#========================================
pdf('/data3/TAREFAS/emilia/extrema_2019/perfil.pdf', width=12)
p1 <- my_plot("Ta")
print(p1)
p1 <- my_plot("Ta", avg='1 day')
print(p1)
p1 <- my_plot("Ua")
print(p1)
p1 <- my_plot("Ua", avg='1 day')
print(p1)
p1 <- my_plot("Pa",lim=c(850,950))
print(p1)
p1 <- my_plot("Pa",avg='1 day',lim=c(850,950))
print(p1)
 dev.off()
