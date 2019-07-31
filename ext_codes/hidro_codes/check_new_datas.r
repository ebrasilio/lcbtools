#---------------------------------------------------------------------
# Verificar novos dados - tem coleta duplicada !!!!!
#---------------------------------------------------------------------

rm(list=ls())

#-----------------------------
# checar dados 2014 "on root"
#-----------------------------
ll <- list.dirs(path = '/data3/TRANSPORTE/EXT/2014-2016', recursive = F)
coletas <- ll[substr(ll, 40,41)== '14']

#----------------------------------------------------------------------
# ler cabeçalho e checar se corresponde ao nome, ver 1} e último dado
#----------------------------------------------------------------------

for (i in 1:length(coletas)){
  sta <- c('H04', 'H05', 'H06', 'H07', 'H08', 'H09')
  for(s in 1:length(sta)){
    ll1 <- list.files(coletas[i], sta[s], recursive = T, full.names = T)
    write.table(file="chech_files_2014.txt", x = paste('coleta: ', coletas[i], 'estação: ', sta[s]),
                append = T, col.names = F, row.names = F, quote = F)
    cat(paste('coleta: ', coletas[i], 'estação: ', sta[s], '\n'))
    if(length(ll1) > 0){
      for(j in 1:length(ll1)){
        aux <- readLines(ll1[j]) 
        label <- unique(aux[stringr::str_detect(aux,'Modulo')])
        ini <- substr(aux[stringr::str_detect(aux,"201")],1,16)[1]
        end <- substr(aux[stringr::str_detect(aux,"201")],1,16) [length(aux[stringr::str_detect(aux,"201")])]
        write.table(file='chech_files_2014.txt',paste('arquivo: ', label, 'periodo: ', ini, " - ", end),
                    append = T, col.names = F, row.names = F, quote = F)
        cat('arquivo: ', label, 'periodo: ', ini, " - ", end, '\n')
      }
      write.table(file='chech_files_2014.txt','coleta: ', x=paste0('\n'),
                  append = T, col.names = F, row.names = F, quote = F)
      cat('\n\n')
    }else{
      write.table(file='chech_files_2014.txt',x=paste('Não houve coleta da estação ',sta[s],'nesta data \n'),
                  append = T, col.names = F, row.names = F, quote = F)
      cat('Não houve coleta da estação ',sta[s],'nesta data \n\n')
    }
  }
}


#--------------------------------------------

id = "H04"
yr = '14'
sta <- list.files('/data3/TRANSPORTE/EXT/2014-2016', paste0(id,yr), full.names = T, recursive = T) # 91
fil <- sta[substr(sta,33,41) == 'Extrema14']

for(i in 1:length(fil)){
  system(paste0("cp ", fil[i], ' /data1/DATA/LCB/EXT/20',yr,'/',id,'/'))
}


# for(i in 1:length(fil)){
#   aux <- sta[substr(sta,nchar(sta)- 11, nchar(sta)) == fil[i]]
#   if(length(aux) > 1){
#     cat('arquivo duplicado: ', fil[i], '\n')
#     for(j in 1:length(aux)){
#       system(paste('md5sum', aux[j]))
#     }
#     cat("\n")
#   }else{
#     cat('arquivo único: ', fil[i], '\n')
#   }
# }







# 
# ll <- list.files('../../data/H/merge','merge.txt',full.names = T)
# 
# for(i in 1:length(ll)){
#   aux <- read.csv(ll[i], header = T)
#   aux$date <- as.POSIXct(aux$date, tz='GMT')
#   
#   #------------------------------------------
#   # checar a sequencia de datas
#   #------------------------------------------
#   
#   dt <- unique(aux$date)
#   a <- table(aux$date)
#   b <- a[ a != 1] 
#   
#   ref <- data.frame(date=seq.POSIXt(aux$date[1], aux$date[nrow(aux)], by='5 min'))
#   timePlot(aux, names(aux)[-1],y.relation = 'free', avg.time = 'day')
# }
