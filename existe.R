
for(yr in 2016:2019){
  cat ('\n',yr)

n20 <- paste0('/media/emilia/Ext_2020/data/', yr)
old <- paste0('/data1/DATA/LCB/EXT/data/', yr)

sta <- substr(list.dirs(n20)[-1], nchar(list.dirs(n20)[-1])-2, nchar(list.dirs(n20)[-1]))

for(i in 1:length(sta)){
  ff <- list.files(paste0(n20, '/', sta[i]))
  if(length(ff) != 0){
    for(j in 1:length(ff)){
      a = system(paste0('find ',old, ' -name ', ff[j]), intern=T)
      if(length(a) == 0){
#        cat('\n*** Arquivos não encontrados na base antiga *** \n ', sta[i], '\n', ff[j])
        write.table(c('*** Arquivos não encontrados na base antiga ***', sta[i], ff[j]),
                    paste0('log_comparabase_',yr,'.txt'), append=T, col.names=F,row.names=F)
      }
    }
  }

  ff <- list.files(paste0(old, '/', sta[i]))
  if(length(ff) != 0){
    for(j in 1:length(ff)){
      a = system(paste0('find ',n20, ' -name ', ff[j]), intern=T)
      if(length(a) == 0){
#        cat('\n*** Arquivos não encontrados na base nova *** \n ', sta[i], '\n', ff[j])
        write.table(c('*** Arquivos não encontrados na base nova ***', sta[i],  ff[j]),
                    paste0('log_comparabase_',yr,'.txt'), append=T, col.names=F,row.names=F)
      }#else{
        #cat('\n Tudo lindo \t ')
      #}
    }
  }
}

}
