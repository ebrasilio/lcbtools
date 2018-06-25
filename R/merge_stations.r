#------------------------------------------------------------
# EMSB 14/06/2018
# Script para juntar dados das estações que estão separados 
# por ano 
# dir_input: raw_data/base
# dir_output: data/base
#------------------------------------------------------------
rm(list=ls())

library(stringr)

# bacia_paraiba_sul   ok 
# cetesb ok
# daee  ok
# iac  (20min e hora)
# iag_cientec  ok 
# inmet  ok 284 e 489
# metar 330 de 335- problemas nos dados do Glauber


args <- commandArgs(TRUE)
if(length(args) == 0){
  stop("\n *** Missing arguments *** \n 
       Use: Rscript lib/merge_stations.r base_de dados \n
          Opções: \n \t \t bacia_paraiba_sul \n \t \t cetesb \n \t \t daee_plu 
       \t \t daee_flu  iac_20min \n \t \t iac_hourly")
}else {
  base <- args[1]
  if(base == 'daee_plu'){
    id <- 'plu'
  } else if(base == 'daee_flu'){
    id <- 'flu'
  } else {
    id <- substr(base,1,3)  
  }
}

# preparar diretórios
if(base == 'daee_plu' | base == 'daee_flu'){
  if(!dir.exists('data/daee')){
    dir.create('data/daee')
  }
} else if(!dir.exists(paste0('data/',base))){
  dir.create(paste0('data/',base))
} 

# listar estações
if(base == 'daee_plu'){
  ll <- list.files('raw_data/daee/data/plu/', 
                   pattern = ".csv",
                   recursive = T)
  sta <- sort(unique(substr(ll, 6, nchar(ll)-9)))
}else if(base == 'daee_flu'){
  ll <- list.files('raw_data/daee/data/flu/', 
                   pattern = ".csv",
                   recursive = T)
sta <- sort(unique(substr(ll, 6, nchar(ll)-9)))
}else if(base == 'cetesb'){
  ll <- list.files(paste0('raw_data/',base,'/data'), 
                   pattern = ".csv",
                   recursive = T)
  sta <- sort(unique(substr(ll, 6, nchar(ll)-4)))
}else{
  ll <- list.files(paste0('raw_data/',base,'/data'), 
                   pattern = ".csv",
                   recursive = T)
  sta <- sort(unique(substr(ll, 6, nchar(ll)-9)))  
}


# 2- ler dados de cada estação ano a ano

for(i in 1:length(sta)){
  cat("Sta:", sta[i], "\n")
  if(base == 'daee_plu'){
    ll <- list.files('raw_data/daee/data/plu/',
                     pattern = '.csv',
                     recursive = T,
                     full.names = T)
    sta_aux <- ll[str_detect(ll, sta[i])] 
  }else if (base == 'daee_flu'){
    ll <- list.files('raw_data/daee/data/flu/',
                     pattern = '.csv',
                     recursive = T,
                     full.names = T)
    sta_aux <- ll[str_detect(ll, sta[i])] 
  }else{
    ll <- list.files(path = paste0('raw_data/',base),
                     pattern = '.csv',
                     recursive = T,
                     full.names = T)
    sta_aux <- ll[str_detect(ll, sta[i])]  
  }
  test.dt <- lapply(sta_aux, read.csv)
  dt.sta <- do.call(rbind, test.dt)
  if(base == 'daee_plu'){
    write.csv(dt.sta, 
              paste0('data/daee/plu_', sta[i],'.csv'),
              row.names = FALSE,
              quote = FALSE)  
  }else if(base == 'daee_flu'){
    write.csv(dt.sta, 
              paste0('data/daee/flu_', sta[i],'.csv'),
              row.names = FALSE,
              quote = FALSE)
  }else{
    write.csv(dt.sta, 
              paste0('data/',base,'/',id, '_', sta[i],'.csv'),
              row.names = FALSE,
              quote = FALSE)
  }
}

