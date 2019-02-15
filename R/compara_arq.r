#----------------------------------------------------------------------------
# Script para comparar dois diretórios 
#
#----------------------------------------------------------------------------

rm(list=ls())

library(stringr)

dir1 <- '/data3/TAREFAS/2017_SVG'
dir2 <- '/media/emilia/EMILIA_01/2017_SVG'

a1 <- list.files(dir1, recursive = T, full.names = T)
a2 <- list.files(dir2, recursive = T, full.names = T)

maior_vetor <- function(vec1, vec2){
  if(length(vec1) >= length(vec2)){
    return(vec1)
  } else{
    return(vec2)
  }
}

vetor <- maior_vetor(a1,a2)
tt <- strsplit(vetor, '/')
ttt <- lapply(tt, length)

for(i in 1:length(vetor)){
  fl <- tt[[i]][ttt[[i]]]
}


