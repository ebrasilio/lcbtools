#---------------------------------------------------------------------------------------
# Função para verificar arquivos duplicados entre as coletas e até dados antigos
# que já foram processados e estão repetidos devido a limitação de coleta e 
# formatação dos cartões no campo.
# Exemplos:
# doppel('/data1/DATA/LCB/EXT/2017/24117') - para verificar se a coleta já foi incluida na base
#
# doppel('/data1/DATA/LCB/EXT/2017/24117', '/data1/DATA/LCB/EXT/2017/30017') - para verificar
# se há arquivos com nomes iguais em mais de uma coleta.
#----------------------------------------------------------------------------------------

doppel <- function(dir_new = NULL, dir_full = '/data1/DATA/LCB/EXT/STORAGE/data/C/Clean',
                    pattern='^C[0-9]'){

  if(is.null(dir_new)){
    return(tryCatch('Sem path da coleta'))
  }
  
  library(stringr)
  
  lista_major <- str_split(
                  list.files(path=dir_full, pattern=pattern, recursive=T),
                  "\\/", simplify = T)
  if(ncol(lista_major) == 0){
    return(tryCatch(paste0('**** Coleta ', dir_full, ' sem arquivos de Clima ****\n')))
  } else if(ncol(lista_major > 1)){
    lista_major <- lista_major[,ncol(lista_major)]
  }
  lista_major <- str_split(lista_major, '\\.', simplify = T)[,1]

    
  lista_minor <- str_split(
                  list.files(path=dir_new, pattern=pattern, recursive=T),
                          "\\/", simplify = T)
  if(ncol(lista_minor) == 0){
    return(tryCatch(paste0('**** Coleta ', dir_new, ' sem arquivos de Clima ****\n')))
  }else if(ncol(lista_minor > 1)){
    lista_minor <- lista_minor[,ncol(lista_minor)]
  }
  lista_minor <- str_split(lista_minor, '\\.', simplify = T)[,1]
  
  doppelganger = lista_minor[which(lista_minor %in% lista_major)]
  
  if(length(doppelganger) > 0){
      cat('*** WARNING *** \n*** Arquivo(s) presente em mais de uma coleta ***\n')
      print(paste0("Coleta em observação: ", dir_new))
      
      for(i in 1:length(doppelganger)){
        print(paste0("Arquivo duplicado: ", doppelganger[i]))
      } 
     
  }else{
    cat("**** Nada duplicado nesta coleta****\n")
  }  
}


#------------------------------------------------------------------------
dir_1 = '/data1/DATA/LCB/EXT/STORAGE/data/C/Clean'
pattern = '^[C]'
dir2 = '/data1/DATA/LCB/EXT/2017/30017' 

doppel()


doppel('/data1/DATA/LCB/EXT/2017/30017')
doppel('/data1/DATA/LCB/EXT/2017/26017')


library(beepr)
for(i in 1:11){
  beep(i)
  invisible(readline(prompt="Press [enter] to continue"))
}

