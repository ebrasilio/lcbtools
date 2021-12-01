

#path <- "/data1/DATA/LCB/EXT/STORAGE/data/H/merge/H04_merge.txt"
cDoubleLines <- function(path){
  library(readr)
  library(dplyr)
  
  mydt <- read_csv(path)
  a <- mydt %>% 
        group_by(date) %>% 
          summarise(n_distinct(date))
  pos <- which(a$`n_distinct(date)` != 1)
  if(length(pos) == 0){
    cat('Sem repetição \n')
  }else{
    cat('Linhas duplicadas:\n')
    a[pos,]
    return(a[pos,])
  }
}

#==============================================================================================
# Remover arquivos de status, que não contém dados

checkFiles <- function(filesCheck){
  
  csv = which(substr(filesCheck, nchar(filesCheck)-3, nchar(filesCheck)) =='.csv')
  if(length(csv) > 0){
    cat("\n Arquivos extranhos:\n")
    for(z in 1:length(csv)) {cat(filesCheck[csv[z]],'\n')}
    filesCheck <- filesCheck[-csv]
  }
  
  xls = which(substr(filesCheck, nchar(filesCheck)-3, nchar(filesCheck)) =='.xls' |
                substr(filesCheck, nchar(filesCheck)-4, nchar(filesCheck)) =='.xlsx')
  if(length(xls) > 0){
    cat("\n Arquivos extranhos:\n")
    for(z in 1:length(xls)) {cat(filesCheck[xls[z]],'\n')}
    filesCheck <- filesCheck[-xls]
  }
  
  cr1 = which(substr(filesCheck, nchar(filesCheck)-3, nchar(filesCheck)) == '.CR1')
  if(length(cr1) > 0){
    cat("\n Arquivos extranhos:\n")
    for(z in 1:length(cr1)) {cat(filesCheck[cr1[z]],'\n')}
    filesCheck <- filesCheck[-cr1]
  }
  return(filesCheck)
  
}

#==============================================================================================
# # Limpando arquivos
# 
# cleanFiles <- function(mydt){ #A
#   if(length(mydt) != 0){ #B
#     
#     label = gsub(",,","",mydt[substr(mydt,1,4) == "T_St"][1])
#     if(is.null(label)|is.na(label)){
#       label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
#     } else if(label != "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"){
#       label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
#     }
#     
#     mydt <- gsub(",,", "", mydt)                                                    # campos vazios
#     mydt <- gsub("/", "-", mydt)                                                  # separador tempo
#     mydt <- mydt[which(is.na(str_match(mydt, "Bateria descarregada")))]
#     mydt <- mydt[str_count(mydt, ',') == 7]                             # remove linhas incompletas
#     mydt <- mydt[-c(grep("[A-z]", substr(mydt, 1, 1)))]                # remove cabeçalhos e textos
#     mydt <- mydt[!(str_locate(mydt, ",")[, 1] != 17)]                    # campo da data incompleto
#       
#     # acertando timestamp
#     if( length(mydt) == 0 ){ # D
#         cat("Arquivo Vazio \n")
#         return()
#       } else { # D1
#         ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))      # acertando data
#         data = matrix(unlist(strsplit(substr(ts,1,10), "-")), ncol=3,byrow = T)
#         a = apply(data, 2, function(x)unique(nchar(x)))
#         
#         if(length(unique(unlist(lapply(a, length)))) == 1){
#           yr_pos = which(apply(data, 2, function(x)unique(nchar(x))) == 4)
#           if(yr_pos == 1){
#             fmt = "%Y-%m-%d %H:%M"
#             aux <- mydt
#           } else {
#             aux = paste0(
#               substr(mydt,7,10), substr(mydt,3,6), substr(mydt,1,2),
#               substr(mydt,11,nchar(mydt)))
#             fmt = "%Y-%m-%d %H:%M"
#           } #### Até aqui está correto
#         } else {                                                      # problemas com data sem padrão
#           pos2 = which(nchar(data[,1]) == 2)
#           cat('\n **** entrando no laço correto ****\n')
#           mydt[pos2] = paste0(
#             substr(mydt[pos2],7,10),substr(mydt[pos2],3,6), substr(mydt[pos2],1,2),
#             substr(mydt[pos2],11,nchar(mydt[pos2])))
#           aux <- mydt
#           fmt = "%Y-%m-%d %H:%M"
#           ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))
#         }
#         
#         # acertando os dados
#         aux1 <- data.frame(matrix(ncol = 8, nrow = length(aux), unlist(strsplit(aux, ",")), byrow = T))
#         names(aux1) =  c("date",unlist(strsplit(label,","))[-1])
#         
#         # verificando dados de formato correto e ano no futuro não existente
#         year.hj <- substr(Sys.Date(),1,4)
#         aux1 <- aux1[-c(which(substr(aux1$date,1,4) > year.hj)),]
#       } #D1
#     return(aux1)
#     }else{
#     return()
#   } else {
#     cat("Arquivo vazio \n")
#     return()
#   }
# } #função


#==============================================================================================
# Limpando arquivos

cleanFiles <- function(mydt){
  if(length(mydt) != 0){ # if 1
    
    label = gsub(",,","",mydt[substr(mydt,1,4) == "T_St"][1])
    if(is.null(label)|is.na(label)){
      label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    } else if(label != "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"){
      label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    }
    
    if(length(mydt) != 0){
      mydt = gsub(",,", "", mydt)                                             # campos vazios
      mydt = gsub("/", "-", mydt)                                           # separador tempo
      mydt <- mydt[which(is.na(str_match(mydt, "Bateria descarregada")))]
      mydt <- mydt[str_count(mydt,',') == 7]
      pos_grep = grep("[A-z]", substr(mydt,1,1))                     # remove cabeçalho e lixos
      if(length(pos_grep) > 0)mydt = mydt[-pos_grep]
      
      p1 = which(str_locate(mydt, ",")[,1] != 17)                    # campo da data incompleto
      if(length(p1) > 0) mydt = mydt[-p1]                                     # remover linha
      
      # acertando timestamp
      if( length(mydt) == 0 ){
        cat("Arquivo Vazio \n")
        return()
      } else {
        ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))      # acertando data
        data = matrix(unlist(strsplit(substr(ts,1,10), "-")), ncol=3,byrow = T)
        a = apply(data, 2, function(x)unique(nchar(x)))
        
        if(length(unique(unlist(lapply(a, length)))) == 1){
          yr_pos = which(apply(data, 2, function(x)unique(nchar(x))) == 4)
          if(yr_pos == 1){
            fmt = "%Y-%m-%d %H:%M"
            aux <- mydt
          } else {
            aux = paste0(
              substr(mydt,7,10), substr(mydt,3,6), substr(mydt,1,2),
              substr(mydt,11,nchar(mydt)))
            fmt = "%Y-%m-%d %H:%M"
          }
        } else {                                                      # problemas com data sem padrão
          pos2 = which(nchar(data[,1]) == 2)
          cat('\n **** entrando no laço correto ****\n')
          mydt[pos2] = paste0(
            substr(mydt[pos2],7,10),substr(mydt[pos2],3,6), substr(mydt[pos2],1,2),
            substr(mydt[pos2],11,nchar(mydt[pos2])))
          aux <- mydt
          fmt = "%Y-%m-%d %H:%M"
          ts = unlist(lapply(strsplit(mydt, ","), function(x){return(x[1])}))
        }
        
        # acertando os dados
        aux1 <- data.frame(matrix(ncol = 8, nrow = length(aux), unlist(strsplit(aux, ",")), byrow = T))
        names(aux1) =  c("date",unlist(strsplit(label,","))[-1])
        
        # verificando dados de formato correto e ano no futuro não existente
        year.hj <- substr(Sys.Date(),1,4)
        aux1 <- aux1[!(substr(aux1$date,1,4) > year.hj),]
      }
      return(aux1)
    } else{
      return()
    } # if 2
  } # if 1
} #função

