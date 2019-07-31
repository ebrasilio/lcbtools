

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