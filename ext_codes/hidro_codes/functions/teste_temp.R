cleanFiles <- function(mydt) {
  if (length(mydt) != 0) {
    
    # label = gsub(",,", "", mydt[substr(mydt, 1, 4) == "T_St"][1])
    # if (is.null(label) | is.na(label)) {
    #   label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    # } else if (label != "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm") {
    #   label = "T_St,Bat mV,S_10cm,S_20cm,S_30cm,S_40cm,S_60cm,S_100cm"
    # }
    
    mydt <- gsub(",,", "", mydt)                                             # campos vazios
    mydt <- gsub("/", "-", mydt)                                           # separador tempo
    mydt <- mydt[which(is.na(str_match(mydt, "Bateria descarregada")))]
    mydt <- mydt[str_count(mydt, ',') == 7]                          # remove linhas incompletas
    mydt <- mydt[-c(grep("[A-z]", substr(mydt, 1, 1)))]  # remove cabeçalhos e textos
    mydt <- mydt[!(str_locate(mydt, ",")[, 1] != 17)]                    # campo da data incompleto
    
    # acertando timestamp
    if (length(mydt) != 0) {
      ts = unlist(
             lapply(strsplit(mydt, ","), function(x){return(x[1])}))
      data = matrix(unlist(strsplit(substr(ts, 1, 10), "-")), ncol = 3, byrow = T)
      a = apply(data, 2, function(x)unique(nchar(x)))
      
      if (length(unique(unlist(lapply(a, length)))) == 1) {
        yr_pos = which(apply(data, 2, function(x)unique(nchar(x))) == 4)
        if (yr_pos == 1) {
          fmt = "%Y-%m-%d %H:%M"
          aux <- mydt
        } else {
          aux = paste0(
            substr(mydt, 7, 10),
            substr(mydt, 3, 6),
            substr(mydt, 1, 2),
            substr(mydt, 11, nchar(mydt))
          )
          fmt = "%Y-%m-%d %H:%M"
        }
      } else {
        # problemas com data sem padrão
        pos2 = which(nchar(data[, 1]) == 2)
        cat('\n **** entrando no laço correto ****\n')
        mydt[pos2] = paste0(
          substr(mydt[pos2], 7, 10),
          substr(mydt[pos2], 3, 6),
          substr(mydt[pos2], 1, 2),
          substr(mydt[pos2], 11, nchar(mydt[pos2]))
        )
        aux <- mydt
        fmt = "%Y-%m-%d %H:%M"
        ts = unlist(lapply(strsplit(mydt, ","), function(x) {
          return(x[1])
        }))
      }
      
      # acertando os dados
      aux1 <-
        data.frame(matrix(
          ncol = 8,
          nrow = length(aux),
          unlist(strsplit(aux, ",")),
          byrow = T
        ))
      names(aux1) =  c("date", unlist(strsplit(label, ","))[-1])
      
      # verificando dados de formato correto e ano no futuro não existente
      year.hj <- substr(Sys.Date(), 1, 4)
      aux1 <- aux1[-c(which(substr(aux1$date, 1, 4) > year.hj)), ]
      
    } else {
      cat("Arquivo Sem dados, apenas cabeçalho \n")
      return()
    } return(aux1)
  } else { #ok arquivo sem dados já na entrada
    cat("Arquivo vazio \n")
    return()
  }
} 