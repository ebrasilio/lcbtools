####
### Lê dados de clima e separa dados por Variável
##  TODO: Organizar função para buscar variavel e periodo de tempo separadamente
#

path = "/data1/DATA/LCB/EXT/outExt/Clima"
out <- "/data1/DATA/LCB/EXT/outExt/Clima_por_variavel"

#inp.files = list.files(path, full.names = T, pattern="clear_merge.txt")
inp.files = list.files(path, full.names = T, pattern="clear_merge_H.txt")

#---- separar o nome das estações - ainda não está a prova de erros
sta <- unique(substr(str_split_fixed(inp.files, '/',8)[,8],1,3))

#sta <- substr(inp.files, 40, 42)

test.data  <- lapply(inp.files, 
                     function(.file) 
                       read.csv(.file, na.strings = c("-9999", "NA")))
for(i in 1:length(test.data)) {
  names(test.data[[i]] ) [-1] <- 
    paste0( names(test.data[[i]])[-1], "_", sta[i])                                
}


for(yr in 2023){
  cat('Processando ano: ',yr, '\n')
  #  separando os dados por variavel 
  vars = c("Dm","Pa","Rc","Sm","Ta","Ua")
  
  for(v in 1:length(vars)){
    aux = list()
    
    for(i in 1:length(test.data)){
      colnames(test.data[[i]])[1] = "date"
      test.data[[i]][1] = as.POSIXct(test.data[[i]]$date, format = "%Y-%m-%d %H:%M:%S", 
                                     tz = "GMT")
      pos_lab = which(substr(names(test.data[[i]]),1,2) %in% vars[v])
      aux[[i]] = test.data[[i]][c(1, pos_lab)]
    }
    
    # merging dados de uma varivel para várias estações
    a <- merge(openair::selectByDate(aux[[1]], year=yr), 
               openair::selectByDate(aux[[2]], year=yr), all=T)
    
    for(i in 3:length(aux)){
      cat( 'processando arquivo: ', sta[i], 'ano: ', yr, '\n')
      a <- merge(a, openair::selectByDate(aux[[i]], year=yr), all=T)
    }
    write.table(a, file=paste0(out,'/Ext_', vars[v],'_', yr,'_Hora.csv'),
                row.names = F, sep=',', dec='.')
    # write.table(a, file=paste0(out,'/Ext_', vars[v],'_', yr,'.csv'),
    #             row.names = F, sep=',', dec='.')
  }
  
}

# #####
# # Dados Horarios
# ####
# 
# inp.files = list.files(path, full.names = T, pattern="clear_merge_H.txt")
# sta <- substr(inp.files, 41, 43)
# 
# test.data  <- lapply(inp.files, 
#                      function(.file) 
#                        read.csv(.file, na.strings = c("-9999", "NA")))
# # for(i in 1:length(test.data)) {
# #   names(test.data[[i]] ) [-1] <- 
# #     paste0( names(test.data[[i]])[-1], "_", sta[i])                                
# # }
# 
# df = Reduce(function(x, y) merge(x, y, all=T, by = "date"), aux)
# wd_ws = df[,c(1, which(substr(names(df),1,2) %in% c("Dm","Sm")))]
# Pa =    df[,c(1, which(substr(names(df),1,2) %in% c("Pa")))]
# chuva = df[,c(1, which(substr(names(df),1,2) %in% c("Rc")))]
# Ta =    df[,c(1, which(substr(names(df),1,2) %in% c("Ta")))]
# ur =    df[,c(1, which(substr(names(df),1,2) %in% c("Ua")))]
# 
# 
#   #names(wd_ws)[-1]=substr(names(wd_ws)[-1], nchar(names(wd_ws)[-1])-2, nchar(names(wd_ws)[-1]))
#   #names(Pa)[-1]=substr(names(Pa)[-1], nchar(names(Pa)[-1])-2, nchar(names(Pa)[-1]))
#   # names(chuva)[-1]=substr(names(chuva)[-1], nchar(names(chuva)[-1])-2, nchar(names(chuva)[-1]))
  #names(Ta)[-1]=substr(names(Ta)[-1], nchar(names(Ta)[-1])-2, nchar(names(Ta)[-1]))
  #names(ur)[-1]=substr(names(ur)[-1], nchar(names(ur)[-1])-2, nchar(names(ur)[-1]))
  
  # inicio dos dados em 12/08/2014
  # chuva <- selectByDate(chuva, '12/8/2014', '31/12/2025')
  # remoção de dados ruins
  # Dados horários Não devem passar de 100

#  for(i in 2:ncol(chuva)){
#    pos <- which(chuva[,i] > 100)
#    chuva[pos,i] <- NA
#  }

  # dados originais
 # write.csv(wd_ws,paste0(out, "/Ext_WD_WS_Hora.csv"), row.names = F)
 # write.csv(Pa,paste0(out, "/Ext_Pa_Hora.csv"), row.names = F)
  # write.csv(chuva,paste0(out, "/Ext_chuva.csv"), row.names = F)
  #write.csv(Ta,paste0(out, "/Ext_Ta_Hora.csv"), row.names = F)
  #write.csv(ur,paste0(out, "/Ext_UR_Hora.csv"), row.names = F)
  # write.csv(a,paste0(out, "/Ext_chuva.csv"), row.names = F)
#}
#------------------------------------------------------------------------------------------------------------------------
# chuva diária

  #path = "/data/ext/out/"
  #out  = paste0("/data/ext/out/")
  #inp.files = list.files(path, full.names = T, pattern="clear_merge_D.txt")
  #test.data  <- lapply(inp.files, 
  #                     function(.file) 
  #                       read.csv(.file, na.strings = c("-9999", "NA")))
  #aux = list()
  #for(i in 1:length(test.data)){
#    test.data[[i]][1] = as.POSIXct(test.data[[i]]$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
   # pos_lab = which(names(test.data[[i]]) %in% 'Rc')
    # names(test.data[[i]])[pos_lab] = paste0(names(test.data[[i]])[pos_lab],"_",
    #                                           substr(inp.files[i], nchar(inp.files[i])-17, nchar(inp.files[i])-15))
    #names(test.data[[i]])[pos_lab] = paste0(names(test.data[[i]])[pos_lab],"_",
    #                                        substr(inp.files[i], nchar(inp.files[i])-19, nchar(inp.files[i])-17))
    #aux[[i]] = test.data[[i]][c(1, pos_lab)]
  #}

  #df = Reduce(function(x, y) merge(x, y, all=T, by = "date"), aux)
  #chuva = df[,c(1, which(substr(names(df),1,2) %in% c("Rc")))]
  #names(chuva)[-1]=substr(names(chuva)[-1], nchar(names(chuva)[-1])-2, nchar(names(chuva)[-1]))

  # dados originais

  #write.csv(chuva, paste0(out, "Ext_chuva_Dia.csv"), row.names = F)
# aux2 <- list
#   for(i in 1:length(aux)){
#     aux2[[i]] = aux[[i]][c(1, pos_lab)]
#   }
#   
  
  
  
  
 