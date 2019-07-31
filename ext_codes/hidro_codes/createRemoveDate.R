library(openair)

sta = c('H04','H05','H06','H07','H08','H09','H10','H11','H21','H23','H24')
for(i in 1:length(sta)){
  arq = paste0('../../data/H/Dados_CorrigidosPierre_Nov2017/', sta[i], '_filtrado_manual.txt')
  if(file.exists(arq)){
    cat('\n', sta[i],': estação já tem arquivo de correção\n')
    hp = read.table(arq, sep="\t", header=T, stringsAsFactors = F, na='NA')
    hp$date = as.POSIXct(hp$date, tz='GMT')
    dataR = data.frame(date = hp$date[apply(hp[,-1], 1, function(x){sum(!is.na(x))==0})])
    
    # checar se tem arquivo de datas para remover:
    arq_dr <- paste0(sta[i],'_remove_date.csv')
    if(file.exists(arq_dr)){
        print('abrindo arquivo de data to removed')
        hbase = read.csv(arq_dr)
        hbase$date <- as.POSIXct(hbase$date, tz='GMT')
        #comparar os dois conjuntos
        hbase2 =  data.frame(date=dataR[which(!(dataR$date %in% hbase$date)),])
        hr = merge(hbase, hbase2, all = T)
        write.table(hr, arq_dr, sep=',', row.names = F)
    }else{
      write.table(dataR, arq_dr, sep=',', row.names = F)
    }
  } else{
    cat('\n', sta[i],': arquivo sem coreção\n')
  }
}













# mod2 = merge(h041[,c(1,3)], hp[,1:2], by='date', all=T)
# x = mod2$S_10cm
# x <- x / 1000
# y <- -0.057 - 0.66*x + 8*x^2 - 27.91*x^3 + 49.23*x^4 - 42.46*x^5 + 14.47*x^6
# mod2$S_10cm.C = ifelse(y < 0, NA, y)
# 
# 
# 
# data = mod2[which(!is.na(mod2$S_10cm) & is.na(mod2$mod2.10cm)),1]
# timePlot(selectByDate(mod2, '2017-09-01','2017-09-16'), names(mod2)[3:4], group=T, pch=c('°','°'),y.relation='free')
# 
# timePlot(a, names(a)[3:4], group=T, pch=c('°','°'),y.relation='free')
# 
# a = merge(dt[,c(1,3)], h041[,c(1,3)], by='date', all=TRUE)
# a = merge(a, hp[,1:2], all = T)
# names(a)[-1] = c('novo', 'anterior','pierre')
# 
# data_sta_conv = deltaTConvert(mod2, channels = names(mod2)[2], soilType = "mineral", relation = "polynomial")
# 
# pdf('check_h04.pdf', width=15, height=7)
# for (year in 2014:2017){
#   timePlot(selectByDate(hp, year=year), names(hp)[-1], group=T, pch=c('°','°'),y.relation='free',main=year)
# }
# for (year in 2014:2017){
#   timePlot(selectByDate(h041, year=year), names(h041)[3:8], group=T, pch=c('°','°'),y.relation='free',main=year)
# }
# dev.off()
# 
# # gerar tabela de dados a serem removidos:
# 
