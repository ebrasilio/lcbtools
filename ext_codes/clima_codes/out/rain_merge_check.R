# TODO tirar  nome do inp.files sem ter que contar

rm(list=ls())
library(openair)

path = "/data1/DATA/LCB/EXT/STORAGE/data/C/merge_resample"
out = "/data1/DATA/LCB/EXT/DATA/"

#inp.files = list.files(path, full.names = T, pattern="clear_merge.txt")
inp.files = list.files(path, full.names = T, pattern="clear_merge_H.txt")


test.data  <- lapply(inp.files, 
                     function(.file) 
                         read.csv(.file))

# separando os dados por variavel
vars = c("Dm.G","Pa.H","Rc.mm","Sm.m.s","Ta.C","Ua..")
aux = list()
for(i in 1:length(test.data)){
    colnames(test.data[[i]])[1] = "date"
    test.data[[i]][1] = as.POSIXct(test.data[[i]]$date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    pos_lab = which(names(test.data[[i]]) %in% vars)
    names(test.data[[i]])[pos_lab] = paste0(names(test.data[[i]])[pos_lab],"_",
                    substr(inp.files[i], nchar(inp.files[i])-19, nchar(inp.files[i])-17))
    aux[[i]] = test.data[[i]][c(1, pos_lab)]
}

df = Reduce(function(x, y) merge(x, y, all=T, by = "date"), aux)

wd_ws = df[,c(1, which(substr(names(df),1,4) %in% c("Dm.G","Sm.m")))]
Pa =    df[,c(1, which(substr(names(df),1,4) %in% c("Pa.H")))]
Chuva = df[,c(1, which(substr(names(df),1,4) %in% c("Rc.m")))]
Ta =    df[,c(1, which(substr(names(df),1,4) %in% c("Ta.C")))]
ur =    df[,c(1, which(substr(names(df),1,4) %in% c("Ua..")))]

names(wd_ws)[-1]=substr(names(wd_ws)[-1], nchar(names(wd_ws)[-1])-2, nchar(names(wd_ws)[-1]))
names(Pa)[-1]=substr(names(Pa)[-1], nchar(names(Pa)[-1])-2, nchar(names(Pa)[-1]))
names(Chuva)[-1]=substr(names(Chuva)[-1], nchar(names(Chuva)[-1])-2, nchar(names(Chuva)[-1]))
names(Ta)[-1]=substr(names(Ta)[-1], nchar(names(Ta)[-1])-2, nchar(names(Ta)[-1]))
names(ur)[-1]=substr(names(ur)[-1], nchar(names(ur)[-1])-2, nchar(names(ur)[-1]))
  
# dados originais
write.csv(wd_ws,paste0(out, "WD_WS_H.csv"), row.names = F)
write.csv(Pa,paste0(out, "Pa_H.csv"), row.names = F)
write.csv(Chuva,paste0(out, "Chuva_H.csv"), row.names = F)
write.csv(Ta,paste0(out, "Ta_H.csv"), row.names = F)
write.csv(ur,paste0(out, "UR_H.csv"), row.names = F)


# chuva diária
# chuva_dia = timeAverage(Chuva, avg.time = "day", statistic = "sum")
# write.csv(chuva_dia, paste0(out, "Chuva_Dia.csv"), row.names = F)
    
    
# pdf(paste0(out,"Check_2017_new.pdf"), width=15, height = 12)
#     timePlot(wd_ws, names(wd_ws)[seq(2,ncol(wd_ws),2)], main="WD", key.columns = 6, ylab = "", y.relation = "free")
#     timePlot(wd_ws, names(wd_ws)[seq(3,ncol(wd_ws),2)], main="WS", key.columns = 6, ylab = "", y.relation = "free")
#     timePlot(Pa, names(Pa)[-1], main="Press", key.columns = 6, ylab = "", y.relation = "free")
#     timePlot(Chuva, names(Chuva)[-1], main="Chuva", key.columns = 6, ylab = "", y.relation = "free")
#     timePlot(Ta, names(Ta)[-1], main="Tar",key.columns = 6, ylab = "", y.relation = "free")
#     timePlot(ur, names(ur)[-1], main="UR", key.columns = 6, ylab = "", y.relation = "free")
# dev.off()    
#     
# pdf(paste0(out,"Check_2017_chuvaV2.pdf"), width=15, height = 12)    
# dia = unique(substr(selectByDate(Chuva, year= 2017)$date,1,10))
# 
# for(i in 1:length(dia)){
#     cat(i,"\n")
#     timePlot(selectByDate(Chuva, dia[i],dia[i]),names(Chuva)[-1], pch=rep(20,ncol(Chuva)),
#              y.relation="free", main=dia[i], ylab = "", key.columns = 6)    
# }
# dev.off()

pdf('/data3/TAREFAS/emilia/extrema_2019/Check_all_vars_H.pdf', width=15, height = 12)
    timePlot(wd_ws, names(wd_ws)[seq(2,ncol(wd_ws),2)], main="WD", key.columns = 6, ylab = "", y.relation = "free")
    timePlot(wd_ws, names(wd_ws)[seq(3,ncol(wd_ws),2)], main="WS", key.columns = 6, ylab = "", y.relation = "free")
    timePlot(Pa, names(Pa)[-1], main="Press", key.columns = 6, ylab = "", y.relation = "free")
    timePlot(Chuva, names(Chuva)[-1], main="Chuva", key.columns = 6, ylab = "", y.relation = "free")
    timePlot(Ta, names(Ta)[-1], main="Tar",key.columns = 6, ylab = "", y.relation = "free")
    timePlot(ur, names(ur)[-1], main="UR", key.columns = 6, ylab = "", y.relation = "free")
dev.off()    



datas = c("01/01/2015","17/01/2015","02/02/2015","23/04/2015",
          "09/05/2015","10/06/2015","26/06/2015","12/07/2015",
          "28/07/2015","13/08/2015","29/08/2015","16/10/2015",
          "08/03/2016","09/04/2016","25/04/2016","12/06/2016",
          "28/06/2016","14/07/2016","30/07/2016","15/08/2016",
          "16/09/2016","02/10/2016","19/11/2016") 
datas = data.frame(date = as.POSIXct(datas, format = "%d/%m/%Y", tz = "GMT"))

tta = Ta[which(substr(Ta$date,1,10) %in% as.character(datas$date)),]
for(i in 1:nrow(datas)){
    timePlot(selectByDate(tta, datas$date[i], datas$date[i]), names(tta)[-1], 
             y.relation="free", main=datas$date[i])
}








