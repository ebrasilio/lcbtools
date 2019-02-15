#-----------------------------------------------------------------#
# check and plot the last 3 month of data                         #
#-----------------------------------------------------------------#
rm(list=ls())

source('~/git/lcbtools/R/readLcbData.R')
sitio <- 'PDG1'

ll <- list.files(paste0("/data1/BANCO/SITES/", site), pattern = ".1d")[-c(1:3)]

dt <- readLcbData(inp=ll, avg.time='1 day', sitio=site)

# 3 months
hoje <- as.POSIXlt(Sys.Date(), format = "%Y-%m-%d", tz="GMT")
inicio <- hoje - 90*24*60*60

dt90 <- selectByDate(dt, inicio, hoje)
varsi <- seq(2, ncol(dt90),5)
varsf <- c(seq(2, ncol(dt90),5)[-1]-1, ncol(dt90))

for(i in 1:length(varsi)){
  timePlot(dt90, names(dt90)[varsi[i]:varsf[i]],
           y.relation = "free", main='90 dias')  
}
#ano
for(i in 1:length(varsi)){
  timePlot(selectByDate(dt, year=2017), names(dt90)[varsi[i]:varsf[i]],
           y.relation = "free", main = 2017)  
}

