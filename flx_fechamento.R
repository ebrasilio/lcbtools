

library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(tidyr) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# torre 1

out <- 'data/Ext_2020/out/'
rad <- read.csv(paste0(out, 'EXT_Torre1_Radiation_30min.csv'), 
                na.strings = '9999', stringsAsFactors = F)
rad$date <- as.POSIXct(rad$date, tz='GMT')
rad$Net [ which( rad$Net < -200)] <- NA
#timePlot(rad, names(rad)[2])

shf <- read.csv(paste0(out, 'extrema_shf_Torre1_30min.csv'))
shf$date <- as.POSIXct(shf$date, tz='GMT')
shf$SHF1[ which(shf$SHF1 > 100 | shf$SHF1 < -50) ] <- NA
shf$SHF2[ which(shf$SHF2 > 100 | shf$SHF2 < -50) ] <- NA
shf$SHF3[ which(shf$SHF3 > 100 | shf$SHF3 < -50) ] <- NA
shf$G <- apply(shf[,4:6], 1, function(x) mean(x, na.rm=T))

t1 <- merge(rad[,1:2], shf[,c(1,7)], all=T)
t1 <- merge(t1, flux1[, c(1, 4,6,8)], all=T)

# --------
t1_dia <- timeAverage(t1, avg.time = '1 day', data.thresh = 50)

t1_count_mes <- function(mydt, dtime = 'day'){
  
dia_mes = as.matrix(c(31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,
                        30,31,31,30,31,30,31), bycol=T)

  
}



# torre 2
rad <- read.csv(paste0(out, 'extrema_rad_Torre2_30min.csv'))
rad$date <- as.POSIXct(rad$date, tz='GMT')
rad$Net_Avg [ which( rad$Net_Avg > 1000)] <- NA
rad$G <- apply(rad[,4:6], 1, function(x) mean(x, na.rm=T))

t2 <- merge(rad[,c(1,11,12)], flux2[, c(1, 4,6,8)])

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Heatmap


library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
#library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 


thour <- timeAverage(t1, avg.time='1 hour', data.thresh = 0)
torre1 <- data.frame(id='Torre-1', hour=as.numeric(substr(thour$date,12,13)),
                     day=as.numeric(substr(thour$date,9,10)),
                     month=as.numeric(substr(thour$date,6,7)),
                     year=as.numeric(substr(thour$date,1,4)), thour[,-1])


# data <- data(Trentino_hourly_T,package = "Interpol.T")
# 
# names(h_d_t)[1:5]<- c("stationid","date","hour","temp","flag")
# df <- tibble(h_d_t) %>%
#   filter(stationid =="T0001")
# 
# df <- df %>% mutate(year = year(date),
#                     month = month(date, label=TRUE),
#                     day = day(date))
# 
# df$date<-ymd(df$date) # not necessary for plot but 
#useful if you want to do further work with the data

#cleanup
# rm(list=c("h_d_t","mo_bias","Tn","Tx",
#           "Th_int_list","calibration_l",
#           "calibration_shape","Tm_list"))
# 
# 
# #create plotting df
# df <-df %>% select(stationid,day,hour,month,year,temp)%>%
#   fill(temp) #optional - see note below

# Re: use of fill
# This code is for demonstrating a visualisation technique
# There are 5 missing hourly values in the dataframe.

# see the original plot here (from my ggplot demo earlier this year) to see the white spaces where the missing values occcur:
# https://github.com/johnmackintosh/ggplotdemo/blob/master/temp8.png 

# I used 'fill' from  tidyr to take the prior value for each missing value and replace the NA
# This is a quick fix for the blog post only - _do not_ do this with your real world data

# Should really use either use replace_NA or complete(with fill)in tidyr 
# OR 
# Look into more specialist way of replacing these missing values -e.g. imputation.



#statno <-unique(df$stationid)
statno <- unique(torre1$id)


######## Plotting starts here#####################
p <-ggplot(torre1, aes(day,hour,fill=H))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")



# p <-ggplot(df,aes(day,hour,fill=temp))+
#   geom_tile(color= "white",size=0.1) + 
#   scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rad <- read.csv('/home/emilia/data/Ext_2020/out/EXT_Torre1_Radiation_30min.csv', na.strings = '9999', stringsAsFactors = F)
rad$date <- as.POSIXct(rad$date, tz='GMT')
rad$Net [ which( rad$Net < -200)] <- NA

shf <- read.csv('/home/emilia/data/Ext_2020/out/extrema_shf_Torre1_30min.csv')
shf$date <- as.POSIXct(shf$date, tz='GMT')
shf$SHF1[ which(shf$SHF1 > 100 | shf$SHF1 < -50) ] <- NA
shf$SHF2[ which(shf$SHF2 > 100 | shf$SHF2 < -50) ] <- NA
shf$SHF3[ which(shf$SHF3 > 100 | shf$SHF3 < -50) ] <- NA
shf$G <- apply(shf[,4:6], 1, function(x) mean(x, na.rm=T))

flx1 <- read.csv('/home/emilia/data/Ext_2020/out/Flx_Extrema_allvars_torre1.csv')
t1 <- merge(rad[,1:2], shf[,c(1,7)], all=T)
t1 <- merge(t1, flx1[, c(1,4,6,8)], all = T)

#--- torre 2
rad <- read.csv('/home/emilia/data/Ext_2020/out/extrema_rad_Torre2_30min.csv')
rad$date <- as.POSIXct(rad$date, tz='GMT')
rad$Net_Avg [ which( rad$Net_Avg > 1000)] <- NA
rad$G <- apply(rad[,4:6], 1, function(x) mean(x, na.rm=T))

flx2 <- read.csv('/home/emilia/data/Ext_2020/out/Flx_Extrema_allvars_torre2.csv')
t2 <- merge(rad[,c(1,11,12)], flx2[, c(1, 4,6,8)], all=T)







# chunk 1
#--- Leitura dos dados de 30 minutos ---
#--- torre1

# rad <- read.csv('/home/emilia/data/Ext_2020/out/EXT_Torre1_Radiation_30min.csv', na.strings = '9999', stringsAsFactors = F)
# rad$date <- as.POSIXct(rad$date, tz='GMT')
# rad$Net [ which( rad$Net < -200)] <- NA
# 
# shf <- read.csv('/home/emilia/data/Ext_2020/out/extrema_shf_Torre1_30min.csv')
# shf$date <- as.POSIXct(shf$date, tz='GMT')
# shf$SHF1[ which(shf$SHF1 > 100 | shf$SHF1 < -50) ] <- NA
# shf$SHF2[ which(shf$SHF2 > 100 | shf$SHF2 < -50) ] <- NA
# shf$SHF3[ which(shf$SHF3 > 100 | shf$SHF3 < -50) ] <- NA
# shf$G <- apply(shf[,4:6], 1, function(x) mean(x, na.rm=T))
# 
# flx1 <- read.csv('/home/emilia/data/Ext_2020/out/Flx_Extrema_allvars_torre1.csv')
# t1 <- merge(rad[,1:2], shf[,c(1,7)], all=TRUE)
# t1 <- merge(t1, flx1[, c(1, 4,6,8)], all=TRUE)
# 
# #--- torre 2
# rad <- read.csv('/home/emilia/data/Ext_2020/out/extrema_rad_Torre2_30min.csv')
# rad$date <- as.POSIXct(rad$date, tz='GMT')
# rad$Net_Avg [ which( rad$Net_Avg > 1000)] <- NA
# rad$G <- apply(rad[,4:6], 1, function(x) mean(x, na.rm=T))
# 
# flx2 <- read.csv('/home/emilia/data/Ext_2020/out/Flx_Extrema_allvars_torre2.csv')
# t2 <- merge(rad[,c(1,11,12)], flx2[, c(1, 4,6,8)], all=TRUE)
# 
# ```
# 
# ```{r, include=FALSE}
# timePlot(t1, names(t1)[2:5], avg.time='1 hour',main='Médias hora - Torre 1', y.relation = 'free')
# timePlot(t2, names(t2)[2:5], avg.time='1 hour',main='Médias hora - Torre 2', y.relation = 'free')
