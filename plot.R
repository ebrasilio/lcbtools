rm(list=ls())

library(ggplot2)
library(plyr)
library(ggbreak)
library(scales)

#---- Functions ----
# Multiple plot function
#
# ggplot objects can be passed in or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


inp <- read.csv("events_all_BC.csv", sep=";", dec=".", skip=2)
names(inp) <- c("ID", "Origem", "LON", "lat", "X", "Y", "MLv","Energia", "C")

#stats
inp$date <- as.POSIXct(inp$Origem, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
inp$mY <- substr(inp$date,1,7)
inp <- inp [inp$C == "I",]
my_bar <- ddply(inp, c("mY","C")
                , summarise,
                N = length(Energia),
                Emean = mean(Energia),
                Etot = sum(Energia))
my_bar$date <- as.Date(paste0(my_bar$mY,'-01'), format="%Y-%m-%d")
ref <- data.frame(date=seq.Date(as.Date('2019-06-01'), as.Date('2024-06-01'), by='month'))

tt <- merge(ref, my_bar, all.x=T)


# histograma
plt1 <- 
  ggplot(my_bar,aes(x=date, y=N)) +
  geom_bar(stat="identity", fill='lightgreen', color='black') + 
  geom_text(aes(label=N, y=N+1.5)) + 
  ylab("Nº Eventos Induzidos") +
  xlab("") +
  scale_x_date(breaks = "4 month", date_labels = "%b-%y") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_text(face = 'bold', size=14),
        axis.text = element_text(face="bold", size=10, angle = 0),
        panel.border = element_rect(colour='black'),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.3, "cm"))

plt2 <- 
  ggplot(my_bar,aes(x=date, y=Etot)) +
  geom_bar(stat="identity", fill="salmon", color='black') +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks() + 
  scale_x_date(breaks = "4 month", date_labels = "%b-%y")+  
  ylab("Energia Total (J)")+
  xlab("")+
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_text(face = 'bold', size=14),
        axis.text = element_text(face="bold", size=10, angle = 0),
        panel.border = element_rect(colour='black'),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.3, "cm"))

  




# ------------------------------------------
#pp <- multiplot(plt1, plt2, cols=1)

pdf("plot_BC_2019-2024.pdf", width = 12)
  multiplot(plt1, plt2, cols=1)
dev.off()




#-------------------------------------------------------------------------------

# inp <- read.csv('Downloads/ipt/events-all.csv', sep=";", dec=".", skip=2)
# names(inp) <- c("ID", "time", "lon", "lat", "X", "Y", "mag","Energia", "C")
# inp$mag[ which(inp$mag < 0) ] <- 0
# #stats
# inp$date <- as.POSIXct(inp$time, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
# evt <- inp[117:129,]
# write.csv(evt, file='ipt/evento.csv', quote = F)

#------------------------------------------------------------------------------

# library(sf)
# library(geobr)
# library(elevatr)

# sp <- read_state(code_state = 'SP', year=2020)

#topografia
# sp_top <- get_elev_raster(sp, z=8, #resolução
#                 clip='location')

# plot(sp_top)


# br.muni <- read_municipality()
# sp.muni <- br.muni [br.muni$abbrev_state == 'SP',]
# reg <- read_municipality(code_muni = )
# lookup_muni(name_muni =  'Paraibuna')

# pp <- read_municipality(code_muni = c(3535606))
# read
# plot(pp)

#------------------------------------------------------------------------------



