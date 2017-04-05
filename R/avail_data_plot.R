#====================================================================
# data availability view
# input:    data.frame
# output:   plot
# 
#====================================================================

data_avail <- function(mdf)
{
    library(reshape)
    library(ggplot2)
    library(fields)
    library(scales)
    library(plyr)
    
    mdf = melt(data.frame(mdf), id.vars = "date")
    mdf$variable = factor(mdf$variable, levels=rev(levels(mdf$variable)))
    mdf <- ddply(mdf, .(variable), transform, rescale = rescale(value))
    
    plot <- 
        ggplot(mdf, aes(y=variable, x=date)) + 
        geom_tile(aes(fill=value)) + 
        scale_fill_gradient(low="white", high="steelblue") +
        theme(legend.position = "bottom")

    # pdf("teste.pdf", width = 12)
    # print(plot)
    # dev.off()
return(plot)
}


