#-----------------------------------------------------------------------------------------
# Function to read monodata from LCB-database
#-----------------------------------------------------------------------------------------

readLcbData <- function(inp=NULL){
if(is.null(inp){ 
   cat("\n Use inp = vector with files address to read \n")
   return()
}

test.data  <- lapply(inp, 
                function(.file) 
                  read.table(paste0(path, sitio, .file, sep = ""),
                    stringsAsFactors = F, header = FALSE, 
                    colClasses = c(rep("character",4),"numeric"),
                    na.strings = c("-7777.00000000","-9999.00000000")))

test.data <- do.call(cbind, test.data)
mdata <- data.frame(test.data[,c(1:5,seq(10,ncol(test.data), 5))])
date  <- as.POSIXct(paste(mdata$V1, mdata$V2, mdata$V3, mdata$V4), 
                    format = "%Y %j %H %M", tz  = "GMT")
mdata <- data.frame(date = date, mdata[, -c(1:4)])
