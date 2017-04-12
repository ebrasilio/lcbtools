#-----------------------------------------------------------------------------------------
# Function to read monodata from LCB-database
#-----------------------------------------------------------------------------------------

readLcbData <- function(inp=NULL, sitio=NULL, avg.time="30 min"){

# inicial conditions 
if(is.null(inp) | is.null(sitio)){ 
   cat("\n Use inp = vector with files \n and sitio = PDG ou SVG\n")
   return()
}

if(sitio == "PDG" | sitio == "PEG" | sitio == "pdg" | sitio == "peg") sitio <- "PDG1"
if(sitio == "SVG" | sitio == "svg") sitio <- "SVG1"
if(sitio == "BAN" | sitio == "ban") sitio <- "BAN1"

path <- "/data1/BANCO/SITES/"
test.data  <- lapply(inp, 
                function(.file) 
                  read.table(paste0(path, sitio,"/", .file, sep = ""),
                    stringsAsFactors = F, header = FALSE, 
                    colClasses = c(rep("character",4),"numeric"),
                    na.strings = c("-7777.00000000","-9999.00000000","-6999.00000000")))

test.data <- do.call(cbind, test.data)


if(length(inp) > 1){
	mdata<- data.frame(test.data[,c(1:5,seq(10,ncol(test.data), 5))])
}else if(length(inp) == 1){
	mdata <- data.frame(test.data[,1:5])
}


# avg.time options from data base
if(avg.time == "month"){
    date  <- as.POSIXct(paste(mdata$V1, substr(mdata$V2,2,3), "01"), 
                        format = "%Y %m %d", tz  = "GMT")
}else{
    date  <- as.POSIXct(paste(mdata$V1, mdata$V2, mdata$V3, mdata$V4), 
                        format = "%Y %j %H %M", tz  = "GMT")
}
mdata <- data.frame(date = date, mdata[, -c(1:4)])

if(sum(substr(inp, nchar(inp)-2 , nchar(inp)) %in% c(".hh",".1d")) > 0){
    names(mdata) <- c("date", substr(inp, 9, nchar(inp)-3))
}else{
    names(mdata) <- c("date", substr(inp, 9, nchar(inp)-4))    
}


return(mdata)
}
