#--------------------------------------------------------
# emsb - 2016-Set
#
#--------------------------------------------------------

rm(list=ls())
setwd("/data1/DATA/EXTREMA")
path <- "Originals"

# only originals
#colectecFiles <- list.files("origins", pattern = ".txt", recursive=T, full.names=T)
colectecFiles <- list.files(path, pattern = ".TXT", recursive=T, full.names=T)
#pos <- which(substr(colectecFiles, nchar(colectecFiles)-2, nchar(colectecFiles)) == "txt")
#colectecFiles <- colectecFiles[pos]

for(j in 1:length(colectecFiles)){
    cat(j, "\t")
    type <- substr(unlist(strsplit(colectecFiles[j], "/"))[length(unlist(strsplit(colectecFiles[j], "/")))],1,1)
    #type <- substr(colectecFiles[j],nchar(colectecFiles[j])-11, nchar(colectecFiles[j])-11)
    out <- paste0("STORAGE/data/",type,"/raw")
        #if(!dir.exists(out)){
        #    dir.create(out)
        #}
    system(paste("rsync -Crapz  --log-file=rsyncTrouble.LOG ", colectecFiles[j], out))    
}


#-------------------------------------------------
# Check
#-------------------------------------------------

path1 <- "/data1/DATA/EXTREMA/STORAGE/data/"
path2 <- "/data1/DATA/EXTREMA/Originals"

a1 <- list.files(path1,recursive=T)
a <- as.vector(matrix(unlist(strsplit(a1,"/")), ncol=3, byrow=T)[,3])

b1 <- list.files(path2, recursive=T)
pos <- which(substr(b1, nchar(b1)-2, nchar(b1)) == "TXT")
b1 <- b1[pos]

b <- strsplit(b1, "/")
pos <- unlist(lapply(b, length))

for(i in 1:length(pos)){
   f <- b[[i]][pos[i]]
   if(length(which(a %in% f)) == 0){
       cat(i, "arquivo", b[[i]], "     não encontrado\n")
   } 
}

 
