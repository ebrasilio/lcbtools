#====================================================================
# Strings manipulations library
# input:    data.frame
# output:   plot
# 
#====================================================================

separa_vars <- function(mylist, var="PLU", label = NULL)
{
    library(openair)
    
    aux = data.frame(date = mylist[[1]]$date, 
                            mylist[[1]][which(substr(names(mylist[[1]]),1,nchar(var)) == var)])
    if(!is.null(label))names(aux)[2]=label[1]
    for(i in 2:length(mylist)){
        aux = merge(aux,
                    data.frame(date = mylist[[i]]$date,
                                      mylist[[i]][which(substr(names(mylist[[i]]),1,nchar(var)) == var)]),
                    all = T, by = "date")
        if(!is.null(label))names(aux)[i+1]=label[i]
    }
    return(aux)
}
    
    