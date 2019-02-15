#===========================================================================================
# vp.setup(3,7)    
# print(DNhist[[1]], vp = vp.layout(1,1))
# Its not mine 
#==========================================================================================

library(grid)

vp.setup <- function(x,y){
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(x,y)))
}

vp.layout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col =y)
}
