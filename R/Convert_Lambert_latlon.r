################################################################################
# convert_Lamb_latlon                                                          #
################################################################################
# Funcao para converter coordenadas entre a Proj Lambert Azimuthal Equal-Area 
# para latlon. A equacao usada foi obtida em:
# http://mathworld.wolfram.com/LambertAzimuthalEqual-AreaProjection.html
#
# xc, yc are the coordinates
#
# id=0, from lat,lon to x,y
# id!=0,from x,y to lat,lon 
#
# lambda0,phi1:	        longitude central , paralelo padrão
# r:                    raio da esfera de influencia
#
# Ex.: 
# xyToll1 <- Convert_Lambert_latlon(NULL,NULL,1533274,-947925) 
# xyToll2 <- Convert_Lambert_latlon(x=1533274,y=-947925) 
# # or
# llToxy1 <- Convert_Lambert_latlon(-45,-23)
# llToxy2 <- Convert_Lambert_latlon(lambda=-45,phi=-23)
#
# by Jonatan Dupont Tatsch 
# jonatan[at]model.iag.usp.br
# LCB/IAG/USP  02/15/2009
# Last upadte: 10/02/2009
################################################################################
Convert_Lambert_latlon <- function(xc,yc,id=0)
{
# esses parametros so alteram se for rodar para outro continente
  r       <- 6370997.0                               # raio da circunferencia
  prad    <- (pi/180)                                # para radianos (transformacao graus -> radianos)
  lambda0 <- -60.0  *prad                            # meridiano - longitude
  phi1    <- -15.0  *prad                            # paralelo - latitude

   if(id==0) 
   {  
                 lambda <- xc; phi <- yc 
                 lambda <- lambda*prad
                 phi <- phi *prad
                 tmp <- 1.0+sin(phi1)*sin(phi)+cos(phi1)*cos(phi)*cos(lambda-lambda0)
                  k1 <- sqrt(2.0/tmp)
                   x <- round(k1*cos(phi)*sin(lambda-lambda0)*r,3)
                   y <- round(k1*(cos(phi1)*sin(phi)-sin(phi1)*cos(phi)*cos(lambda-lambda0))*r,3)
                   saida <-array(c(x,y))
                   return(saida)
   } else { 
                 x <- xc; y <- yc
                 tmp2=x*x+y*y
                 if(tmp2 > 0.0) 
                 {
                   rho <- sqrt(tmp2)
                   cc <- 2.0*asin(rho/r/2.0)
                   phi <- asin(cos(cc)*sin(phi1)+y*sin(cc)*cos(phi1)/rho)
                   tmp <-rho*cos(phi1)*cos(cc)-y*sin(phi1)*sin(cc)
                   lambda <- lambda0 + atan(x*sin(cc)/tmp)
                 } else {
                         phi <- phi1
                         lambda <- lambda0
                 }
                  saida2 <- array(c(lambda,phi))*(1/prad)
                  return(saida2)
   }
} # end function ;o)

################################################################################
# Apply the Convert_Lambert_latlon function on a matrix (mat)
# mat is a matrix of 2 columns and n rows
# mat[,1] is the xc
# mat[,2] is the yc
Conv_Vec_Lamb_latlon <- function(mat, id)
{
 res <-  t(sapply(1:nrow(mat), 
                   function(i) Convert_Lambert_latlon(xc = mat[i,1], yc = mat[i,2], id=id) )
 )
 return(res)
} # end function


















