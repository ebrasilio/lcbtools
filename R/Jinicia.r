###################################################################
###################################################################
# Programa Jinicia.r                                              #
# Desenvolvido por:                                               #
#    Jonatan Tatsch, Lab. CLIMA E BIOSFERA/IAG/USP                #
# Atualizado em:                                                  #
#    São Paulo, 09 de abril de 2009                               #
#                                                                 #
# Este script inicializa algumas funcoes uteis e carrega pacotes  #
# mais frequentemente usados                                      #
###################################################################
 graphics.off()
 rm(list=ls())
#------------------------------------------------------------------
# pacotes instalados via R CMD INSTALL ...
# pacote                                      utilidade
library(fields)                            # funcao image.plot
library(sp)                                # dados espaciais
library(cluster)                           # analise de cluster
library(rgl)                               # geração de superfícies de dados

library(spam)                              # pre-requisito p/ outro
#library(plotrix)                           # pre-requisito p/ drc e multiplos histogramas
#library(alr3)                              # pre-requisito p/ drc

library(abind)                             # cbind de matrizes e manipulacao arrays
library(chron)                             # gera sequencia de datas
library(time)

library(akima)                             # interpolacao de ptos irregulares
library(fCalendar)                         # datas e calendarios 
library(its)                               # irregular time series
library(chron)                             # gera sequencia de datas

library(lattice)                           # funcao levelplot
library(Hmisc)                             # graficos de alto nivel
library(tcltk)
#library(RNetCDF)                          # NAO CONSEGUI INSTALAR - UDUNITS DEPENDENCIA!
#library(udunits)

library(RHydro)                            # geoprocessamento MNT
library(RODBC)                             # pre-requisito p/ outro
library(doBy)                              # muito boa para operacoes estatiscas basicas

library(Amelia)                            # multiple imputation
library(norm)
library(Zelig)
library(boot)

library(climatol)
library(seas)                              # dados de estacao
library(clim.pact)

#-------------------------------------------------------------------------------
# pacotes a serem instalados

# modelos ecologicos no R
#library("deSolve")

#library(ncvar)                  #don't working(!?)
#library(ncdf)
#library(date)
#library(RColorBrewer)            

#Extreme value packages
#library(ismev)
#library(POT)
#library(evd)
#library(evir)
#library(evdbayes)
#library(extRemes)             # abre uma janela indigesta
#library(fExtremes)
#library(labstatR)              # miscelânea de funções estatísticas

#library(cyclones)
#library(verification)
#library(ensembleBMA)
#library(ProbForecastGOP)

#library(clue)

#stat packages
#library(tseries)
#library(fBasics)
#library(nortest)
#library(lmtest)


#library(bootstrap)
#library(iid.test)
#library(copula)
#library(DAAG)
#library(e1071)
#library(fastICA)

#library(Lmoments)
#library(mAr)
#library(Matrix)
#library(mcmc)
#library(moments)
#library(multcomp)
#library(neural)
#library(nlme)
#library(wavelets)
#library(outliers)
#library(pcaPP)
#library(quantreg)
#library(quantregForest)

#library(gtools)               # saplly
#library(stinepack)            # preenche NAS com funcoes racionais
#library(Kendall)

#Map and plotting
library(maps)
library(mapdata)
library(mapproj)

#library(mapLD)            
#library(clines)             # don't working

#library(geoR)
#library(pixmap)
#library(gridBase)
#library(PBSmapping)
#library(oz)
#library(RandomFields)
#library(maptools)
#library(scatterplot3d)
#library(grDevices)            # clic grafico
#library(gplots)               # funcao textplot

# outros pacotes uteis 
#library(schoolmath)           # para saber se numero é par ou impar
#library(car)
#library(qualV)                # set of statistical indexes
#library(gdata)                # write.fwf
#library(caTools)              # media movel e tratamento de imagens hdr 

#require(tkrplot)
#fontHeading <- tkfont.create(family="times",size=40,weight="bold",slant="italic")
#fontHeading1<-tkfont.create(family="times",size=20,weight="bold")
#fontHeading2<-tkfont.create(family="times",size=14,weight="bold")
#fontTextLabel <- tkfont.create(family="times",size=12)
#fontFixedWidth <- tkfont.create(family="courier",size=12)

#================================================================
write("funcoes carregadas:","")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
mediaArray <- function(datarray,intervalo,fun)  
# funcao que calula a media para uma array num intervalo de tempo
# intervalo refere-se o intervalo de dias, de 1:365, p.ex ou 1:31, ...
# intervalo depende da 3a dimensao da array, ou seja de dim(array)[3]
{
 ix <- 1:dim(datarray)[1]
 iy <- 1:dim(datarray)[2]
 ik <- 1:dim(datarray)[3]
  if(fun=="soma"){
                 result <- t( sapply(ix, function(i) 
                              sapply(iy, function(j) sum(datarray[i,j,intervalo], na.rm=T) ) 
                              ) 
                 )  
    result
  } else {
                 result <- t( sapply(ix, function(i) 
                              sapply(iy, function(j) mean(datarray[i,j,intervalo],na.rm=T) )           
                              ) 
                 )  
    result
  }
} # end function mediaArray
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# funcoes auxiliares para trabalhar com datas, anos e doy
is.leapyear <- function(year) {
	year <- round(year)
    if(((year %% 100 !=0) & (year %%4 ==0)) | (year %% 400==0) )
    { return(TRUE) 
	} else { return(FALSE) }
}
 
days.in.year <- function(year) {
	year <- round(year)
	if (is.leapyear(year)) { return(366)
	} else { return(365) }
}

 date.from.doy <- function(doy, year) {
	year <- round(year)
	doy <- round(doy)
	if (doy < 1) { 
		year <- year-1
		doy <- days.in.year(year) + doy
	}
	else if (doy > days.in.year(year)) { 
		doy <- doy - days.in.year(year)
		year <- year+1
	}
	if (doy < 1) { stop('cannot understand value for doy') }
	if (doy > days.in.year(year)) { stop('cannot understand value for doy') }
	
 	as.Date(doy, origin=paste(year, "-01-01", sep=''))-1
 }

#:::::::::::::::::::::::::::::::::::::::
#Funçao que determina o resto de uma divisao
resto <- function(a,b){ 
          sobra<- a %% b
          sobra          
                # For real arguments, %% can be subject to catastrophic loss of accuracy 
                # if x is much larger than y, and a warning is given if this is detected
}
write("resto","")

#:::::::::::::::::::::::::::::::::::::::
#Funçao que determina a parte inteira de uma divisao
integ.div <- function(a,b){ 
          parteinteira<- a %/% b   
          parteinteira
}
write("integ.div","")

#:::::::::::::::::::::::::::::::::::::::
# funcao que determina a anomalia
  anm <- function(y){ 
           anm.s <- y - mean(y,na.rm=T)                      # anomaly series
           anm.s       
} 
write("anm","")

#:::::::::::::::::::::::::::::::::::::::
# funcao que normaliza a serie pelo desvio padrao
  nor.std <- function(y){ 
           nor.s <- y/sd(y,na.rm=T)                          #normalized series
           nor.s     
}
write("nor.std","")
#:::::::::::::::::::::::::::::::::::::::
# funcao para contagem de NAS
  quantos.nas <- function(x){ 
                            n <- length(which(is.na(x)))
                            n
  }

#:::::::::::::::::::::::::::::::::::::::
# funcao regresao linear (output tendencia) 
  tend <- function(y){
           x <- seq(1,length(y))
           reg <- lm(y ~ x)
           tend.s <- reg$coefficients[[2]]  # tendencia da serie
#           tend.s <- reg$coefficients[[1]] 
           tend.s
}
write("nor.std","")

#:::::::::::::::::::::::::::::::::::::::
# funcao regresao linear (output tendencia)
  tend.nas <- function(x,y){
           Fullx <- 1:length(y)
            miss.y <- which(is.na(y))
            x[miss.y] <- NA
                      
           y.reg <- lm(y ~ x, data.frame(x=x, y=y))
            y.predict <- predict(y.reg, data.frame(x=Fullx))

            saida <- lm(y.predict ~ x)
            saida

}
write("tend.nas","")  

#:::::::::::::::::::::::::::::::::::::::
# funcao remocao da tendencia 
  rem.tend <- function(y){
           x <- seq(1,length(y))
           reg <- lm(y ~ x)
           det.s <- y - (reg$coefficients[[2]] * x)  # detrended series
           det.s
}
write("rem.tend","")

#:::::::::::::::::::::::::::::::::::::::
# funcao que determina intervalos de confiança
 ic <- function(x, conf = 0.95){
         n <- length(na.omit(x))
          media <- mean(x,na.rm=T)
           variancia <- var(x,na.rm=T)
            quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
            ic <- media + quantis * sqrt(variancia/n)
            return(ic)
 }
write("ic","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para fazer bootstrap
# sample(base) R Documentation 
# Random Samples and Permutations
# This is safer, but only for sampling without replacement
  resample <- function(x, size, ...)
              if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
              } else sample(x, size, ...)
write("resample","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para mudar a hora de 300,330 para 3,3.5 
# util para transformacao do horario nos arquivos
# de fluxos passados pelo osvaldo
       change.h <- function(x){
                    b<-gsub("0","z", x)
                    bb<-sub("zz","00", b)
                    bbb<-gsub("3z","50", bb)
                    bbbb<-gsub("z","0", bbb)
                    bbbbb<- as.numeric(bbbb)/100
                    bbbbb
}
write("change.h","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para calcular o indice de concordancia
# Willmott index of agreement (Willmott,1982), d2
# 1 value indicating perfect agreement 
       indconc.f <- function(obs,prev){    
             pe <- sum((abs(prev - mean(obs)) + abs(obs - mean(obs)))^2)    # potential error              
             mse<- sum((obs - prev)^2)            
             d2<- 1 - (mse/ pe)            
             d2
}
write("indconc.f","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para listar somente os diretorios 
# de um dado endereço             
       sodir.f <- function(caminho){             
              lista <- dir(caminho)             
               lista_arqs<- dir(caminho,pattern="[.]")              
                pastas <- lista[-c(which(lista %in% lista_arqs))]             
                pastas
}        

#:::::::::::::::::::::::::::::::::::::::
# funcao para fazer a primeira diferença de uma serie
# retorna um vetor de tamanho n-1 em relacao a serie original
       firstdiff <- function(x){             
              fd <- c(x[-c(1)],NA) - x             
              fd <- as.numeric(na.exclude(fd))             
              fd                          
}        
write("firstdiff","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para calcular graus-dia 
     GD.f <- function(tbase,tcut,tmed){
         # tbase=constante
         # tcut=constante
         # tmed=vetor        
      GD <- rep(NA,length(tmed))
        ok <- which(tmed > tbase)
        nobase <- which(tmed <= tbase)
        nocut<- which(tmed >= tcut)
          GD[nobase] <- tbase
          GD[nocut] <- tcut
          GD[ok] <- tmed[ok]
          CGD <- cumsum(GD)
          CGD
 } 
write("GD.f","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para criar labels de datas para o eixo x 
#
  gera.axdate <- function(x,datai){       
                                   stopifnot(nchar(datai)==10) 
                                
                                # variavel a ser plotada
                                # datai é uma string que especifica a data inicial no seguinte formato 
                                # "dd-mm-yyyy" SEMPRE ENTRE ASPAS!!! (ex. "10-04-2007" = 10 de abril de 2007)                               
                                   seqday<- 0:(length(x)-1)                                   
                                   dataini<- as.character(datai)                                                                      
                                      dia <- as.numeric(substr(dataini,1,2))
                                      mes <- as.numeric(substr(dataini,4,5))
                                      ano <- as.numeric(substr(dataini,7,10))                                                                                                                                     
                                      datas<- chron(seqday,out.format = c(dates="d-mon-y"),          # serie das datas
                                                     origin.=c(month = mes, day = dia, year = ano))                                          
                                      meiodomes <- which(substr(datas,1,2) =="15")                                          
                                      inidomes1 <- seq(1,length(x),by=30)
                                      inidomes2 <- seq(1,length(x),by=31)
                                      
                                      labsmm <- substr(datas[meiodomes],1,6)  # labels meio do mes
                                      labs30 <- substr(datas[inidomes1],1,6)
                                      labs31 <- substr(datas[inidomes2],1,6)                                 
                                    # LISTA DE SAIDA:::::::::::::::::::::::::                     		                               		              
				     axdate<- list(posmm= meiodomes, pos30=inidomes1, pos31=inidomes2,
                                 labelsxmm=as.character(labsmm),labelsx30=as.character(labs30),labelsx31=as.character(labs31))
				     axdate                  		              
  }
 write("gera.axdate","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para gerar 3 colunas de ano, mes, dia
#
  gera.data <- function(x,datai){       
                                   stopifnot(nchar(datai)==10) 
                                
                                # variavel a ser plotada
                                # datai é uma string que especifica a data inicial no seguinte formato 
                                # "dd-mm-yyyy" SEMPRE ENTRE ASPAS!!! (ex. "10-04-2007" = 10 de abril de 2007)                               
                                   seqday<- 0:(length(x)-1)
                                    num.cols <- 3                                   
                                    matriz.out <- data.frame (matrix(rep(NA, num.cols * length(x)), ncol = 3, nrow = length(x))) 
                                     names(matriz.out) <- c("ano","mes","dia")
                                                                                                           
                                   dataini<- as.character(datai)                                                                     
                                      dia <- as.numeric(substr(dataini,1,2))
                                      mes <- as.numeric(substr(dataini,4,5))
                                      ano <- as.numeric(substr(dataini,7,10))                                                                                                                                     
                                       datas<- chron(seqday,out.format = c(dates="Year m d"),          # serie das datas
                                                         origin.=c(month = mes, day = dia, year = ano))
                                          matriz.out[,"ano"] <- substr(datas,1,4)
                                          matriz.out[,"mes"] <- substr(datas,6,11)  
                                           
                                      #meiodomes <- which(substr(datas,1,2) ==15)       
                                    
                                      #labs <- substr(datas[meiodomes],4,6)
                                    
                                    # LISTA DE SAIDA:::::::::::::::::::::::::                     		                               		              
				   #  axdate<- data.frame(posicao= meiodomes,labelsx=as.character(labs))
				   #  axdate                  		              
  }
 write("gera.axdate","")
 
#:::::::::::::::::::::::::::::::::::::::
# funcao para remoção do ciclo anual
   desas <-  function(x,dias,num,p) {
                                     
		# x   : serie temporal
		# dias: numero de dias (ou meses) que tem o ciclo anual [para dados diarios dias =365, para dados mensais dias=12]
		# num : 1 
		# p   : é o periodo da serie p=,p*deltat. No caso p=N*1, onde N é o número de
		# dados da série

		# output
		# xd serie desazonalisada
		# f que e o cosseno ajustado
                            
                      N <- length(x)
                     mx <- mean(x)      # media de x
                      y <- x - mx       #(série-média)

                      # c = NUMERO DE CICLOS
			
			c <- N/(dias * num);# o primeiro numero apos a divisao e o numero de dias 
					    # que o ciclo  possui e o ultimo numero diz respeito
					    # se a serie possui mais de um dado dor dia, este
					    # varia conforme a serie. Ex: tenho uma serie de 9496 dados, cada
					    # dia e formado por 2 dados. Quero um ciclo de 365 dias. Faço 365*2

			# W E A FREQUENCIA (STUL,1988)
			w <- ((2*pi*c)/p) 
			t <- seq(0,N-1)

			# O CALCULO DO DELTA 
			delta0 <- (sum((cos(w * t))^2) * sum((sin(w * t))^2))
			delta <- delta0 - (sum((cos(w * t) * sin(w*t))^2))

			# O CALCULO DO a E b 
			a0 <- (sum(y * cos( w * t)) * sum((sin(w *t))^2))
			a1 <- a0-(sum(y * sin(w * t)) * sum(cos(w * t) * sin(w * t)))
			a  <- a1/delta

			b0 <- (sum(y * sin(w * t)) * sum((cos(w * t))^2))
			b1 <- b0-(sum(y * cos(w * t)) * sum(cos(w * t) * sin(w * t)))
			b  <- b1/delta

			r  <-sqrt((a^2)+(b^2))			

			if(a>0)         fi<-atan(-b/a)			
			if(a==0 & b>0)  fi<- -pi/2			   
			if(a==0 & b<0)  fi <- pi/2			
			if(a<0  & b>0)  fi<-atan(-b/a)-pi			
			if(a<0 & b<=0)  fi<-atan(-b/a)+pi
			
			f  <- r * cos((w * t) + fi)
			xd <- y-f  	
#			saida <- data.frame(sem.ciclo = xd, cos.ajust = f)	
			saida <- cbind (sem.ciclo = xd)	
			saida	
}			
 write("desas","")

#:::::::::::::::::::::::::::::::::::::::
# funcao para colocar no grafico pairs na parte superior a correlacao e significancia
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
    usr <- par("usr"); on.exit(par(usr)) 
    par(usr = c(0, 1, 0, 1)) 
    r <- (abs(cor(x, y)))^2 
    txt <- format(c(r, 0.123456789), digits=digits)[1] 
    txt <- paste(prefix, txt, sep="") 
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 

    test <- cor.test(x,y) 
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " ")) 

    text(0.5, 0.5, txt, cex = cex * r) 
    text(.8, .8, Signif, cex=cex, col=2) 
}
#:::::::::::::::::::::::::::::::::::::::
# funcao para colocar no grafico pairs uma reta de regressão linear
  panel.regressao <- function (x, y, col = par("col"), bg = NA, pch=20, 
                          cex = 0.7, col.reta = "red", cex.lab=2,...) 
                   { points(x, y, pch = pch, col = col, bg = bg, cex = cex)
                       ok <- is.finite(x) & is.finite(y)
                       if (any(ok)) 
                         abline(lm(y[ok] ~ x[ok]), col = col.reta,lwd=2,...)
                         segments(min(x[ok]), min(x[ok]), max(x[ok]), max(x[ok]),col = "gray",lwd=2,...)      
                         box(lwd=2)
                    }

# funcao para colocar no grafico pairs na parte superior os coeficientes da regressao linear
 panel.coefs <- function(x, y, digits=3, prefix="") {
                 usr <- par("usr"); on.exit(par(usr)) 
                  par(usr = c(0, 1, 0, 1)) 
                   reg <- lm(y ~ x)
                   tend <- reg$coefficients[[2]]
                   txt1 <- format(c(tend, 0.123456789), digits=digits)[1] 
                   txt1 <- paste(prefix, txt1, sep="") 
                   desl <- reg$coefficients[[1]]
                   txt2 <- format(c(desl, 0.123456789), digits=digits)[1] 
                   txt2 <- paste(prefix, txt2, sep="")         
                     r2 <- (abs(cor(x, y)))^2 
                    txt <- round(r2,4) 
                    txt <- paste(prefix, txt, sep="")    
                   cex <- 1.5 
                    text(0.5, 0.7, paste("coef.ang = ",txt1,sep=""), cex = cex ) 
                    text(0.5, 0.5, paste("coef.lin = ",txt2,sep=""), cex = cex )
                    text(0.5, 0.3, paste("coef.det = ",txt,sep=""), cex = cex )
}

############################################################################################################
# EM CONTRUÇAO
#:::::::::::::::::::::::::::::::::::::::
# funcao para remoção do ciclo anual por analise harmomica
#   desas.h <-  function(x) {
#                  n <- length(x)    #numero de linhas               
#                  if(odd(n)==TRUE) k <- (n-1)/2 else k <- n/2                  
#                  K <- seq(1,k)                  
# calculo da frequencia
#		   w <- (2 * pi * K)/n
# calculo do periodo
#		   peri<- (1/w) * 2 * pi
# cria-se uma matriz pra achar harmonico
# q queremos remover
# olha-se o periodo colocado na 3a col
# ate achar o periodo desejado (12 meses)
# ver q k (harmonico) corresponde (na primeira col
#                   per12 <- peri[which(round(peri) == 12)]
#                   mat<- cbind(K, w, peri)
# achamos k 48
# daqui pra frente lembrar de substituir 
# tudo onde tiver 48 pelo valor achado
# entao atribuo este valor a k
# k=48
# calculo do coef A
# for t=1:n
# a(t)=tmc(t)*cos((2*pi*k*t)/n)
# A48=(2/n)*sum(a)
# end
# calculo do coef B
# for t=1:n
# b(t)=tmc(t)*sin((2*pi*k*t)/n)
# B48=(2/n)*sum(b)
# end
# calculo da amplitude
# c48=sqrt((A48^2)+(B48^2))
# calculo da fase
# f48=atan(B48/A48)
# pi
# se a fase for < pi, f = f+pi
# se nao f = f - pi
# f48=f48+pi
# calculo da media da serie original
# m=mean(tmc)
# contruindo o harmonico
# ww=(2*pi*48)/n
# ww e a nova frequencia
# for t=1:n
# a(t)=ww*t
# end
# a=a'
# yt21 eh o harmonico q representa o ciclo anual
# yt21=m+c48*(cos(a-f48))

#plotando
#plot(tmc)
#hold
#plot(yt21,'r')
#figure
#s=tmc-yt21'
#plot(s)

############################################################################################################                                             
# pentadas <- seq(1,73,5)   # 1        6     11      16     21      26     31            
#           labelsx  <-  c("03jan","28jan","22fev","19mar","13abr","08mai","02jun",                         
#                          # 36      41       46       51    56      61     66       71
#                          "27jun","22jul","16ago","10set","05out","30out","24nov","19dez")
#Paleta de cores comum
paleta<- function(n){
  r <- c( 41,   0, 222, 255, 192)
  g <- c(  9, 192, 222, 255,   0)
  b <- c(147, 255, 222,  96,   0)
  nos <- as.integer(n/4)
  verm <- NULL
  verd <- NULL
  azul <- NULL
  verm <- c(verm,seq(r[1],r[2],length=nos),seq(r[2],r[3],length=nos),seq(r[3],r[4],length=n-3*nos),seq(r[4],r[5],length=nos))
  verd <- c(verd,seq(g[1],g[2],length=nos),seq(g[2],g[3],length=nos),seq(g[3],g[4],length=n-3*nos),seq(g[4],g[5],length=nos))
  azul <- c(azul,seq(b[1],b[2],length=nos),seq(b[2],b[3],length=nos),seq(b[3],b[4],length=n-3*nos),seq(b[4],b[5],length=nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores<- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("paleta","")

#Paleta de cores parecida com a do GrADS
pvelha <- function(n){
  r <- c(  0, 22,239,255,153)
  g <- c(  0, 82,205,128, 20)
  b <- c(112,147,137, 0, 20)
  nos <- as.integer(n/4)
  verm <- NULL
  verd <- NULL
  azul <- NULL
  verm <- c(verm,seq(r[1],r[2],length=nos),seq(r[2],r[3],length=nos),seq(r[3],r[4],length=n-3*nos),seq(r[4],r[5],length=nos))
  verd <- c(verd,seq(g[1],g[2],length=nos),seq(g[2],g[3],length=nos),seq(g[3],g[4],length=n-3*nos),seq(g[4],g[5],length=nos))
  azul <- c(azul,seq(b[1],b[2],length=nos),seq(b[2],b[3],length=nos),seq(b[3],b[4],length=n-3*nos),seq(b[4],b[5],length=nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores<- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("pvelha","")

#Gradiente de azuis
azulado <-function(n){
  r <- c(  0, 23, 46)
  g <- c(  0, 57,113)
  b <- c( 79,124,168)
  nos <- as.integer(n/2)
  verm<-c(seq(r[1],r[2],length=nos),seq(r[2],r[3],length=n-nos))
  verd<-c(seq(g[1],g[2],length=nos),seq(g[2],g[3],length=n-nos))
  azul<-c(seq(b[1],b[2],length=nos),seq(b[2],b[3],length=n-nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores <- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("azulado","")

#Gradiente de azuis
iazulado <-function(n){
  r <- c( 46, 23,  0)
  g <- c(113, 57,  0)
  b <- c(168,124, 79)
  nos <- as.integer(n/2)
  verm<-c(seq(r[1],r[2],length=nos),seq(r[2],r[3],length=n-nos))
  verd<-c(seq(g[1],g[2],length=nos),seq(g[2],g[3],length=n-nos))
  azul<-c(seq(b[1],b[2],length=nos),seq(b[2],b[3],length=n-nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores <- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("iazulado","")

#Paleta de cores parecida com o atlas do GrADS
atlas <- function(n){
  r <- c( 40, 50,244,130, 70)
  g <- c(109,184,224, 98, 62)
  b <- c( 33, 33,122,  5, 34)
  nos <- as.integer(n/4)
  verm <- NULL
  verd <- NULL
  azul <- NULL
  verm <- c(verm,seq(r[1],r[2],length=nos),seq(r[2],r[3],length=nos),seq(r[3],r[4],length=n-3*nos),seq(r[4],r[5],length=nos))
  verd <- c(verd,seq(g[1],g[2],length=nos),seq(g[2],g[3],length=nos),seq(g[3],g[4],length=n-3*nos),seq(g[4],g[5],length=nos))
  azul <- c(azul,seq(b[1],b[2],length=nos),seq(b[2],b[3],length=nos),seq(b[3],b[4],length=n-3*nos),seq(b[4],b[5],length=nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores<- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("atlas","")

#Paleta de cores invertida, parecida com a do GrADS
ipaleta <- function(n){
  r <- c(153,255,239, 22,  0)
  g <- c( 20,128,205, 82,  0)
  b <- c( 20,  0,137,147,112)
  nos <- as.integer(n/4)
  verm <- NULL
  verd <- NULL
  azul <- NULL
  verm <- c(verm,seq(r[1],r[2],length=nos),seq(r[2],r[3],length=nos),seq(r[3],r[4],length=n-3*nos),seq(r[4],r[5],length=nos))
  verd <- c(verd,seq(g[1],g[2],length=nos),seq(g[2],g[3],length=nos),seq(g[3],g[4],length=n-3*nos),seq(g[4],g[5],length=nos))
  azul <- c(azul,seq(b[1],b[2],length=nos),seq(b[2],b[3],length=nos),seq(b[3],b[4],length=n-3*nos),seq(b[4],b[5],length=nos))
  verm <- round(verm);verd <- round(verd);azul=round(azul)
  cores<- rgb(r=verm,g=verd,b=azul,maxColorValue=255)
  cores
}
write("ipaleta","")

# paletas
#e1071::hsv_palette                            Sequential color palette based on HSV colors
#fBasics::colorPalette                         Color Palettes
#fUtilities::colorPalette                      Color Palettes
#gplots::rich.colors                           Rich color palettes
#grDevices::gray.colors                        Gray Color Palette
#grDevices::rainbow                            Color Palettes

#Função baseada no filled.contour, mas que não acrescenta aqueles retângulos pretos
sombreado <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels),
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) -
        1), plot.title, plot.axes, key.title, key.axes, asp = NA,
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes,
    ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no `z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing x and y values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
        yaxs = "i")
    rect(0, border = col,levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes)
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title))
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
        stop("no proper `z' matrix specified")
    if (!is.double(z))
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),
      col = col))
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            axis(1)
            axis(2)
        }
    }
    else plot.axes
    if (frame.plot)
        box()
    if (missing(plot.title))
        title(...)
    else plot.title
    invisible()
} #end function sombreado
write("sombreado","")

#Função baseada no sombreado, mas que não plota o campo principal
solegenda <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels),
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) -
        1), plot.title, plot.axes, key.title, key.axes, asp = NA,
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes,
    ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no `z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing x and y values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
        yaxs = "i")
    rect(0, border = col,levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes)
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title))
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp, type="n")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
        stop("no proper `z' matrix specified")
    if (!is.double(z))
        storage.mode(z) <- "double"
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            axis(1)
            axis(2)
        }
    }
    else plot.axes
    if (frame.plot)
        box()
    if (missing(plot.title))
        title(...)
    else plot.title
    invisible()
} #end function sombreado
write("solegenda","")

variascores <- c("aquamarine","azure","beige","bisque","black",
                 "blanchedalmond","blue","blueviolet","brown",
                 "burlywood","cadetblue","chartreuse","chocolate",
                 "coral","cornflowerblue","cornsilk","cyan","darkblue",
                 "darkcyan","darkgoldenrod","darkgreen","darkkhaki",
                 "darkmagenta","darkolivegreen","darkorange","darkorchid",
                 "darkred","darksalmon","darkseagreen","darkslateblue",
                 "darkturquoise","darkviolet","deeppink","deepskyblue",
                 "dodgerblue","firebrick","forestgreen","gainsboro","gold",
                 "goldenrod","gray","green","greenyellow","grey","honeydew",
                 "hotpink","indianred","ivory","khaki","lavender",
                 "lavenderblush","lawngreen","lemonchiffon","lightblue",
                 "lightcoral","lightcyan","lightgoldenrod","lightgreen",
                 "lightpink","lightsalmon","lightseagreen","lightskyblue",
                 "lightslateblue","lightsteelblue","lightyellow","limegreen",
                 "linen","magenta","maroon","mediumaquamarine","mediumblue",
                 "mediumorchid","mediumpurple","mediumseagreen","mediumslateblue",
                 "mediumspringgreen","mediumturquoise","mediumvioletred",
                 "midnightblue","mintcream","mistyrose","moccasin","navy",
                 "navyblue","oldlace","olivedrab","orange","orangered","orchid",
                 "palegoldenrod","palegreen","paleturquoise","palevioletred",
                 "papayawhip","peachpuff","peru","pink","plum","powderblue",
                 "purple","red","rosybrown","royalblue","saddlebrown","salmon",
                 "sandybrown","seagreen","seashell","sienna","skyblue",
                 "slateblue","springgreen","steelblue","tan","thistle",
                 "tomato","turquoise","violet","violetred","wheat","yellow",
                 "yellowgreen")

cores <- c("midnightblue","slateblue4","steelblue","cornflowerblue","deepskyblue","skyblue","powderblue","azure2",
           "cornsilk2","khaki1","yellow","gold","orange","darkorange","coral1","tomato")

addtickmarks <- function(...,side,fator.tm) {
                         # fator.tm significa fator para tickmarks
                         # side=2 e um fator.tm=2, p.ex, duplica o numero de marcas default do R colocado no eixo y 
                         # axis(side, at = pretty(axTicks(side), n = fator.tm *length(axTicks(side))),
                         #        labels = rep("",(fator.tm * length(axTicks(side)) - 1) ) 
                         #)
                         
                          #res = resolução 
                          res <- axTicks(side)[2]-axTicks(side)[1]
#                          posicoes <- seq(axTicks(side)[1],
#                                         axTicks(side)[length(axTicks(side))],
#                                          by = res/fator.tm)
                         
                          posicoes <- seq(axTicks(side)[1]- 2*res/fator.tm,
                                         axTicks(side)[length(axTicks(side))] + 2*res/fator.tm,
                                          by = res/fator.tm)
                         
                          axis(side, at = posicoes, labels = rep("", length(posicoes)) ) 
}

