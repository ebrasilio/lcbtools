# dt <- cc
# calcular u*
#
#dt <- cc
eddy <- function(dt){
    
    dt1 <- dt[,which(names(dt) %in% c("Ux","Uy","Uz","Ts","CO2","H2O"))]
    dt2 <- apply(dt1, 2,function(x){return(mean(x,na.rm=T))})
    dt3 <- data.frame(apply(dt1, 2,function(x){return(x - mean(x,na.rm=T))}))
    
    H  <- mean(dt3[,"Uz"] * dt3[,"Ts"]) * 1231 #Cp_ar para ar seco
    FC <- mean(dt3[,"Uz"] * dt3[,"CO2"]) * 1000 / 44 #(umol/m2s)
    LE <- mean(dt3[,"Uz"] * dt3[,"H2O"]) * 2.5*10**6 * 10**-3
    ust <- sqrt(mean(dt3[,"Uz"]*dt3[,"Ux"])**2 + mean(dt3[,"Uz"]*dt3[,"Uy"])**2)
    
    out <- data.frame(H,LE,FC, ust)
    return(out)
}

#----------------------------------------------------------------------------
profile <- function(dt, z1, z2){
    library(openair)
    k <- 0.4
    
    dt$dT <- dt$Ta_2  - dt$Ta_1
    dt$dQ <- dt$H20_2 - dt$H20_1
    dt$dC <- dt$CO2_2 - dt$CO2_1
    dt$dM <- dt$ws2   - dt$ws1
    
    dt$K <- k**2 * z2**2 * abs(dt$dM / (z2 - z1))
    
    
    dt$H  <- -1 * dt$K * (dt$dT/(z2 - z1)) * 1231
    dt$Le <-  dt$K * (dt$dQ/(z2 - z1)) * 2.5*10**6 * 10**-3
    dt$FC <-  dt$K * (dt$dC/(z2 - z1)) * 1000 / 44 #(umol/m2s)
    return()
}

#----------------------------------------------------------------------------
# calc turbulent flux using bowen method


calc_bowen = function(mydt){
    # mydt = bowenIn
    # Unidades
    # Pressão de hPa para kPa
    mydt$Pa <- mydt$Pa / 10
    
    # Constantes
    Epson = 0.622 
    rho_d = 1.225       # densidade do ar seco (kg/m3)
    rhoCp = 1231        # (W/m2 / K.m.s-1)
    L  = 2.5*10^6      # J/Kg

    # converter umidade relativa em densidade:
    # Tetens
    mydt$es1 = 0.611 * exp(17.2694 * mydt$WXT520_1_Ta_Avg / (mydt$WXT520_1_Ta_Avg + 237.3)) # kPa
    mydt$ea1 = mydt$WXT520_1_Ua_Avg * mydt$es1 / 100  # kPa
    mydt$Q1  = mydt$ea1 / mydt$Pa * Epson * rho_d  #(Rv * (mydt$WXT520_1_Ta_Avg + 273.15))  # kg/m3
    
    mydt$es2 = 0.611 * exp(17.2694 * mydt$WXT520_2_Ta_Avg / (mydt$WXT520_2_Ta_Avg + 237.3)) # kPa
    mydt$ea2 = mydt$WXT520_2_Ua_Avg * mydt$es2 / 100  # kPa
    mydt$Q2  = mydt$ea2 / mydt$Pa * Epson * rho_d  #(Rv * (mydt$WXT520_2_Ta_Avg + 273.15))  # kg/m3
    
    # razão de Bowen
    mydt$B = (rhoCp * (mydt$WXT520_2_Ta_Avg - mydt$WXT520_1_Ta_Avg)) / (L * (mydt$Q2 - mydt$Q1))
    
    # Fluxos:
    # W/m2
    mydt$H  = (mydt$Net_Avg - mydt$G) / (1 + 1 / mydt$B)
    mydt$Le = mydt$H / mydt$B
    # mg m-2 s-1
    mydt$FC = (mydt$H * (mydt$Co2_1 - mydt$Co2_2)) / (rhoCp * (mydt$WXT520_2_Ta_Avg - mydt$WXT520_1_Ta_Avg))
    mydt$FC_umol = mydt$FC * 1000 / 44

    return(mydt)
}

