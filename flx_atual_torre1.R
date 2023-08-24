#
rm(list=ls())

#---- Inicial Setings ----
# libraries
library(dplyr, warn.conflicts = F)
library(stringr)


args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2 ) {
  stop("Input and Output directories are mandatory \n", call.=FALSE)
} else if (length(args)==2) {
  # default output file
  inpDir <- args[1] 
  outDir <- args[2]
}
cat(inpDir,'\n')
cat(outDir,'\n')
# inpDir = 'data/2021/21119'  
# outDir = 'outExt/Flux'
if(dir.exists(inpDir)){
  inpFile <- list.files(pattern = 'eddypro_EXT_', path = inpDir, recursive = T, full.names = T)
  cat('\n Reading: ', inpFile, '\n')
  inpf <- read.csv(inpFile, skip=4, na.strings = c('NA','-9999'),
                   stringsAsFactors = FALSE, header = FALSE)
  
  lab <- str_split_fixed(readLines(inpFile,2)[2],',',110)
  names(inpf) <- lab
  inpf$date <- as.POSIXct(paste(inpf$date, inpf$time), format="%Y-%m-%d %H:%M", tz = 'GMT')
  inpf <- inpf[,-c(1,3,4,5,6,7)]
  
  
  outFile <- list.files(pattern = 'Flx_Extrema_allvars_torre1', path=outDir, recursive = T, full.names = T) 
  cat('\n Reading: ', outFile, '\n')
  full <- read.csv(outFile, na.strings = c('NA','-9999','-9999.0'), stringsAsFactors = FALSE, header = T)
  full$date <- as.POSIXct(full$date, tz='GMT')
  names(full) <- c("date","Tau","qc_Tau","H","qc_H","LE","qc_LE","co2_flux","qc_co2_flux","h2o_flux","qc_h2o_flux",
                   "H_strg","LE_strg","co2_strg","h2o_strg","co2_v-adv","h2o_v-adv","co2_molar_density",
                   "co2_mole_fraction","co2_mixing_ratio","co2_time_lag","co2_def_timelag","h2o_molar_density",
                   "h2o_mole_fraction","h2o_mixing_ratio","h2o_time_lag","h2o_def_timelag","sonic_temperature",
                   "air_temperature","air_pressure","air_density","air_heat_capacity","air_molar_volume","ET",
                   "water_vapor_density","e","es","specific_humidity","RH","VPD","Tdew","u_unrot","v_unrot",
                   "w_unrot","u_rot","v_rot","w_rot","wind_speed","max_wind_speed","wind_dir","yaw","pitch","roll",
                   "u*","TKE","L","(z-d)/L","bowen_ratio","T*","model","x_peak","x_offset","x_10%","x_30%","x_50%",
                   "x_70%","x_90%","un_Tau","Tau_scf","un_H","H_scf","un_LE","LE_scf","un_co2_flux","co2_scf",
                   "un_h2o_flux","h2o_scf","spikes_hf","amplitude_resolution_hf","drop_out_hf",
                   "absolute_limits_hf","skewness_kurtosis_hf","skewness_kurtosis_sf",
                   "discontinuities_hf","discontinuities_sf","timelag_hf","timelag_sf","attack_angle_hf",
                   "non_steady_wind_hf","u_spikes","v_spikes","w_spikes","ts_spikes","co2_spikes","h2o_spikes",
                   "u_var","v_var","w_var","ts_var","co2_var","h2o_var","w/ts_cov","w/co2_cov","w/h2o_cov",
                   "chopper_LI.7500","detector_LI.7500","pll_LI.7500","sync_LI.7500","mean_value_RSSI_LI.7500",
                   "diag_75_mean")
  
  if(inpf$date[1] > full$date[nrow(full)]){
    cat(inpf$date[1], '\n')
    cat(full$date[nrow(full)], '\n')
    
    out <- merge(full, inpf, all=T)
    
    ref <- data.frame(date=seq.POSIXt(min(out$date), max(out$date), by='30 min'))
    outf <- merge(ref, out, all.x=T)
    
    
    write.csv(outf, file = outFile, row.names = F, na = '-9999.0')
    
    write.csv(outf[,which(names(outf) %in% c("date","H","LE","co2_flux","u*","wind_speed","wind_dir"))],
              "../outExt/Flux/EXT_Flx_Torre1_vars.csv", row.names = F, na = '-9999.0')
  }else 
    if( (inpf$date[1] < full$date[nrow(full)]) & (inpf$date[nrow(inpf)] > full$date[nrow(full)])    ) {
      cat('\n**** Old and New data ****\n')
      
  }else {
    
    
      
  
    cat('\n No new data \n')
  }
  
  
  
  
 # Se não existe entrada, não processa 
}else{
  cat('\n Input File doesnt exist \n')
}



  

