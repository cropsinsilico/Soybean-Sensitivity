library(BioCroSoyBean)
# Clear workspace
rm(list=ls())

source("calc_GDD.R")

# turn off warnings
options(warn=-1)

weather_data_path = '../../Weather_Data/' #slash is needed at the end 
output_path       = '../results_rds/'

years = 2006:2015
#I used an averaged sowing and harvest days for all years.
sow_day     =   150  
harvest_day =   280

# sowing and harvest DOYs for each growing season
dates <- data.frame("year" = years,"sow" = sow_day, "harvest" = harvest_day)

# initialize variables
weather.growingseason <- list()

no_layers = 10

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
CO2s <- c(400,600,800,1000)

solar_threshold = 10 #to get the daytime

stage = 0 #0: whole growing season; 1: veg stage; 2: rep stage
 
#remove days with very small LAI
lai_threshold = 0.5
doys = c(180,220,260) #specfic doy to investigate

#varying vmax & jmax from -50% to +50%,by a 5% step
varying_step = 0.05
v_scaler   = seq(0.5,1.5,by=varying_step) 
j_scaler   = seq(0.5,1.5,by=varying_step) 
convert_rate_Assim = 1/(3600 * 1e-6 * 30 * 1e-6 * 10000) #Mg/ha/hr to umol/m2/s
convert_rate_Trans = 1/(3600 * 1e-3 * 18 * 1e-6 * 10000) #Mg/ha/hr to mmol/m2/s 
convert_gs         = 1/1000 #mmol to mol 

soybean_para0 = soybean_parameters
soybean_para  = soybean_parameters

#init output matrix
podmass     = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
shootmass   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
total_assim = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    

wue                = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #umol CO2/ mmol H2O 
trans              = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #mmol H2O 
total_assim_dmax   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),3)) #mean of daily daytime max,mean & accumulate 
layer_assim_sunlit = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_assim_shaded = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
Ci_sunlit          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean 
Ci_shaded          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
minA_freq_sunlit   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers,3)) #3 to save FREQs of Ac, Aj & Ap
minA_freq_shaded   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers,3)) 

climate_metrics    = array(NaN,c(length(years),6)) #mean temp, TTC, total Precip

#loop through CO2 scenarios
for (CO2 in CO2s){
t0 = Sys.time()
#loop through years 
  for (i in 1:length(years)) {
    yr <- years[i]
    print(paste("processing year",yr)) 
  
    weather <- read.csv(file = paste0(weather_data_path,yr, '_Bondville_IL_daylength.csv'))
  
    sd.ind <- which(weather$doy == sow_day)[1]      # start of sowing day
    hd.ind <- which(weather$doy == harvest_day)[24] # end of harvest day
    
    weather.growingseason[[i]] <- weather[sd.ind:hd.ind,]
    solar = weather.growingseason[[i]]$solar 
    daytime_ind = which(solar>solar_threshold) #look at daytime only
  
    for (j in 1:length(j_scaler)){
    for (k in 1:length(v_scaler)){
    	soybean_para$jmax =  soybean_para0$jmax   *  j_scaler[j] 
    	soybean_para$vmax1 = soybean_para0$vmax1  *  v_scaler[k]
    	soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_para, weather.growingseason[[i]],
                                         soybean_steadystate_modules, soybean_derivative_modules,
                                         arg_names, soybean_solver_params)
          results         <- soybean_solver(CO2)
          canopy_assim = results[,"canopy_assimilation_rate"]
          canopy_trans = results[,"canopy_transpiration_rate"]
          canopy_gs    = results[,"canopy_conductance"]
          lai_hourly   = results[,"lai"]
          cname = colnames(results)
          sunlit_index = grep("^sunlit_Assim.*", cname) #matching all sunlit assim 
          shaded_index = grep("^shaded_Assim.*", cname) 
          if(length(sunlit_index) != no_layers || length(shaded_index) !=no_layers){
             stop("results issues!?")
          }
  #        layer_assim_sunlit[k,j,i,] = apply(results[,sunlit_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
  #        layer_assim_shaded[k,j,i,] = apply(results[,shaded_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
          assim_sunlit = results[,sunlit_index]
          assim_shaded = results[,shaded_index]
  
          sunlit_index_Ci = grep("^sunlit_Ci_layer.*", cname)
          shaded_index_Ci = grep("^shaded_Ci_layer.*", cname)
          if(length(sunlit_index_Ci) != no_layers || length(shaded_index_Ci) !=no_layers){
             stop("results issues!?")
          }
          tmp = results[,sunlit_index_Ci]
          if(dim(tmp)[1] != length(solar)) stop("what?")
          tmp = tmp[daytime_ind,] 
          Ci_sunlit[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer
          tmp = results[,shaded_index_Ci]
          tmp = tmp[daytime_ind,] 
          Ci_shaded[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer
  
          DVI = results[,"DVI"]
          DOY = results[,"doy"]
  #subset the DOYs by DVI stages
          if(stage == 1){
            DVI_sub_condition = which(DVI < 1)
          }else if(stage == 2){
            DVI_sub_condition = which(DVI >= 1)
          }else if(stage == 0){ #all of them!
            DVI_sub_condition = which(DVI < 100)
          }else{stop("wrong stage value!")}
          DOY_sub = DOY[DVI_sub_condition] 
          doy_unique = unique(DOY_sub)
          doy_unique = doy_unique[!is.na(doy_unique)]
  #for assimilation, we calculate the MEAN of DAILY MAX! 
  #use a simple loop for now. Using matrix operation can speed this up
          canopy_assim_dmax = c()
          canopy_assim_dmean = c()
          canopy_gs_dmean = c()
          canopy_trans_dmean = c()
          assim_sunlit_dmax = c()
          assim_shaded_dmax = c()
          if(length(solar)!=length(canopy_assim)) stop("bug2")
          for (ss in 1:length(doy_unique)){
              d_ss = doy_unique[ss]
              doy_ind = which(DOY==d_ss)
              if(length(doy_ind)<2) next  # sometimes we get an output of only 1-hour!?
              canopy_assim_dmax =  c(canopy_assim_dmax,max(canopy_assim[doy_ind],na.rm=TRUE))
              solar_dayi = solar[doy_ind]
              canopy_assim_dayi = canopy_assim[doy_ind]
              canopy_gs_dayi    = canopy_gs[doy_ind] 
              canopy_trans_dayi = canopy_trans[doy_ind] 
              canopy_assim_dmean = c(canopy_assim_dmean,mean(canopy_assim_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              canopy_gs_dmean    = c(canopy_gs_dmean,mean(canopy_gs_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              canopy_trans_dmean = c(canopy_trans_dmean,mean(canopy_trans_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
  
              tmp = apply(assim_sunlit[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              if(length(tmp) != no_layers) stop("bug1")
              assim_sunlit_dmax = cbind(assim_sunlit_dmax,tmp)
              tmp = apply(assim_shaded[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              assim_shaded_dmax = cbind(assim_shaded_dmax,tmp)
          }
  #remove days when LAI are too small
  	doy_min = DOY[which(lai_hourly>lai_threshold)][1]
  	doy_max = tail(DOY[which(lai_hourly>lai_threshold)],1)
  	canopy_assim_dmax = canopy_assim_dmax[doy_unique>doy_min & doy_unique<doy_max]
  	canopy_assim_dmean= canopy_assim_dmean[doy_unique>doy_min & doy_unique<doy_max]
  	canopy_trans_dmean= canopy_trans_dmean[doy_unique>doy_min & doy_unique<doy_max]
  	canopy_assim      = canopy_assim[DOY > doy_min & DOY<doy_max] #this is hourly data!

  	if( (abs(j_scaler[j]-1) < 1e-4) & (abs(v_scaler[k]-1) < 1e-4) ){ #save only the CTL's lai_sub for subsetting climates
  		print(c(soybean_para$jmax,soybean_para$vmax1))
  		lai_sub = which(lai_hourly>lai_threshold)
  	}
  #daily averaging
          total_assim_dmax[k,j,i,1] = mean(canopy_assim_dmax,na.rm=TRUE)
          total_assim_dmax[k,j,i,2] = mean(canopy_assim_dmean,na.rm=TRUE)
          total_assim_dmax[k,j,i,3] = sum(canopy_assim,na.rm=TRUE) #sum includes night-time!!!!
  
  #calculate WUE (mean of daily DAYTIME-MEAN)
          top = canopy_assim_dmean * convert_rate_Assim
          #bot = canopy_gs_dmean * convert_gs   #using gs 
          bot = canopy_trans_dmean * convert_rate_Trans #using transpiration
          wue[k,j,i]   = mean(top/bot,na.rm=TRUE) 
          trans[k,j,i] = mean(bot,na.rm=TRUE) 
  
          layer_assim_sunlit[k,j,i,] = rowMeans(assim_sunlit_dmax,na.rm=TRUE)
          layer_assim_shaded[k,j,i,] = rowMeans(assim_shaded_dmax,na.rm=TRUE)
          if(min(layer_assim_shaded,na.rm=TRUE) < (-100)){
             print(dim(assim_shaded_dmax))
             print(assim_shaded_dmax[1:5,1:10])
             print(which(layer_assim_shaded < (-100),arr.ind=TRUE))
             print(layer_assim_shaded[k,j,i,])
             saveRDS(results,"results_debug.rds")
             stop()
          }
  
          if(any(is.na(layer_assim_shaded[k,j,i,]))){
                print(c(k,j,i))
                saveRDS(results,"results_nan.rds")
                print(layer_assim_shaded[k,j,i,])
                print(c(soybean_para0$jmax, soybean_para0$vmax1))
                print(c(soybean_para$jmax, soybean_para$vmax1))
                print(c(j_scaler[j], v_scaler[k]))
                stop()
          }
  #for biomass and canoy_assim we save the values on those specfic doys
        dd = 3  #put it in the last to be consistent with the plotting
        doy_ind = which(DOY==tail(doy_unique,1)) #the last day
        grain = results[,"Grain"]
        podmass[k,j,i,dd] = max(grain[doy_ind],na.rm=TRUE)
        aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
        shootmass[k,j,i,dd] = max(aboveground_mass[doy_ind],na.rm=TRUE) 
        total_assim[k,j,i,dd] = max(canopy_assim[doy_ind],na.rm=TRUE)
  
  #save the minimal index of As
  	sunlit_min_index = grep("^sunlit_min_index.*", cname) #matching all sunlit assim 
  	shaded_min_index = grep("^shaded_min_index.*", cname)
  	y_sunlit = results[,sunlit_min_index]
  	y_shaded = results[,shaded_min_index]
  	table_sunlit = c()
  	table_shaded = c()
  	for (ii in 1:10){
  	  table_sunlit = rbind(table_sunlit,table(factor(y_sunlit[,ii],levels = 1:3)))
  	  table_shaded = rbind(table_shaded,table(factor(y_shaded[,ii],levels = 1:3)))
  	}
  	minA_freq_sunlit[k,j,i,,] = as.matrix(table_sunlit) 
  	minA_freq_shaded[k,j,i,,] = as.matrix(table_shaded) 
        rm(soybean_solver)  #make sure the solver is properly cleaned
    } 
    } 
    temp   = weather.growingseason[[i]]$temp
    precip = weather.growingseason[[i]]$precip
    rh     = weather.growingseason[[i]]$rh
    ws     = weather.growingseason[[i]]$ws
    subset_condition = intersect(lai_sub,DVI_sub_condition) 
#    print(c("subset length is",length(subset_condition)))
    climate_metrics[i,1] = mean(temp[subset_condition],na.rm=TRUE) 
    climate_metrics[i,2] = mean(solar[subset_condition],na.rm=TRUE) 
    climate_metrics[i,3] = mean(rh[subset_condition],na.rm=TRUE) 
    climate_metrics[i,4] = sum(precip[subset_condition],na.rm=TRUE) 
    climate_metrics[i,5] = mean(ws[subset_condition],na.rm=TRUE) 
    climate_metrics[i,6] = gdd_calc(DVI[subset_condition],temp[subset_condition])  
  } #end years
  
  t1 = Sys.time()
  print(t1-t0)
  #save the output for fast plotting
  X_output = list(layer_assim_sunlit,layer_assim_shaded,total_assim,podmass,shootmass,
            Ci_sunlit,Ci_shaded,total_assim_dmax,minA_freq_sunlit,minA_freq_shaded,wue,trans,climate_metrics)
  saveRDS(X_output,file=paste0(output_path,"results_CO2_",CO2,"_stage",stage,"_05.rds"))
} #end CO2s
