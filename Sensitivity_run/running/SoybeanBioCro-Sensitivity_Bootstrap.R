library(BioCroSoyBean)
# Clear workspace
rm(list=ls())
hourly2daily<-function(data_hourly,method,daytime_ind){
	if(length(data_hourly) %% 24 !=0) stop("length not right!")
	nighttime_ind = setdiff(1:length(data_hourly),daytime_ind)
	data_hourly[nighttime_ind] = NA #Remove night-time hours
	x = matrix(data_hourly,nrow=24)
	if(method == "mean"){
        	data_daily = colMeans(x,na.rm=TRUE)
	}else if(method=="sum"){
        	data_daily = colSums(x,na.rm=TRUE)
	}else if(method=="max"){
        	data_daily = apply(x,2,max,na.rm=TRUE)
	}else{
		stop("no such method exists!")
	}
	return(data_daily)
}
source("calc_GDD.R")

# turn off warnings
options(warn=-1)

output_path       = '../results_bootstrap/'

#This weather file is a bit large and I don't want to put it on Github
#You can simply run the boostrap script in the Weather_Data folder to generate this data
weather_all <- readRDS("../../Weather_Data/bootstrap_weather_withSeed_s1000.rds")
years = 1:length(weather_all) #number of bootstrap samples
sow_day     =   150 
harvest_day =   280

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
CO2s <- c(400,600,800,1000)
solar_threshold         = -1 #to get the whole day 
solar_threshold_daytime = 10 #to get the daytime
no_layers = 10
stage = 0 #0:all season 
lai_threshold = 0.5 #days with lai less than this are removed
doys = c(180,220,260) #specfic doy to investigate
#varying vmax & jmax by +20%
v_scaler   = c(1,1.2)#
j_scaler   = c(1,1.2)#
convert_rate_Assim = 1/(3600 * 1e-6 * 30 * 1e-6 * 10000) #Mg/ha/hr to umol/m2/s
convert_rate_Trans = 1/(3600 * 1e-3 * 18 * 1e-6 * 10000) #Mg/ha/hr to mmol/m2/s 
convert_gs         = 1/1000 #mmol to mol 

soybean_parameters$ScaleFactor=1.0

soybean_para0 = soybean_parameters
soybean_para  = soybean_parameters

#initialize output arrays
podmass     = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
shootmass   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
#total_assim = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    

wue            = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #umol CO2/ mmol H2O 
iwue           = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #umol CO2/ mmol H2O 
trans          = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #mmol H2O 
total_assim_season = array(NaN,c(length(v_scaler),length(j_scaler),length(years),3)) #mean of daily daytime max,mean & accumulate 
layer_assim_sunlit = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_assim_shaded = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_assim_sunlit_max = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_assim_shaded_max = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_Q_sunlit    = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_Q_shaded    = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_Qmax_sunlit = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_Qmax_shaded = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_CanA_weighted = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
#layer_Ci_sunlit    = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean 
#layer_Ci_shaded    = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
#minA_freq_sunlit   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers,3)) #3 is to save FREQs of Ac, Aj & Ap
#minA_freq_shaded   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers,3)) 
max_daily_series      = sow_day:harvest_day
climate_metrics       = array(NaN,c(length(years),8)) #radi,rh, temp, TTC, Precip,ws,dvi, dvi/hour
climate_metrics_daily = array(NaN,c(length(max_daily_series),length(years),11)) #Actual daily data may be shorter than this, but never be longer than this
podmass_daily     = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years)))
shootmass_daily   = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years)))
wue_daily         = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years))) #umol CO2/ mmol H2O 
iwue_daily        = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years))) #umol CO2/ mmol H2O 
trans_daily       = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years))) #mmol H2O 
assim_daily       = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years),3)) #mean of daily daytime max,mean & accumulate 
StomataWS_daily   = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years))) 
LAI_daily         = array(NaN,c(length(v_scaler),length(j_scaler),length(max_daily_series),length(years))) 
Ci_sunlit_CTL_daily = array(NaN,c(no_layers,length(max_daily_series),length(years))) #Actual daily data may be shorter than this, but never be longer than this
Ci_shaded_CTL_daily = array(NaN,c(no_layers,length(max_daily_series),length(years))) #Actual daily data may be shorter than this, but never be longer than this
Ci_sunlit_SEN_daily = array(NaN,c(no_layers,length(max_daily_series),length(years))) #Actual daily data may be shorter than this, but never be longer than this
Ci_shaded_SEN_daily = array(NaN,c(no_layers,length(max_daily_series),length(years))) #Actual daily data may be shorter than this, but never be longer than this

for (CO2 in CO2s){
  t0 = Sys.time()
  for (i in 1:length(weather_all)) {
    #print(paste("processing year",yr)) 
  
    weather = weather_all[[i]]
    sd.ind <- which(weather$doy == sow_day)[1]      # start of sowing day
    hd.ind <- which(weather$doy == harvest_day)[24] # end of harvest day
    
    weather.growingseason <- weather[sd.ind:hd.ind,]
    solar = weather.growingseason$solar 
    doy_gs = weather.growingseason$doy
    daytime_ind = which(solar>solar_threshold) #look at daytime or all day only
    #double check when solar_threshold is negative, the daytime_ind should do Nothing!
    if(solar_threshold<0 & length(daytime_ind) != length(solar)) stop("sth is wrong!") 
    temp   = weather.growingseason$temp
    precip = weather.growingseason$precip
    rh     = weather.growingseason$rh
    ws     = weather.growingseason$windspeed
  
    for (j in 1:length(j_scaler)){
    for (k in 1:length(v_scaler)){
    	soybean_para$jmax =  soybean_para0$jmax   *  j_scaler[j] 
    	soybean_para$vmax1 = soybean_para0$vmax1  *  v_scaler[k]
    	soybean_para$scale_factor_Ac= 1.0 
  #        soybean_para$tpu_rate_max = soybean_para0$tpu_rate_max * tpu_scaler[k]
    	soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_para, weather.growingseason,
                                         soybean_steadystate_modules, soybean_derivative_modules,
                                         arg_names, soybean_solver_params)
          results         <- soybean_solver(CO2)
  #pull out some outputs from BioCro
          canopy_assim = results[,"canopy_assimilation_rate"]
          canopy_trans = results[,"canopy_transpiration_rate"]
          canopy_gs    = results[,"canopy_conductance"]
          StomataWS    = results[,"StomataWS"]
          lai_hourly   = results[,"lai"]
          cname = colnames(results)
          sunlit_index = grep("^sunlit_Assim.*", cname) #matching all sunlit assim 
          shaded_index = grep("^shaded_Assim.*", cname) 
          if(length(sunlit_index) != no_layers || length(shaded_index) !=no_layers){
             stop("results issues!?")
          }
          assim_sunlit = results[,sunlit_index]
          assim_shaded = results[,shaded_index]
  
          sunlit_index = grep("^sunlit_incident_par.*", cname) #matching all sunlit assim 
          shaded_index = grep("^shaded_incident_par.*", cname) 
          Q_sunlit = results[,sunlit_index]
          Q_shaded = results[,shaded_index]
          sunlit_index = grep("^sunlit_fraction.*", cname) #matching all sunlit assim 
          shaded_index = grep("^shaded_fraction.*", cname) 
          fraction_sunlit = results[,sunlit_index]
          fraction_shaded = results[,shaded_index]
          An_layer_sunlit = assim_sunlit * fraction_sunlit 
          An_layer_shaded = assim_shaded * fraction_shaded
          An_layer_weighted = array(NaN,dim(An_layer_sunlit))
          for (ii in 1:10){
           An_layer_weighted[,ii] = (An_layer_sunlit[,ii] + An_layer_shaded[,ii])*lai_hourly/10 
          }
  
          sunlit_index_Ci = grep("^sunlit_Ci_layer.*", cname)
          shaded_index_Ci = grep("^shaded_Ci_layer.*", cname)
          Ci_sunlit = results[,sunlit_index_Ci]
          Ci_shaded = results[,shaded_index_Ci]
  
          kgrain = results[,"kGrain"]
          kleaf  = results[,"kLeaf"]
          grain = results[,"Grain"]
          aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
          DVI 	 = results[,"DVI"]
          DVI_rate = results[,"development_rate_per_hour"]
          DOY 	 = results[,"doy"]
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
  #calculate DAILY values! 
  #use a simple loop for now. Using matrix operation can speed this up
          canopy_assim_dmax = c()
          canopy_assim_dmean = c()
          canopy_gs_dmean = c()
          canopy_trans_dmean = c()
          StomataWS_dmean   = c()
          LAI_dmean         = c()
          assim_sunlit_dmean = c()
          assim_shaded_dmean = c()
          assim_sunlit_dmax = c()
          assim_shaded_dmax = c()
          Q_sunlit_dsum = c()
          Q_shaded_dsum = c()
          Q_sunlit_dmax = c()
          Q_shaded_dmax = c()
          Ci_sunlit_dmean = c()
          Ci_shaded_dmean = c()
    	  An_weighted_dmax = c()
          fraction_sunlit_dmean = c()
          fraction_shaded_dmean = c()
  	  pod_eachday       = c()
          shoot_eachday     =c()
          stomaWS_eachday     =c()
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
              StomataWS_dayi    = StomataWS[doy_ind] 
              LAI_dayi          = lai_hourly[doy_ind] 
  	      pod_dayi          = grain[doy_ind]
  	      shoot_dayi        = aboveground_mass[doy_ind]
              canopy_assim_dmean = c(canopy_assim_dmean,mean(canopy_assim_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              canopy_gs_dmean    = c(canopy_gs_dmean,mean(canopy_gs_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              canopy_trans_dmean = c(canopy_trans_dmean,mean(canopy_trans_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              StomataWS_dmean = c(StomataWS_dmean,mean(StomataWS_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
              LAI_dmean = c(LAI_dmean,mean(LAI_dayi[solar_dayi>solar_threshold],na.rm=TRUE)) #daytime mean
  	      pod_eachday = c(pod_eachday,tail(pod_dayi,1)) # get the last one of that day for biomass
  	      shoot_eachday = c(shoot_eachday,tail(shoot_dayi,1)) # get the last one of that day for biomass
  ###by layer###
              tmp = apply(assim_sunlit[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              if(length(tmp) != no_layers) stop("bug1")
              assim_sunlit_dmax = cbind(assim_sunlit_dmax,tmp)
              tmp = apply(assim_shaded[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              assim_shaded_dmax = cbind(assim_shaded_dmax,tmp)
              tmp = apply(assim_sunlit[doy_ind,],2,mean,na.rm=TRUE) #get the daily mean of each layer
              assim_sunlit_dmean = cbind(assim_sunlit_dmean,tmp)
              tmp = apply(assim_shaded[doy_ind,],2,mean,na.rm=TRUE) #get the daily mean of each layer
              assim_shaded_dmean = cbind(assim_shaded_dmean,tmp)
              tmp = apply(Q_sunlit[doy_ind,],2,sum,na.rm=TRUE) #get the daily sum of each layer
              Q_sunlit_dsum = cbind(Q_sunlit_dsum,tmp)
              tmp = apply(Q_shaded[doy_ind,],2,sum,na.rm=TRUE) #get the daily sum of each layer
              Q_shaded_dsum = cbind(Q_shaded_dsum,tmp)
              tmp = apply(Q_sunlit[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              Q_sunlit_dmax = cbind(Q_sunlit_dmax,tmp)
              tmp = apply(Q_shaded[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              Q_shaded_dmax = cbind(Q_shaded_dmax,tmp)
  
              tmp = apply(An_layer_weighted[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
              An_weighted_dmax = cbind(An_weighted_dmax,tmp)
  ###by layer###
              tmp = Ci_sunlit[doy_ind,] 
              tmp = apply(tmp[solar_dayi>solar_threshold_daytime,],2,mean,na.rm=TRUE) #get the daytime mean of each layer
              Ci_sunlit_dmean = cbind(Ci_sunlit_dmean,tmp)
              tmp = Ci_shaded[doy_ind,] 
              tmp = apply(tmp[solar_dayi>solar_threshold_daytime,],2,mean,na.rm=TRUE) #get the daytime mean of each layer
              Ci_shaded_dmean = cbind(Ci_shaded_dmean,tmp)
          }
        pod_eachday_whole_growingseason = pod_eachday
  #remove days when LAI are too small
  	doy_min = DOY[which(lai_hourly>lai_threshold)][1]
  	doy_max = tail(DOY[which(lai_hourly>lai_threshold)],1)
  	lai_bounding_index = which(doy_unique>doy_min & doy_unique<doy_max)
  	canopy_assim_dmax = canopy_assim_dmax[lai_bounding_index]
  	canopy_assim_dmean= canopy_assim_dmean[lai_bounding_index]
  	canopy_trans_dmean= canopy_trans_dmean[lai_bounding_index]
  	canopy_gs_dmean   = canopy_gs_dmean[lai_bounding_index]
  	StomataWS_dmean   = StomataWS_dmean[lai_bounding_index]
  	LAI_dmean         = LAI_dmean[lai_bounding_index]
  	pod_eachday       = pod_eachday[lai_bounding_index]
  	shoot_eachday     = shoot_eachday[lai_bounding_index]
  	canopy_assim_sub  = canopy_assim[DOY > doy_unique[lai_bounding_index[1]] & DOY< tail(doy_unique[lai_bounding_index],1)] #this is hourly data!
  
        assim_sunlit_dmean = assim_sunlit_dmean[,lai_bounding_index] 
        assim_shaded_dmean = assim_shaded_dmean[,lai_bounding_index] 
        assim_sunlit_dmax  = assim_sunlit_dmax[,lai_bounding_index] 
        assim_shaded_dmax  = assim_shaded_dmax[,lai_bounding_index] 
        Q_sunlit_dsum      = Q_sunlit_dsum[,lai_bounding_index] 
        Q_shaded_dsum      = Q_shaded_dsum[,lai_bounding_index] 
        Q_sunlit_dmax      = Q_sunlit_dmax[,lai_bounding_index] 
        Q_shaded_dmax      = Q_shaded_dmax[,lai_bounding_index] 
        An_weighted_dmax   = An_weighted_dmax[,lai_bounding_index] 
        Ci_sunlit_dmean = Ci_sunlit_dmean[,lai_bounding_index] 
        Ci_shaded_dmean = Ci_shaded_dmean[,lai_bounding_index] 
  	if( (abs(j_scaler[j]-1) < 1e-4) & (abs(v_scaler[k]-1) < 1e-4) ){ #save only the CTL's lai_sub for subsetting climates
  		print("HI this is CTL")
  		print(c(soybean_para$jmax,soybean_para$vmax1))
  		lai_sub = which(lai_hourly>lai_threshold)
  	}
  ###now after hourly->daily calculation, we do seasonal operations of daily->seasonal
  #daily averaging
        total_assim_season[k,j,i,1] = mean(canopy_assim_dmax,na.rm=TRUE)
        total_assim_season[k,j,i,2] = mean(canopy_assim_dmean,na.rm=TRUE)
        total_assim_season[k,j,i,3] = sum(canopy_assim_sub,na.rm=TRUE) #sum includes night-time!!!!
  
  #calculate WUE (mean of daily DAYTIME-MEAN)
        top = canopy_assim_dmean * convert_rate_Assim
        bot_trans = canopy_trans_dmean * convert_rate_Trans #using transpiration
        wue[k,j,i]   = mean(top/bot_trans,na.rm=TRUE) 
        trans[k,j,i] = mean(bot_trans,na.rm=TRUE) 
  
        bot_gs = canopy_gs_dmean * convert_gs   #using gs 
        iwue[k,j,i]   = mean(top/bot_gs,na.rm=TRUE) 
  
  #save by layer, seasonal mean of daily- means or maxs
        layer_assim_sunlit[k,j,i,]     = rowMeans(assim_sunlit_dmean,na.rm=TRUE)
        layer_assim_shaded[k,j,i,]     = rowMeans(assim_shaded_dmean,na.rm=TRUE)
        layer_assim_sunlit_max[k,j,i,] = rowMeans(assim_sunlit_dmax,na.rm=TRUE)
        layer_assim_shaded_max[k,j,i,] = rowMeans(assim_shaded_dmax,na.rm=TRUE)
        layer_Q_sunlit[k,j,i,]         = rowMeans(Q_sunlit_dsum,na.rm=TRUE)
        layer_Q_shaded[k,j,i,]         = rowMeans(Q_shaded_dsum,na.rm=TRUE)
        layer_Qmax_sunlit[k,j,i,]         = rowMeans(Q_sunlit_dmax,na.rm=TRUE)
        layer_Qmax_shaded[k,j,i,]         = rowMeans(Q_shaded_dmax,na.rm=TRUE)
        layer_CanA_weighted[k,j,i,]       = rowMeans(An_weighted_dmax,na.rm=TRUE)
  
        dd = 3  #put it in the last to be consistent with the plotting
        doy_ind = which(DOY==tail(doy_unique,1)) #the last day
        grain = results[,"Grain"]
        podmass[k,j,i,dd] = max(grain[doy_ind],na.rm=TRUE) #take the max of that day!
        aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
        shootmass[k,j,i,dd] = max(aboveground_mass[doy_ind],na.rm=TRUE) 
  #save the daily data
  	if(length(canopy_assim_sub) %% 24 != 0) canopy_assim_sub= canopy_assim_sub[1:(length(canopy_assim_sub)-1)] 
  	canopy_assim_sum  = matrix(canopy_assim_sub,nrow=24)
  	canopy_assim_sum  = colSums(canopy_assim_sum,na.rm=TRUE)
  #Somehow I still need the begin and end DOY of the daily mean data (after the small LAI cut out)
  #Because the output arrays have the length of the whole growing season
  #Should be able to be simplified.
  	doy_begin = which(max_daily_series == doy_unique[lai_bounding_index[1]])
  	doy_end   = which(max_daily_series == tail(doy_unique[lai_bounding_index],1)) 
  	assim_daily[k,j,doy_begin:doy_end,i,] = cbind(canopy_assim_dmax,canopy_assim_dmean,canopy_assim_sum)
        wue_daily[k,j,doy_begin:doy_end,i]    =  top/bot_trans
        iwue_daily[k,j,doy_begin:doy_end,i]   =  top/bot_gs
        trans_daily[k,j,doy_begin:doy_end,i]  =  bot_trans
        StomataWS_daily[k,j,doy_begin:doy_end,i]   = StomataWS_dmean 
        LAI_daily[k,j,doy_begin:doy_end,i]         = LAI_dmean 
  #      podmass_daily[k,j,doy_begin:doy_end,i]     = pod_eachday
        podmass_daily[k,j,,i]     = pod_eachday_whole_growingseason 
        shootmass_daily[k,j,doy_begin:doy_end,i]   = shoot_eachday
  
        rm(soybean_solver)  #make sure the solver is properly cleaned
  
        if(j_scaler[j]==1 & v_scaler[k]==1){
           Ci_sunlit_CTL_daily[,lai_bounding_index,i] = Ci_sunlit_dmean  
           Ci_shaded_CTL_daily[,lai_bounding_index,i] = Ci_shaded_dmean 
        }else if(abs(j_scaler[j]-1.2)<1e-4 & v_scaler[k]==1){
           Ci_sunlit_SEN_daily[,lai_bounding_index,i] = Ci_sunlit_dmean  
           Ci_shaded_SEN_daily[,lai_bounding_index,i] = Ci_shaded_dmean 
        }
    } #end j
    } #end k
  #save some climate data
    subset_condition = intersect(lai_sub,DVI_sub_condition) 
    print(c("subset length is",length(subset_condition)))
    climate_metrics[i,1] = mean(temp[subset_condition],na.rm=TRUE) 
    climate_metrics[i,2] = mean(solar[subset_condition],na.rm=TRUE) 
    climate_metrics[i,3] = mean(rh[subset_condition],na.rm=TRUE) 
    climate_metrics[i,4] = sum(precip[subset_condition],na.rm=TRUE) 
    climate_metrics[i,5] = mean(ws[subset_condition],na.rm=TRUE) 
    climate_metrics[i,6] = gdd_calc(DVI[subset_condition],temp[subset_condition])  
    climate_metrics[i,7] = mean(DVI[subset_condition],na.rm=TRUE) 
    climate_metrics[i,8] = mean(DVI_rate[subset_condition],na.rm=TRUE) 
  
  #save the daily climate time series 
    climate_metrics_daily[,i,1] =   hourly2daily(temp,"mean",daytime_ind) 
    climate_metrics_daily[,i,2] =   hourly2daily(solar,"mean",daytime_ind) 
    climate_metrics_daily[,i,3] =   hourly2daily(rh,"mean",daytime_ind) 
    climate_metrics_daily[,i,4] =   hourly2daily(precip,"sum",daytime_ind) 
    climate_metrics_daily[,i,5] =   hourly2daily(ws,"mean",daytime_ind) 
    gdd_perday = c()
  #calculate GDD and save it
    doy_unique_all = unique(DOY)
    doy_unique_all = doy_unique_all[!is.na(doy_unique_all)]
    for (ss in 1:length(doy_unique_all)){
         d_ss = doy_unique_all[ss]
         doy_ind = which(DOY==d_ss)
   	 tmp = gdd_calc(DVI[doy_ind],temp[doy_ind])
  	 gdd_perday = c(gdd_perday,tmp)
    }
    climate_metrics_daily[,i,6] =   gdd_perday 
    climate_metrics_daily[,i,7] =   hourly2daily(DVI,"mean",daytime_ind) 
    climate_metrics_daily[,i,8] =   hourly2daily(DVI_rate,"mean",daytime_ind) 
    climate_metrics_daily[,i,9] =   hourly2daily(kleaf,"mean",daytime_ind) 
    climate_metrics_daily[,i,10] =   hourly2daily(kgrain,"mean",daytime_ind) 
    climate_metrics_daily[,i,11] =   hourly2daily(solar,"max",daytime_ind) 
  } #end years
  
  t1 = Sys.time()
  print(t1-t0)
  ##save the output for fast plotting
  #X1 = list(layer_assim_sunlit,layer_assim_shaded,layer_assim_sunlit_max,layer_assim_shaded_max,layer_Q_sunlit,layer_Q_shaded,layer_Qmax_sunlit,layer_Qmax_shaded,layer_CanA_weighted)
  #saveRDS(X1,             file=paste0(output_path,"results_layers_CO2_",CO2,"_stage",stage,"_05_bootstrap_set1000.rds"))
  #X1 = list(podmass,shootmass,total_assim_season,wue,trans)
  #saveRDS(X1,             file=paste0(output_path,"results_CO2_",CO2,"_stage",stage,"_05_bootstrap_finalday_newBSset1000.rds"))
  #saveRDS(climate_metrics,file=paste0(output_path,"climate_metrics_CO2_",CO2,"_stage",stage,"_05_bootstrap.rds"))
  #save Daily Data
  X1 = list(podmass_daily,shootmass_daily,assim_daily,wue_daily,trans_daily,StomataWS_daily,LAI_daily,iwue_daily)
  saveRDS(X1,             file=paste0(output_path,"results_CO2_",CO2,"_stage",stage,"_05_bootstrap_daily_StomaWS_allday_wholegrowingseason_newBSset1000.rds"))
} #end CO2s
