library(BioCroSoyBean)
# Clear workspace
rm(list=ls())

source("calc_GDD.R")
# Set working directory to location of this file
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

# turn off warnings
options(warn=-1)

# if needed, create ./figs/ directory
path.figs <- './figs/'
dir.create(path = path.figs, showWarnings = FALSE)

#years <- c('2002','2004','2005','2006')
years = 2006:2015
sow_day     =   150 
harvest_day =   280

# sowing and harvest DOYs for each growing season
dates <- data.frame("year" = years,"sow" = sow_day, "harvest" = harvest_day)

# initialize variables
# results <- list()
# results.elevCO2 <- list()
weather.growingseason <- list()

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
CO2s <- c(400,600,800,1000)
solar_threshold = 10 #to get the daytime
no_layers = 10
doys = c(180,220,260) #specfic doy to investigate
#varying vmax & jmax from -50% to +50%
varying_step = 0.1
v_scaler   = seq(0.5,1.5,by=varying_step) 
j_scaler   = seq(0.5,1.5,by=varying_step) 
tpu_scaler = seq(0.5,1.5,by=varying_step) 
convert_rate_Assim = 1/(3600 * 1e-6 * 30 * 1e-6 * 10000) #Mg/ha/hr to umol/m2/s
convert_rate_Trans = 1/(3600 * 1e-3 * 18 * 1e-6 * 10000) #Mg/ha/hr to mmol/m2/s 
convert_gs         = 1/1000 #mmol to mol 

soybean_para0 = soybean_parameters
soybean_para  = soybean_parameters
print(c("default tpu rate is ",soybean_para$tpu_rate_max))
#init outputs
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

climate_metrics    = array(NaN,c(length(years),3)) #mean temp, TTC, total Precip

for (CO2 in CO2s){
t0 = Sys.time()
for (i in 1:length(years)) {
  yr <- years[i]
  print(paste("processing year",yr)) 

#  weather <- read.csv(file = paste0('../Data/Weather_data/', yr, '_Bondville_IL_daylength.csv'))
  weather <- read.csv(file = paste0('./WeatherData/',yr,'/', yr, '_Bondville_IL_daylength.csv'))

#  weather$temp = weather$temp + 2  #mannually change temperature 

#  sowdate <- dates$sow[which(dates$year == yr)]
#  harvestdate <- dates$harvest[which(dates$year == yr)]
#  sd.ind <- which(weather$doy == sowdate)[1] # start of sowing day
#  hd.ind <- which(weather$doy == harvestdate)[24] # end of harvest day
  sd.ind <- which(weather$doy == sow_day)[1]      # start of sowing day
  hd.ind <- which(weather$doy == harvest_day)[24] # end of harvest day
  
  weather.growingseason[[i]] <- weather[sd.ind:hd.ind,]
  solar = weather.growingseason[[i]]$solar 
  daytime_ind = which(solar>solar_threshold) #look at daytime only

  for (j in 1:length(j_scaler)){
  for (k in 1:length(v_scaler)){
  	soybean_para$jmax =  soybean_para0$jmax   *  j_scaler[j] 
  	soybean_para$vmax1 = soybean_para0$vmax1  *  v_scaler[k]
#        soybean_para$tpu_rate_max = soybean_para0$tpu_rate_max * tpu_scaler[k]
  	soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_para, weather.growingseason[[i]],
                                       soybean_steadystate_modules, soybean_derivative_modules,
                                       arg_names, soybean_solver_params)
        results         <- soybean_solver(CO2)
        canopy_assim = results[,"canopy_assimilation_rate"]
        canopy_trans = results[,"canopy_transpiration_rate"]
        canopy_gs    = results[,"canopy_conductance"]
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
        DVI_sub_condition = which(DVI < 1)
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
#        for (dd in 1:length(doys)){
#             doy_ind = which(DOY==doys[dd])
             dd = 3  #put it in the last to be consistent with the plotting
             doy_ind = which(DOY==tail(doy_unique,1)) #the last day
             grain = results[,"Grain"]
             podmass[k,j,i,dd] = max(grain[doy_ind],na.rm=TRUE)
             aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
             shootmass[k,j,i,dd] = max(aboveground_mass[doy_ind],na.rm=TRUE) 
             total_assim[k,j,i,dd] = max(canopy_assim[doy_ind],na.rm=TRUE)
#        }
if(yr==2006 & k==6 &j==6) saveRDS(results,"results_2006_CTL.rds")
if(yr==2006 & k==8 &j==8) saveRDS(results,"results_2006_SEN.rds")

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
  #      print(which(cname=="sunlit_Assim_layer_0"))
  #      print(which(cname=="shaded_Assim_layer_0"))
  #      saveRDS(results,"results_example.rds")
        rm(soybean_solver)  #make sure the solver is properly cleaned
  } 
  } 
  temp   = weather.growingseason[[i]]$temp
  precip = weather.growingseason[[i]]$precip
  climate_metrics[i,1] = mean(temp[DVI_sub_condition],na.rm=TRUE) 
  climate_metrics[i,2] = gdd_calc(DVI[DVI_sub_condition],temp[DVI_sub_condition])  
  climate_metrics[i,3] = sum(precip[DVI_sub_condition],na.rm=TRUE) 
} #end years

t1 = Sys.time()
print(t1-t0)
#save the output for fast plotting
X1 = list(layer_assim_sunlit,layer_assim_shaded,total_assim,podmass,shootmass,
          Ci_sunlit,Ci_shaded,total_assim_dmax,minA_freq_sunlit,minA_freq_shaded,wue,trans,climate_metrics)
saveRDS(X1,file=paste("results_rds/results_CO2_",CO2,"_Jmax_and_Vmax_stage1.rds",sep=""))
#saveRDS(X1,file=paste("results_rds/results_CO2_",CO2,"_Jmax_and_TPU.rds",sep=""))
#saveRDS(X1,file=paste("results_rds/results_CO2_",CO2,"_2C.rds",sep=""))
} #end CO2s

