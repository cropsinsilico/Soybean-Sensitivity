library(gridExtra)
library(lattice)
library(reshape2)
library(ggplot2)
library(metR)
library(akima)
library(abind)
library(scales)

calc_difference<-function(x1,x0,thres,method){

    x0[abs(x0) < thres] = NaN #remove x0 that are too small to calculate percentages 
    if(method==1){#percentage
      y=(x1-x0)/abs(x0)*100
    }else if(method==2){#absolute value
      y=x1-x0
    }else if(method==3){#no difference!
      y=x1
    }else{stop("wrong difference method!")}

    return(y)
}

source("./plot_functions.R")
output_folder = "../results_figs/"
CO2s <- c(400,600,800,1000)
years <- 2006:2015
stage = 0
doys = c(180,220,260)
no_layers = 10
step = 0.05
v_scaler = seq(0.5,1.5,by=step) 
j_scaler = seq(0.5,1.5,by=step) 
doy_to_plot = 260
doy_ind = which(doys==doy_to_plot)
#index for the default Vcmax and Jmax
v0_ind = which(v_scaler==1)
j0_ind = which(j_scaler==1)

months_gs = 6:10
convert_rate_Assim = 1/(3600 * 1e-6 * 30 * 1e-6 * 10000) #Mg/ha/hr to umol/m2/s

barplot_output=c()
for (CO2 in CO2s){
   barplot_output = rbind(barplot_output,data.frame(year = years,CO2 = CO2))
} 
num_vars = 9 #pod, shoot, A_dmax, A_dmean,A_sum, WUE,TRANS,iWUE,LAI 
array_init = array(NaN,c(dim(barplot_output)[1],num_vars))
colnames(array_init)=c("Pod","Shoot","An_DailyMax","An_DailyMean",
                       "An_DailySum","WUE","Trans","iWUE","LAI")
barplot_output = cbind(barplot_output,array_init)
barplot_output1 = barplot_output
barplot_output2 = barplot_output

for (ii in 1:length(CO2s)){
  CO2_case = CO2s[ii] 
  X1 = readRDS(paste0("../results_rds/results_CO2_",CO2_case,"_stage",stage,"_05_withLAI_maxpod_new.rds"))
  X2 = readRDS(paste0("../results_rds/climate_metrics_CO2_",CO2_case,"_stage",stage,"_05.rds"))
  total_assim_all  = X1[[3]] #these are at specfic days
  podmass_all      = X1[[4]] #these are at specfic days
  shootmass_all    = X1[[5]] #these are at specfic days
  total_assim_dmax = X1[[8]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),3))
  wue   = X1[[11]]
  trans = X1[[12]]
  iwue   = X1[[13]]
  lai    = X1[[14]]
  #print(dim(lai))
  
  assim_diurnal    = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(months_gs),24,2))#X1[[9]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(months_gs),24,2))
  
  layer_assim_sunlit = X1[[1]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
  layer_assim_shaded = X1[[2]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
  Ci_sunlit = X1[[6]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
  Ci_shaded = X1[[7]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
  
  podmass     = podmass_all[,,,doy_ind]
  shootmass   = shootmass_all[,,,doy_ind]
  total_assim = total_assim_all[,,,doy_ind]  
  if(length(dim(podmass))<3){
      podmass = abind(podmass,podmass,along=3)
      shootmass = abind(shootmass,shootmass,along=3)
      total_assim = abind(total_assim,total_assim,along=3)
  }
  podmass_diff = podmass
  shootmass_diff = shootmass
  wue_diff = wue
  iwue_diff = wue
  lai_diff = wue
  trans_diff = trans 
  total_assim_diff = total_assim
  total_assim_dmax_diff = total_assim_dmax
  assim_diurnal_diff = assim_diurnal
  method  = 3 #1: %; 2: absolute difference; 3: no difference
  for (j in 1:length(j_scaler)){
  for (k in 1:length(v_scaler)){
     thres = 1e-5
     x0 = podmass[v0_ind,j0_ind,]
     podmass_diff[k,j,] =  calc_difference(podmass[k,j,],x0,thres,method) 
  
     x0 = shootmass[v0_ind,j0_ind,]
     shootmass_diff[k,j,] =  calc_difference(shootmass[k,j,],x0,thres,method) 
  
     x0 = total_assim[v0_ind,j0_ind,]
     total_assim_diff[k,j,] =  calc_difference(total_assim[k,j,],x0,thres,method) 
  
     x0 = total_assim_dmax[v0_ind,j0_ind,,]
     total_assim_dmax_diff[k,j,,] =  calc_difference(total_assim_dmax[k,j,,],x0,thres,method) 
    
     x0 = wue[v0_ind,j0_ind,]
     wue_diff[k,j,] =  calc_difference(wue[k,j,],x0,thres,method) 
  
     x0 = iwue[v0_ind,j0_ind,]
     iwue_diff[k,j,] =  calc_difference(iwue[k,j,],x0,thres,method) 
  
     x0 = lai[v0_ind,j0_ind,]
     lai_diff[k,j,] =  calc_difference(lai[k,j,],x0,thres,method) 
     #lai_diff[k,j,] = x0 
  
     x0 = trans[v0_ind,j0_ind,]
     trans_diff[k,j,] =  calc_difference(trans[k,j,],x0,thres,method) 
  
     x0 = assim_diurnal[v0_ind,j0_ind,,,,]
     thres = 1e-10
     assim_diurnal_diff[k,j,,,,] = calc_difference(assim_diurnal[k,j,,,,],x0,thres,method)
  }
  }
  
  list_diff = list(podmass_diff,shootmass_diff,total_assim_diff,total_assim_dmax_diff[,,,1],total_assim_dmax_diff[,,,2],total_assim_dmax_diff[,,,3],wue_diff,trans_diff,iwue_diff,lai_diff)
  #varnames_all = c("POD","SHOOT","ASSIM","A_DMAX","A_DMEAN","A_SUM","WUE","TRANS","LAI")
  varnames_all = c("Pod","Shoot","Assim","An_DailyMax","An_DailyMean",
                         "An_DailySum","WUE","Trans","iWUE","LAI")
  
  layer_assim_sunlit_diff = layer_assim_sunlit
  layer_assim_shaded_diff = layer_assim_shaded
  Ci_sunlit_diff = Ci_sunlit
  Ci_shaded_diff = Ci_shaded
  thres = 0.1
  for (l in 1:no_layers){
  for (j in 1:length(j_scaler)){
  for (k in 1:length(v_scaler)){
     x0 = layer_assim_sunlit[v0_ind,j0_ind,,l]
     layer_assim_sunlit_diff[k,j,,l] = calc_difference(layer_assim_sunlit[k,j,,l],x0,thres,method) 
     x0 = layer_assim_shaded[v0_ind,j0_ind,,l]
     layer_assim_shaded_diff[k,j,,l] = calc_difference(layer_assim_shaded[k,j,,l],x0,thres,method) 
     x0 = Ci_sunlit[v0_ind,j0_ind,,l]
     Ci_sunlit_diff[k,j,,l] = calc_difference(Ci_sunlit[k,j,,l],x0,thres,method) 
     x0 = Ci_shaded[v0_ind,j0_ind,,l]
     Ci_shaded_diff[k,j,,l] = calc_difference(Ci_shaded[k,j,,l],x0,thres,method)
  }
  }
  }
  
  v_use = 1.0
  j_use = 1.0
  v_index = which(abs(v_scaler-v_use)<1e-10)
  j_index = which(abs(j_scaler-j_use)<1e-10)
  if(method==2 | method==3){
    total_assim_dmax_diff= total_assim_dmax_diff*convert_rate_Assim
  }
  tmp0 = cbind(podmass_diff[v_index,j_index,],shootmass_diff[v_index,j_index,],total_assim_dmax_diff[v_index,j_index,,],wue_diff[v_index,j_index,],trans_diff[v_index,j_index,],iwue_diff[v_index,j_index,],lai_diff[v_index,j_index,])
  barplot_output[barplot_output$CO2 == CO2_case,-c(1,2)] = tmp0 
  barplot_output1[barplot_output1$CO2 == CO2_case,-c(1,2)] = tmp0 
  v_use = 1.2
  j_use = 1.2
  v_index = which(abs(v_scaler-v_use)<1e-10)
  j_index = which(abs(j_scaler-j_use)<1e-10)
  tmp0 = cbind(podmass_diff[v_index,j_index,],shootmass_diff[v_index,j_index,],total_assim_dmax_diff[v_index,j_index,,],wue_diff[v_index,j_index,],trans_diff[v_index,j_index,],iwue_diff[v_index,j_index,],lai_diff[v_index,j_index,])
  barplot_output[barplot_output$CO2 == CO2_case,-c(1,2)] = tmp0 
  barplot_output2[barplot_output2$CO2 == CO2_case,-c(1,2)] = tmp0 

}#end for CO2s loop!!!

#1:years; 2: CO2s
#3:pod; 4:shoot; 5:An_max; 6:An_dmean; 7:An_sum; 8:wue; 9: trans; 10: iwue; 11:lai_max
barplot_output_sub = barplot_output[,c(1,2,
                                   3,4,5,
                                   6,11,8)] #choose SIX to plot!
barplot_output_sub1 = barplot_output1[,c(1,2,
                                   3,4,5,
                                   6,11,8)] #choose SIX to plot!
barplot_output_sub2 = barplot_output2[,c(1,2,
                                   3,4,5,
                                   6,11,8)] #choose SIX to plot!

#these require method to be 3!!!!! because the difference will be calculated in the plot functions
if(method !=3) stop("timeseries_plot needs method to be 3!")
pdfname = paste0(output_folder,"Fig4.pdf")
timeseries_plot(barplot_output_sub1,barplot_output_sub2,pdfname,3)
