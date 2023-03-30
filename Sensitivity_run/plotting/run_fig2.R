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

source("plot_functions.R")
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

num_vars = 9 #pod, shoot, A_dmax, A_dmean,A_sum, WUE,TRANS,iWUE,LAI 

plot_list_heatmap = list()
for (ii in 1:length(CO2s)){
  CO2_case = CO2s[ii] 
  X1 = readRDS(paste0("../results_rds/results_CO2_",CO2_case,"_stage",stage,"_05_withLAI_maxpod_new.rds"))
  total_assim_all  = X1[[3]] #these are at specfic days
  podmass_all      = X1[[4]] #these are at specfic days
  shootmass_all    = X1[[5]] #these are at specfic days
  total_assim_dmax = X1[[8]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),3))
  wue    = X1[[11]]
  trans  = X1[[12]]
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

  method  = 1 #1: percentage %; 2: absolute difference; 3: no difference
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

  if(method==1){
    cbar_limit = c(-30,10) #for percentages
  }else{
    cbar_lists = list(c(5,8),c(6,10),c(6,10),c(4,6),c(20,40))
  }
  customize_colorbar = TRUE
  var2plot = c("Pod","Shoot","LAI","An_DailyMean","An_DailyMax")
  for (j in 1:length(var2plot)){
      t0 = Sys.time()
      varname = var2plot[j]
      var_diff = list_diff[[which(varnames_all==varname)]]
      if(method!=1){
        cbar_limit = cbar_lists[[j]] 
        if(j<=2){
           varname1= bquote(paste(.(varname)," (Mg/ha)",sep=""))
        }else if(j==3){
           varname1= bquote(.(varname)~(m^2/m^2))
        }else{
           varname1= bquote(.(varname)~(mu*mol~m^-2*s^-1))
        }
      }else{varname1=varname}

      var_diff_avg = apply(var_diff,c(1,2),mean) #10-year mean
      if(j>=4 & method==3){
       var_diff_avg = var_diff_avg*convert_rate_Assim
      }
      fig_i = plot_contour(v_scaler,j_scaler,var_diff_avg,varname1,cbar_limit,ii,customize_colorbar)
  #+Ading the max Gradient line
      xy_trace = gradient_desc(v_scaler,j_scaler,var_diff_avg,2) #1: descent; 2: ascent
      new_df   = as.data.frame(xy_trace)
      fig_i    <- fig_i+ geom_point(data = new_df,aes(x=V1,y=V2),inherit.aes = FALSE)
  #-Ading the max Gradient line
      fig_order = ii + (j-1) * length(CO2s) #arrange plot order by rows
      plot_list_heatmap[[fig_order]] = fig_i 
  }
}#end for CO2s loop!!!

#heatmap plot
pdf(paste0(output_folder,"Fig2.pdf"),height = 30, width=24)
grid.arrange(grobs = plot_list_heatmap,nrow=length(var2plot),ncol=length(CO2s))
dev.off()
