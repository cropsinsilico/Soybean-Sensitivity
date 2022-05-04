library(gridExtra)
library(lattice)
library(reshape2)
library(ggplot2) # To ggplot functions
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
output_folder = "figs_outputs/"
CO2s <- c(400,600,800,1000)
years <- 2006:2015#c('2002','2004','2005','2006')
stage = 0
doys = c(180,220,260)
no_layers = 10
step = 0.05
v_scaler = seq(0.5,1.5,by=step) 
j_scaler = seq(0.5,1.5,by=step) 
doy_to_plot = 260
doy_ind = which(doys==doy_to_plot)
v0_ind = which(v_scaler==1)
j0_ind = which(j_scaler==1)
#print(c(v0_ind,j0_ind))
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

plot_list_heatmap = list()
for (ii in 1:length(CO2s)){
CO2_case = CO2s[ii] 
X1 = readRDS(paste0("../results_rds/results_CO2_",CO2_case,"_stage",stage,"_05_withLAI_maxpod.rds"))
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
#print(c(min(Ci_sunlit),max(Ci_sunlit)))
#print(c(min(Ci_shaded),max(Ci_shaded)))
#print(c(min(Ci_shaded[,,,1]),max(Ci_shaded[,,,1])))
#print(c(min(Ci_shaded[,,,10]),max(Ci_shaded[,,,10])))
#print(c(min(layer_assim_shaded[,,2,2]),max(layer_assim_shaded[,,2,2])))
#print(layer_assim_shaded[,,2,2])
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
method  = 3
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
   #wue_diff[k,j,] = (wue[k,j,] - x0)/x0*100 
   wue_diff[k,j,] =  calc_difference(wue[k,j,],x0,thres,method) 

   x0 = iwue[v0_ind,j0_ind,]
   #iwue_diff[k,j,] = (iwue[k,j,] - x0)/x0*100 
   iwue_diff[k,j,] =  calc_difference(iwue[k,j,],x0,thres,method) 

   x0 = lai[v0_ind,j0_ind,]
   #iwue_diff[k,j,] = (iwue[k,j,] - x0)/x0*100 
   lai_diff[k,j,] =  calc_difference(lai[k,j,],x0,thres,method) 
   #lai_diff[k,j,] = x0 

   x0 = trans[v0_ind,j0_ind,]
   trans_diff[k,j,] =  calc_difference(trans[k,j,],x0,thres,method) 

   x0 = assim_diurnal[v0_ind,j0_ind,,,,]
   thres = 1e-10
   assim_diurnal_diff[k,j,,,,] = calc_difference(assim_diurnal[k,j,,,,],x0,thres,method)
}
}
#print(assim_diurnal[6,6,1,2,,1])
#print(assim_diurnal[8,8,1,2,,1])
#print(assim_diurnal_diff[8,8,1,2,,1])
#print(trans[1,1,])
#print(trans[2,2,])
#if(CO2_case==400 | CO2_case==1000){
#print(paste("CO2 is",CO2_case))
#print(lai[11,11,])
#print(lai[15,15,])
#print(lai[15,15,]-lai[11,11,])
##print(mean(lai[15,15,]-lai[11,11,]))
#}
#print(total_assim_dmax[6,6,,])
#print(total_assim_dmax[8,8,,])
#print(total_assim_dmax_diff[8,8,,])
#print(c(min(total_assim_diff),max(total_assim_diff)))
#stop()
#list_diff = list(podmass_diff,shootmass_diff,total_assim_diff,total_assim_dmax_diff)
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
#print(layer_assim_shaded[6,6,2,])
#print(layer_assim_shaded[,,2,2])
#print(layer_assim_shaded[,,1,5])
#print(layer_assim_shaded[,,2,5])
#print(c(min(Ci_sunlit_diff),max(Ci_sunlit_diff)))
#print(c(min(Ci_shaded_diff),max(Ci_shaded_diff)))
#print(c(min(layer_assim_sunlit_diff),max(layer_assim_sunlit_diff)))
#print(c(min(layer_assim_shaded_diff),max(layer_assim_shaded_diff)))
#print(c(min(layer_assim_sunlit),max(layer_assim_sunlit)))
#print(c(min(total_assim_dmax_diff),max(total_assim_dmax_diff)))
#print(layer_assim_shaded[6,6,3,])
#print(layer_assim_shaded[11,11,3,])
#print(which(layer_assim_shaded<(-100),arr.ind=TRUE))
#print(which(layer_assim_sunlit<(-100),arr.ind=TRUE))
#stop()
if(FALSE){
  cbar_limit = c(-30,10)
  #cbar_limit = c(0,5)
  var2plot = c("Pod","Shoot","LAI","An_DailyMean","An_DailyMax")
  for (j in 1:length(var2plot)){
         t0 = Sys.time()
         varname = var2plot[j]
         var_diff = list_diff[[which(varnames_all==varname)]]
         print(varname)
         print(range(var_diff))
 #        if(j==3) cbar_limit = c(-40,40)
 #        if(j==4) cbar_limit = c(-20,20)
 # 	for (i in 1:length(years)){
 # 	    year_i = years[i]
         var_diff_avg = apply(var_diff,c(1,2),mean) #10-year mean
	 pos_ind = which(var_diff_avg>0,arr.ind=TRUE)
	 vj = cbind(v_scaler[pos_ind[,1]],j_scaler[pos_ind[,2]]) - 1
         vj_multi = vj[,1] * vj[,2]
         vj_plus  = vj[,1] + vj[,2]
#         print(vj[vj_multi<0|vj_plus<0,]) #find the Gains but with decreased Vmax or Jmax 
  	 fig_i = plot_contour(v_scaler,j_scaler,var_diff_avg,varname,cbar_limit,ii)
  #+Adding the max gradient line
         xy_trace = gradient_desc(v_scaler,j_scaler,var_diff_avg,2) #1: descent; 2: ascent
         new_df = as.data.frame(xy_trace)
         fig_i <- fig_i+ geom_point(data = new_df,aes(x=V1,y=V2),inherit.aes = FALSE)
  #-Adding the max gradient line
         fig_order = ii + (j-1) * length(CO2s) #arrange plot order by rows
  	 plot_list_heatmap[[fig_order]] = fig_i 
 # 	}
         t1 = Sys.time()
         print(t1-t0)
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
#write.csv(csv_output,paste("diff_",CO2_case,".csv",sep=""))

#if(ii==1) z_gd = total_assim_dmax_diff[,,1,2] 

#diurnal plot
#pdfname = paste0("assim_diurnal_",CO2_case,".pdf")
#v_use = 1.2
#j_use = 1.2
#plot_diurnal(assim_diurnal_diff,years,months_gs,pdfname,v_use,j_use,v_scaler,j_scaler)

}#end for CO2s loop!!!

##heatmap plot
#pdf(paste0(output_folder,"Fig_heatmap_new_withLAI_podmax.pdf"),height = 30, width=24)
#grid.arrange(grobs = plot_list_heatmap,nrow=length(var2plot),ncol=length(CO2s))
#dev.off()
#print("finished heatmap plot")
#stop()

#bar plot
#barplot_output = barplot_output[barplot_output$year==2002,-6] #only plot 2002 and remove dmean
#saveRDS(barplot_output,"barplot_output.rds")

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

#print(colMeans(barplot_output_sub[barplot_output_sub$CO2==400,]))
#print(sd(barplot_output_sub[barplot_output_sub$CO2==1000,"iWUE"]))
pdfname = paste0(output_folder,"barplot_stage",stage,"_lai.pdf")
#1:diff-%; 2:diff-absolute; 3: raw values of CTL or EXP; 
var_type = method 
#barplot(barplot_output_sub,pdfname,var_type)

pdfname = paste0(output_folder,"barplot_stage",stage,"_CTL_EXP.pdf")
var_type = 3 
#barplot_two(barplot_output_sub1,barplot_output_sub2,pdfname,var_type)

pdfname = paste0(output_folder,"scatterplot_stage",stage,"_CTL_EXP.pdf")
#scatter_plot(barplot_output_sub1,barplot_output_sub2,pdfname)

#these require method to be 3!!!!! because the difference will be calculated in the plot functions
if(method !=3) stop("timeseries_plot needs method to be 3!")
pdfname = paste0(output_folder,"timeseries_stage",stage,"_CTL_EXP.pdf")
timeseries_plot(barplot_output_sub1,barplot_output_sub2,pdfname,1)

pdfname = paste0(output_folder,"timeseries_diff_stage",stage,"_CTL_EXP.pdf")
timeseries_plot(barplot_output_sub1,barplot_output_sub2,pdfname,2)

pdfname = paste0(output_folder,"timeseries_diff_percent_stage",stage,"_CTL_EXP.pdf")
timeseries_plot(barplot_output_sub1,barplot_output_sub2,pdfname,3)
print("finished barplot")
stop()

#correlation plot
#sow_day     =   150 
#harvest_day =   280
#temp = c()
#pr   = c()
#for (i in 1:length(years)) {
#  yr <- years[i]
#  weather <- read.csv(file = paste0('../WeatherData/',yr,'/', yr, '_Bondville_IL_daylength.csv'))
#  sd.ind <- which(weather$doy == sow_day)[1]      # start of sowing day
#  hd.ind <- which(weather$doy == harvest_day)[24] # end of harvest day
#  weather.growingseason <- weather[sd.ind:hd.ind,]
#  temp = c(temp,mean(weather.growingseason$temp))
#  pr   = c(pr,sum(weather.growingseason$precip))
##  print(c(mean(weather.growingseason$temp),sum(weather.growingseason$precip),mean(weather.growingseason$solar),mean(weather.growingseason$rh)))
#}
climates = X2#X1[[13]] 
#varnames = c("Pod","Shoot","An_max","An_dmean","An_sum","WUE")
varnames = c("Pod","Shoot","Trans","An_dmean","An_sum","WUE")
df = barplot_output#[,c("year","CO2",varname)]
CO2_level = 400 
df = df[df$CO2==CO2_level,]
df = cbind(df,climates)
cname = colnames(df) 
colnames(df) = c(cname[1:(length(cname)-8)],"temp","solar","rh","precip","ws","ttc","dvi","dvirate")

weather_vars = c("temp","solar","rh","precip","ws","ttc","dvi","dvirate")
#for (weather_var in weather_vars){
#    pdfname = paste0("corr_plot_",weather_var,CO2_level,"s",stage,".pdf")
#    corr_plot(df,pdfname,varnames,weather_var)
#}
saveRDS(df,paste0("barplot_output_with_weather_",CO2_level,".rds"))


#gradient descent
#cbar_limit = c(-30,10) 
#var_name = "A_DMEAN" 
#xy_trace = gradient_desc(v_scaler,j_scaler,z_gd,2)
#new_df = as.data.frame(xy_trace)
##saveRDS(xy_trace,"xy_trace.rds")
#p = plot_contour(v_scaler,j_scaler,z_gd,var_name,cbar_limit)
#p <- p + geom_point(data = new_df,aes(x=V1,y=V2),inherit.aes = FALSE)
#pdf(paste("Fig_CO2_",CO2_case,"_gd.pdf",sep=""),height = 12, width=12)
#print(p)
##grid.arrange(grobs = plot_list,nrow=length(var2plot),ncol=length(years))
#dev.off()


stop()

#Look at the diagonal or horizontal,averaged all years
pod_diag = c()
shoot_diag = c()
Admax_diag = c()
for (i in 1:length(v_scaler)){
   tmp = mean(podmass_diff[i,6,],na.rm=TRUE)
   pod_diag = c(pod_diag,tmp) 
   tmp = mean(shootmass_diff[i,6,],na.rm=TRUE)
   shoot_diag = c(shoot_diag,tmp) 
   tmp = mean(total_assim_dmax_diff[i,6,],na.rm=TRUE)
   Admax_diag= c(Admax_diag,tmp) 
}
csv_output = cbind(pod_diag,shoot_diag,Admax_diag)
write.csv(csv_output,paste("vmax_",CO2_case,".csv",sep=""))

########################## plot layers' assimilation 

#sunlit assim, mean of daily max, layered
cbar_limit = c(-20,20)
pdfname = paste("Fig_",CO2_case,"_layers_sunlit.pdf",sep="")
var2plot = layer_assim_sunlit_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)

#shaded assim, mean of daily max, layered
cbar_limit = c(-20,20)
pdfname = paste("Fig_",CO2_case,"_layers_shaded.pdf",sep="")
var2plot = layer_assim_shaded_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)

########################## plot layers' Ci
#cbar_limit = c(300,450)
#cbar_limit = c(300,330)
cbar_limit = c(-30,30)
pdfname = paste("Fig_",CO2_case,"_Ci_sunlit_diff.pdf",sep="")
var2plot = Ci_sunlit_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)
cbar_limit = c(-10,2)
pdfname = paste("Fig_",CO2_case,"_Ci_shaded_diff.pdf",sep="")
var2plot = Ci_shaded_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)


#vertical profile

v_use = c(1,1.2)
j_use = c(1,1.2)
var2plot1 = Ci_shaded_diff  
var2plot2 = Ci_sunlit_diff #shaded first, then sunlit 
pdfname = paste("Fig_",CO2_case,"Ci_profile_sunlit.pdf",sep="")
#profile_plot1(var2plot1,var2plot2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler) #include both sunlit and shaded
#profile_plot2(var2plot2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler) #looking at sunlit or shaded once at a time

# below is to plot biomass time series, This requires the raw results output from BioCro
stop()

calc_diff<-function(res1,res2,varname,method){
    x1 = res1[,varname]
    x2 = res2[,varname]
    xdiff = x2-x1
    xdiff_percentage = (x2-x1)/x1*100
    if(method=="last"){
       return(xdiff_percentage[length(xdiff)])
    }else if(method=="max"){
       return(max(xdiff_percentage))
    }else if(method=="mean"){
       return(mean(xdiff_percentage))
    }else if(method=="min"){
       return(min(xdiff_percentage))
    }
}

difference = array(NaN,c(4,length(years)))
plot_list = list()
varname = "Grain"
method  = "last"
for (i in 1:length(years)){
    year_i = years[i]
    Fig_AmbCO2 <- plot_all_tissues(results[[i]],results[[i+length(years)]], year_i, ExpBiomass[[1]], ExpBiomass.std[[1]],"AmbCO2")
    Fig_EleCO2 <- plot_all_tissues(results.elevCO2[[i]],results.elevCO2[[i+length(years)]], year_i, ExpBiomass[[1]], ExpBiomass.std[[1]],"EleCO2")
    tmp1 = calc_diff(results[[i]],results[[i+length(years)]],varname,method)
    tmp2 = calc_diff(results.elevCO2[[i]],results.elevCO2[[i+length(years)]],varname,method)
    difference[,i] = c(tmp1,tmp2) 
    plot_list[[i]] = Fig_AmbCO2 
    plot_list[[length(years)+i]] = Fig_EleCO2 
}
write.csv(difference,"diff.csv")
pdf("Fig_timeseries_sensitivity.pdf",width=16)
grid.arrange(grobs = plot_list,nrow=2,ncol=length(years))
dev.off()
