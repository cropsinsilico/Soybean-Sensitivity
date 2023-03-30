library(gridExtra)
library(lattice)
library(reshape2)
library(ggplot2) # To ggplot functions
library(metR)
library(akima)
library(abind)

calc_difference<-function(x1,x0,thres){

    x0[abs(x0) < thres] = NaN #remove x0 that are too small to calculate percentages 

    y=(x1-x0)/abs(x0)*100

    return(y)
}

source("plot_functions.R")
CO2s <- 400
stage = 0
doys = c(180,220,260)
no_layers = 10
step = 0.05
v_scaler = c(1,1.2) 
j_scaler = c(1,1.2) 
doy_to_plot = 260
doy_ind = which(doys==doy_to_plot)
v0_ind = which(v_scaler==1)
j0_ind = which(j_scaler==1)
months_gs = 6:10
par_energy_content = 0.235

CO2_case = CO2s
#This is 100 samples' results
#X1 = readRDS(paste("../results_rds/results_layers_CO2_",CO2_case,"_stage",stage,"_05_bootstrap.rds",sep=""))
X1 = readRDS(paste("../results_bootstrap/results_layers_CO2_",CO2_case,"_stage",stage,"_05_bootstrap_set1000.rds",sep=""))
layer_assim_sunlit        = X1[[1]] #
layer_assim_shaded        = X1[[2]] #
layer_assim_sunlit_max    = X1[[3]] #
layer_assim_shaded_max    = X1[[4]]
layer_Q_sunlit            = X1[[5]]
layer_Q_shaded            = X1[[6]]
layer_Qmax_sunlit         = X1[[7]]
layer_Qmax_shaded         = X1[[8]]
layer_An_weighted         = X1[[9]]
ctl_assim_sunlit = layer_assim_sunlit[1,1,,]
sen_assim_sunlit = layer_assim_sunlit[2,2,,]
ctl_assim_shaded = layer_assim_shaded[1,1,,]
sen_assim_shaded = layer_assim_shaded[2,2,,]
ctl_assim_sunlit_max = layer_assim_sunlit_max[1,1,,]
sen_assim_sunlit_max = layer_assim_sunlit_max[2,2,,]
ctl_assim_shaded_max = layer_assim_shaded_max[1,1,,]
sen_assim_shaded_max = layer_assim_shaded_max[2,2,,]
ctl_An_weighted      = layer_An_weighted[1,1,,]
sen_An_weighted      = layer_An_weighted[2,2,,]
ctl_Q_sunlit     = layer_Q_sunlit[1,1,,]/par_energy_content
sen_Q_sunlit     = layer_Q_sunlit[2,2,,]/par_energy_content
ctl_Q_shaded     = layer_Q_shaded[1,1,,]/par_energy_content
sen_Q_shaded     = layer_Q_shaded[2,2,,]/par_energy_content
ctl_Qmax_sunlit     = layer_Qmax_sunlit[1,1,,]/par_energy_content
sen_Qmax_sunlit     = layer_Qmax_sunlit[2,2,,]/par_energy_content
ctl_Qmax_shaded     = layer_Qmax_shaded[1,1,,]/par_energy_content
sen_Qmax_shaded     = layer_Qmax_shaded[2,2,,]/par_energy_content
list_ctl = list(ctl_assim_sunlit, ctl_assim_shaded, ctl_assim_sunlit_max, ctl_assim_shaded_max)
list_sen = list(sen_assim_sunlit, sen_assim_shaded, sen_assim_sunlit_max, sen_assim_shaded_max)
var_names = c("An_sunlit","An_shaded","An_max_sunlit","An_max_shaded")
layers = seq(10,1,by=-1)

fontsize = 1.5
fontsize_label= 1.3

##plot Q,Q dist & An
par(mfrow=c(1,3),cex=1)
#hist
units = expression(paste("Qmax_sunlit (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
hist(ctl_Qmax_sunlit,breaks = 20,xlab = units,main = "",cex.lab=fontsize_label,cex.axis=fontsize)

#Q by layers
list_ctl = list(ctl_Qmax_sunlit, ctl_An_weighted)
list_sen = list(sen_Qmax_sunlit, sen_An_weighted)
x1 = list_ctl[[1]]
x1_mean = colMeans(x1)
x2 = list_sen[[1]]
x2_mean = colMeans(x2)
layers = seq(10,1,by=-1)
yticks = c(10,1)
ytick_labels =c("Top","Bottom")
plot(x1_mean,layers,type="b",xlab = units,ylab = "layers",yaxt="n",
     xlim=range(c(x1_mean,x2_mean)),cex.lab=fontsize_label,cex.axis=fontsize)
lines(x2_mean,layers,type="b",col="red")
axis(side=2, at=yticks, labels = ytick_labels,cex.lab=fontsize_label)
legend(700,10, legend=c('CTL','V20J20'),
       col=c('black','red'), lty=1, cex=1,bty="n",seg.len=0.3,
       y.intersp=0.5,x.intersp = 0.2)

#An
x1 = list_ctl[[2]]
x1 = colMeans(x1)
x2 = list_sen[[2]]
x2 = colMeans(x2)
units = expression(paste("An_sunlit (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
plot(x1,layers,type="b",xlab = units,ylab = "layers",yaxt="n",
     xlim=range(c(x1,x2)),cex.lab=fontsize_label,cex.axis=fontsize)
lines(x2,layers,type="b",col="red")
axis(side=2, at=yticks, labels = ytick_labels)
legend(-4, 10, legend=c('CTL','V20J20'),
       col=c('black','red'), lty=1, cex=1,bty="n",seg.len=0.3,
       y.intersp=0.5,x.intersp = 0.2)

