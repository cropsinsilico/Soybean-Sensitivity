# library(ggbiplot)
library(pls)
library(epiR)
sow_day     =   150
harvest_day =   280
total_samples    = 1000    #bootstrap sample size
CO2 = 400
t_doys    = sow_day:harvest_day
x_daily = readRDS(paste0('../results_bootstrap//results_CO2_',CO2,
                         '_stage0_05_bootstrap_daily_StomaWS_allday_wholegrowingseason_newBSset1000.rds'))
names(x_daily)=c("pod", "shoot", "An", "wue", "trans", "stomaWS", "lai", "iwue")
climate_daily  = readRDS(paste0("../results_bootstrap/climate_metrics_CO2_",CO2,
                                "_stage0_05_bootstrap_daily_StomaWS_allday_newBSset1000.rds"))
pod_ctl  = x_daily[[1]][1,1,,]
pod_sen  = x_daily[[1]][2,2,,]
An_ctl   = x_daily[[3]][1,1,,,3] #3 is the sum of a day
An_sen   = x_daily[[3]][2,2,,,3]
lai_ctl  = x_daily[["lai"]][1,1,,]
lai_sen  = x_daily[["lai"]][1,2,,]
lai_diff = lai_sen-lai_ctl

pod_diff = pod_sen - pod_ctl
gs_lastday = which(t_doys==280)

pod_harvest = (pod_sen[gs_lastday,] - pod_ctl[gs_lastday,])/pod_ctl[gs_lastday,] *100
podmax_percent = (apply(pod_sen,2,max,na.rm=TRUE) - apply(pod_ctl,2,max,na.rm=TRUE))/
  apply(pod_ctl,2,max,na.rm=TRUE) *100

doy_kgrain_large = 207
lai_threshold    = 0.5
ind_stage1 = which(lai_ctl[,1]>lai_threshold & t_doys<doy_kgrain_large)
ind_stage2 = which(lai_ctl[,1]>lai_threshold & t_doys>=doy_kgrain_large)

dependent_var = podmax_percent
pca_var = c()
climate_vars = climate_daily[,,c(11,1,4,3,5)] #Q, T, P, RH, WS
no_of_climate = 5
#do annual average of all daily vars
for (i in 1:no_of_climate){
  tmp = climate_vars[,,i]
  var_avg1 = colMeans(tmp[ind_stage1,]) #phase1
  var_avg2 = colMeans(tmp[ind_stage2,]) #phase2
  pca_var = cbind(pca_var,var_avg1,var_avg2)
}
colnames(pca_var) = c("Q1","Q2","T1","T2","P1","P2","RH1","RH2","WS1","WS2")
mydata=cbind(pca_var,podmax_percent)
mydata=as.data.frame(mydata)
mydata_sub = mydata[1:total_samples,]
PRCC = epi.prcc(mydata_sub, sided.test = 2, conf.level = 0.95)
significance = 1:length(dim(PRCC)[1])
significance[PRCC$p.value>0.05] = "ns"
significance[PRCC$p.value<=0.05 & PRCC$p.value>0.01] = "*"
significance[PRCC$p.value<=0.01 & PRCC$p.value>0.001] = "**"
significance[PRCC$p.value<=0.001] = "***"
importance = PRCC$est
significance_pos = importance
label_pos_shift = 0.02
significance_pos[importance>0] = importance[importance>0]+label_pos_shift
significance_pos[importance<0] = importance[importance<0]-label_pos_shift
names(importance) = colnames(pca_var)
bp <- barplot(importance,horiz = FALSE,ylim=c(-0.5,0.5),
              ylab = "Partial rank correlation coefficient",
              cex.axis = 1.2,cex.names=1.2,cex.lab=1.2)
abline(h=0)
text(bp, significance_pos, labels = significance,cex=1.2)
