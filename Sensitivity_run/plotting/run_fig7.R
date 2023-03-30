library(scales)
library(ggplot2)
calc_cumsum<-function(climate_daily,total_years){
  var_cumsum = climate_daily
  for (i in 1:total_years){
    tmp = var_cumsum[,i]
    var_cumsum[,i] = cumsum(tmp)
  }
  return(var_cumsum)
}
sow_day     =   150
harvest_day =   280
CO2 = 400
t_doys    = sow_day:harvest_day
x_daily   = readRDS(paste0('../results_bootstrap//results_CO2_',CO2,
                         '_stage0_05_bootstrap_daily_StomaWS_allday_wholegrowingseason_newBSset1000.rds'))
names(x_daily) = c("pod", "shoot", "An", "wue", "trans", "stomaWS", "lai", "iwue")
climate_daily  = readRDS(paste0("../results_bootstrap//climate_metrics_CO2_",CO2,
                                "_stage0_05_bootstrap_daily_StomaWS_allday_newBSset1000.rds"))
pod_ctl   = x_daily[[1]][1,1,,]
pod_sen   = x_daily[[1]][2,2,,]
An_ctl    = x_daily[[3]][1,1,,,3] #3 is the sum of a day
An_sen    = x_daily[[3]][2,2,,,3]
Anmax_ctl = x_daily[[3]][1,1,,,1] #1 is the max of a day
Anmax_sen = x_daily[[3]][2,2,,,1]
lai_ctl   = x_daily[["lai"]][1,1,,]
lai_sen   = x_daily[["lai"]][1,2,,]
lai_diff  = lai_sen-lai_ctl
An_diff   = Anmax_sen-Anmax_ctl

pod_diff   = pod_sen - pod_ctl
gs_lastday = which(t_doys==280)

pod_harvest = (pod_sen[gs_lastday,] - pod_ctl[gs_lastday,])/pod_ctl[gs_lastday,] *100
podmax_percent = (apply(pod_sen,2,max,na.rm=TRUE) - apply(pod_ctl,2,max,na.rm=TRUE))/
              apply(pod_ctl,2,max,na.rm=TRUE) *100

setS  = which(podmax_percent < quantile(podmax_percent,0.25))
setL =  which(podmax_percent > quantile(podmax_percent,0.75))
set_minmax = c(which.min(podmax_percent),which.max(podmax_percent))

source("plot_bootstrap_functions.R")
multiplot(t_doys,podmax_percent,pod_diff,lai_ctl,lai_sen,lai_diff)
