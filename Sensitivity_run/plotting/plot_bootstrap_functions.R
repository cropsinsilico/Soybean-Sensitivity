#this is for fig.6 of the bootstrapping results
multiplot<-function(t_doys,podmax_percent,pod_diff,lai_ctl,lai_sen,lai_diff){
  fs = 1.2
  xlabel_space = 1.8
  par(mfrow=c(2,2),cex=fs,mar=c(4,4,3,2)) #c(bottom, left, top, right) 
  ###subplot 1: histogram
  xlabel = expression(paste(Delta,"pod (%)"))
  hist(podmax_percent,breaks=20,xlab=xlabel,main = "",ylab='')
  title(ylab='frequency', line=xlabel_space, cex.lab=fs)
  # lines(x=c(1.5,1.5),y=c(0,50),col='red')
  
  ###subplot 2: pod difference time series
  dataL_mean = rowMeans(pod_diff[,setL])
  dataL_sd   = apply(pod_diff[,setL],1,sd,na.rm=TRUE)
  dataS_mean = rowMeans(pod_diff[,setS])
  dataS_sd   = apply(pod_diff[,setS],1,sd,na.rm=TRUE)
  #use polygon to get the shadow bands
  polygon_x  = c(t_doys,rev(t_doys))
  polygon_y1 = c(dataL_mean+dataL_sd,rev(dataL_mean-dataL_sd))
  polygon_y2 = c(dataS_mean+dataS_sd,rev(dataS_mean-dataS_sd))
  #first line
  ylabel = expression(paste(Delta,"pod (Mg/ha)"))
  plot(t_doys,dataL_mean,type='l',ylim=c(-0.1,0.4),xlim = c(180,260),xlab="DOY",ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  # plot(polygon_x[!is.na(polygon_y1)],polygon_y1[!is.na(polygon_y1)])
  polygon(polygon_x[!is.na(polygon_y1)],polygon_y1[!is.na(polygon_y1)], 
          col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)
  #second line
  lines(t_doys,dataS_mean,col="red")
  polygon(polygon_x[!is.na(polygon_y2)],polygon_y2[!is.na(polygon_y2)], 
          col = rgb(1, 0.5, 0.5, 0.5), border = NA)
  legend(170,0.5, legend=c('set-75','set-25'),
         col=c('black','red'), lty=1, cex=1,bty="n",seg.len=0.3,
         y.intersp=0.2,x.intersp = 0.2)
  # #line separate veg and rep stages
  # doy_reproductive = t_doys[which(DVI>1)[1]]
  # lines(x=c(doy_reproductive,doy_reproductive),y=c(-0.2,0.3),col="green")
  
  ###subplot 3: LAI
  lai1_mean = rowMeans(lai_diff[,setL])
  lai1_sd   = apply(lai_diff[,setL],1,sd,na.rm=TRUE)
  lai2_mean = rowMeans(lai_diff[,setS])
  lai2_sd   = apply(lai_diff[,setS],1,sd,na.rm=TRUE)
  #use polygon to get the shadow bands
  polygon_x  = c(t_doys,rev(t_doys))
  polygon_y1 = c(lai1_mean+lai1_sd,rev(lai1_mean-lai1_sd))
  polygon_y2 = c(lai2_mean+lai2_sd,rev(lai2_mean-lai2_sd))
  #first line
  ylabel = expression(paste(Delta,"LAI (m"^2,"/m"^2,")"))
  plot(t_doys,lai1_mean,type='l',ylim=c(0,0.6),xlim = c(180,260),xlab="DOY",ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  # plot(polygon_x[!is.na(polygon_y1)],polygon_y1[!is.na(polygon_y1)])
  polygon(polygon_x[!is.na(polygon_y1)],polygon_y1[!is.na(polygon_y1)], 
          col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)
  #second line
  lines(t_doys,lai2_mean,col="red")
  polygon(polygon_x[!is.na(polygon_y2)],polygon_y2[!is.na(polygon_y2)], 
          col = rgb(1, 0.5, 0.5, 0.5), border = NA)
  legend(170,0.7, legend=c('set-75','set-25'),
         col=c('black','red'), lty=1, cex=1,bty="n",seg.len=0.3,
         y.intersp=0.2,x.intersp = 0.2)
  
  ###subplot 4: solar or LAI
  plot_lai=TRUE
  if(plot_lai){
    lai_ctl_max = apply(lai_ctl,2,max,na.rm=TRUE)
    lai_sen_max = apply(lai_sen,2,max,na.rm=TRUE)
    lai_max_diff = (lai_sen_max - lai_ctl_max)/lai_ctl_max*100
    # xlabel = expression(paste(Delta,"LAI (m"^2,"/m"^2,")"))
    xlabel = expression(paste(Delta,"LAI (%)"))
    ylabel = expression(paste(Delta,"pod (%)"))
    plot(lai_max_diff,podmax_percent,xlab = xlabel, ylab='')
    title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
    #####do PCA to use TLS instead OLS
    v <- prcomp(cbind(lai_max_diff,podmax_percent))$rotation 
    beta <- v[2,1]/v[1,1]
    intecept <- mean(podmax_percent) - beta*mean(lai_max_diff)
    yhat_TLS <- beta*lai_max_diff+intecept
    #####
    lfit = lm(podmax_percent~lai_max_diff)
    print(cor.test(podmax_percent,lai_max_diff))
    yhat = lfit$fitted.values
    lines(lai_max_diff,yhat,col='red')
  }else{
    climate_varnames = c("temp","solar","rh","precip","ws","gdd","DVI","DVIrate","kleaf","kgrain","solar_max")
    climate_index=2
    condi = which(t_doys>0)  #all growing season
    # condi = which(t_doys<=doy_reproductive)
    plot_option = "no"#"cumsum"
    if(plot_option=="cumsum"){
      var_cumsum = climate_daily[condi,,climate_index]
      for (i in 1:no_years){
        tmp = var_cumsum[,i]
        var_cumsum[,i] = cumsum(tmp)
      }
      data1 = var_cumsum[,setL]
      data2 = var_cumsum[,setS]
    }else{
      var2plot = climate_daily[condi,,climate_index]
      data1 = var2plot[,setL]
      data2 = var2plot[,setS]
    }
    # #plot all replicas
    # var2plot = var_cumsum
    # t2plot   = t_doys[condi]
    # for (i in 1:no_years){
    #   if(i==1){
    #     plot(t2plot, var2plot[,1],type='l',col=alpha("grey", 0.3))
    #   }else{
    #     if(i%in%set2){
    #       lines(t2plot,var2plot[,i],col="red")
    #     }else{
    #       lines(t2plot,var2plot[,i],col=alpha("grey", 0.3))
    #     }
    #   }
    # }
    data1_mean = rowMeans(data1)
    data2_mean = rowMeans(data2)
    # data1_log  = log(data1_mean)
    # data2_log  = log(data2_mean)
    # plot(data1_mean,type="l")
    # lines(data2_mean,col="red")
    # data1_sd = apply(data1,1,sd,na.rm=TRUE)
    # data2_sd = apply(data2,1,sd,na.rm=TRUE)
    # data1_var = apply(data1,1,var,na.rm=TRUE)
    # data2_var = apply(data2,1,var,na.rm=TRUE)
    data_diff = (data1_mean-data2_mean)#/max(data1_mean)*100
    # combine_sd = sqrt(data1_var + data2_var)
    # plot(t_doys[condi],data_diff,type='l'
    #      ,ylab='radiation difference',xlab="DOY")
    # lines(x=c(doy_reproductive,doy_reproductive),y=c(min(data_diff),max(data_diff)),col="green")
    ylabel = expression(paste("Q (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
    # plot(t_doys,log(data1_mean),type='l',xlim = c(180,260),xlab="DOY",ylab='')
    # lines(t_doys,log(data2_mean),col="red")
    # title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
    # #density plot
    hist(data1, prob=TRUE, col="grey",xlim=c(0,800),xlab=ylabel,main="")# prob=TRUE for probabilities not counts
    # lines(density(X), col="blue", lwd=2) # add a density estimate with defaults
    lines(density(data1, adjust=1), col="black", lwd=2)
    hist(data2, prob=TRUE, col=alpha("red", 0.2),add=TRUE,main="")# prob=TRUE for probabilities not counts
    # lines(density(X), col="blue",lty="dotted", lwd=2) # add a density estimate with defaults
    lines(density(data2, adjust=2), col="red", lwd=2)
  }
  
}

multiplot_tempsolar<-function(t_doys,podmax_percent,temp,solar,lai_ctl,lai_sen){
  doy_kgrain_large = 207
  lai_threshold=0.5
  fs = 1.2
  xlabel_space = 1.8
  ind_stage1 = which(lai_ctl[,1]>lai_threshold & t_doys<doy_kgrain_large)
  ind_stage2 = which(lai_ctl[,1]>lai_threshold & t_doys>=doy_kgrain_large)
  lai_ctl_max = apply(lai_ctl,2,max,na.rm=TRUE)
  lai_sen_max = apply(lai_sen,2,max,na.rm=TRUE)
  lai_max_diff = (lai_sen_max - lai_ctl_max)/lai_ctl_max*100
  # print(ind_stage2)
  # 
  pval = c()
  rval = c()
  par(mfrow=c(2,2),cex=fs,mar=c(4,4,3,2)) #c(bottom, left, top, right) 
  xlabel = expression(paste("Temp (°C)"))
  ylabel = expression(paste(Delta,"pod (%)"))
  #p1
  temp_avg = colMeans(temp[ind_stage1,]) #phase1
  plot(temp_avg,podmax_percent,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~temp_avg)
  yhat = lfit$fitted.values
  lines(temp_avg,yhat,col='red')
  # pval = c(pval,summary(lfit)$coefficients[2,4])
  # rval = c(rval,summary(lfit)$r.squared)
  pearson_cor = cor.test(podmax_percent,temp_avg)
  print(cor(podmax_percent,temp_avg))
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  #p2
  temp_avg = colMeans(temp[ind_stage2,]) #phase2
  plot(temp_avg,podmax_percent,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~temp_avg)
  yhat = lfit$fitted.values
  lines(temp_avg,yhat,col='red')
  # pval = c(pval,summary(lfit)$coefficients[2,4])
  # rval = c(rval,summary(lfit)$r.squared)
  pearson_cor = cor.test(podmax_percent,temp_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  
  xlabel = expression(paste("Q (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
  ylabel = expression(paste(Delta,"pod (%)"))
  #p3
  solar_avg = colMeans(solar[ind_stage1,]) #phase1
  plot(solar_avg,podmax_percent,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~solar_avg)
  yhat = lfit$fitted.values
  lines(solar_avg,yhat,col='red')
  # pval = c(pval,summary(lfit)$coefficients[2,4])
  # rval = c(rval,summary(lfit)$r.squared)
  pearson_cor = cor.test(podmax_percent,solar_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  #p4
  solar_avg = colMeans(solar[ind_stage2,]) #phase2
  plot(solar_avg,podmax_percent,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~solar_avg)
  yhat = lfit$fitted.values
  lines(solar_avg,yhat,col='red')
  # pval = c(pval,summary(lfit)$coefficients[2,4])
  # rval = c(rval,summary(lfit)$r.squared)
  pearson_cor = cor.test(podmax_percent,solar_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  
  pr_values = data.frame(pval = pval,rval=rval)
  
  # plot(colMeans(temp[ind_stage1,]),colMeans(solar[ind_stage1,]))
  # plot(colMeans(temp[ind_stage2,]),colMeans(solar[ind_stage2,]))
  # print(cor(colMeans(temp[ind_stage1,]),colMeans(solar[ind_stage1,])))
  # print(cor(colMeans(temp[ind_stage2,]),colMeans(solar[ind_stage2,])))
  # plot(colMeans(temp[ind_stage1,]),lai_max_diff)
  # plot(colMeans(temp[ind_stage2,]),lai_max_diff)
  return(pr_values)
}

scatterplot<-function(var1,var2,xlabel,ylabel,xlabel_space,fs){
  plot(var1,var2,xlab = '', ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  title(xlab=xlabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(var2~var1)
  yhat = lfit$fitted.values
  lines(var1,yhat,col='red')
}

multiplot_prhws<-function(t_doys,podmax_percent,precip,rh,ws,lai_ctl,lai_sen){
  doy_kgrain_large = 207
  lai_threshold=0.5
  fs = 1.2
  xlabel_space = 1.8
  ind_stage1 = which(lai_ctl[,1]>lai_threshold & t_doys<doy_kgrain_large)
  ind_stage2 = which(lai_ctl[,1]>lai_threshold & t_doys>=doy_kgrain_large)
  lai_ctl_max = apply(lai_ctl,2,max,na.rm=TRUE)
  lai_sen_max = apply(lai_sen,2,max,na.rm=TRUE)
  lai_max_diff = (lai_sen_max - lai_ctl_max)/lai_ctl_max*100
  # print(ind_stage2)
  # 
  pval = c()
  rval = c()
  par(mfrow=c(3,2),cex=fs,mar=c(3,4,2,2)) #c(bottom, left, top, right) 
  xlabel = expression(paste("Precip (mm/day)"))
  ylabel = expression(paste(Delta,"pod (%)"))
  #p1
  var_avg = colMeans(precip[ind_stage1,]) #phase1
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  print(cor(podmax_percent,var_avg))
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  #p2
  var_avg = colMeans(precip[ind_stage2,]) #phase2
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  
  xlabel = expression(paste("rh"))
  ylabel = expression(paste(Delta,"pod (%)"))
  #p3
  var_avg = colMeans(rh[ind_stage1,]) #phase1
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  #p4
  var_avg = colMeans(rh[ind_stage2,]) #phase2
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  
  xlabel = expression(paste("ws (m/s)"))
  ylabel = expression(paste(Delta,"pod (%)"))
  #p3
  var_avg = colMeans(ws[ind_stage1,]) #phase1
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  #p4
  var_avg = colMeans(ws[ind_stage2,]) #phase2
  scatterplot(var_avg,podmax_percent,xlabel,ylabel,xlabel_space,fs)
  pearson_cor = cor.test(podmax_percent,var_avg)
  pval = c(pval,pearson_cor$p.value)
  rval = c(rval,pearson_cor$estimate)
  
  pr_values = data.frame(pval = pval,rval=rval)
  
  # plot(colMeans(temp[ind_stage1,]),colMeans(solar[ind_stage1,]))
  # plot(colMeans(temp[ind_stage2,]),colMeans(solar[ind_stage2,]))
  # print(cor(colMeans(temp[ind_stage1,]),colMeans(solar[ind_stage1,])))
  # print(cor(colMeans(temp[ind_stage2,]),colMeans(solar[ind_stage2,])))
  # plot(colMeans(temp[ind_stage1,]),lai_max_diff)
  # plot(colMeans(temp[ind_stage2,]),lai_max_diff)
  return(pr_values)
}

singleplot_climate<-function(t_doys,climate1,climate2,var2,lai_ctl,lai_sen,lai_diff){
  doy_kgrain_large = 207
  lai_threshold=0.5
  fs = 1.2
  xlabel_space = 1.8
  ind_stage1 = which(lai_ctl[,1]>lai_threshold & t_doys<doy_kgrain_large)
  ind_stage2 = which(lai_ctl[,1]>lai_threshold & t_doys>=doy_kgrain_large)
  lai_ctl_max = apply(lai_ctl,2,max,na.rm=TRUE)
  lai_sen_max = apply(lai_sen,2,max,na.rm=TRUE)
  lai_max_diff = (lai_sen_max - lai_ctl_max)/lai_ctl_max*100
  xlabel = expression(paste("Temp (°C)"))
  ylabel = expression(paste(Delta,"An (t/ha)"))
  par(mfrow=c(2,2),cex=fs,mar=c(4,4,3,2)) #c(bottom, left, top, right) 
  #p1
  var1_avg = colMeans(climate1[ind_stage1,],na.rm=TRUE) #phase1
  var2_avg = colMeans(var2[ind_stage1,],na.rm=TRUE) #phase1
  plot(var1_avg,var2_avg,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(var2_avg~var1_avg)
  yhat = lfit$fitted.values
  lines(var1_avg,yhat,col='red')
  #p2
  var1_avg = colMeans(climate1[ind_stage2,],na.rm=TRUE) #phase2
  var2_avg = colMeans(var2[ind_stage2,],na.rm=TRUE) #phase2
  plot(var1_avg,var2_avg,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(var2_avg~var1_avg)
  yhat = lfit$fitted.values
  lines(var1_avg,yhat,col='red')
  
  xlabel = expression(paste("Q (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
  #p3
  var1_avg = colMeans(climate2[ind_stage1,],na.rm=TRUE) #phase1
  var2_avg = colMeans(var2[ind_stage1,],na.rm=TRUE) #phase1
  plot(var1_avg,var2_avg,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(var2_avg~var1_avg)
  yhat = lfit$fitted.values
  lines(var1_avg,yhat,col='red')
  #p4
  var1_avg = colMeans(climate2[ind_stage2,],na.rm=TRUE) #phase2
  var2_avg = colMeans(var2[ind_stage2,],na.rm=TRUE) #phase2
  plot(var1_avg,var2_avg,xlab = xlabel, ylab='')
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(var2_avg~var1_avg)
  yhat = lfit$fitted.values
  lines(var1_avg,yhat,col='red')
}

singleplot<-function(t_doys,var1,podmax_percent,lai_ctl,lai_sen,lai_diff){
  doy_kgrain_large = 207
  lai_threshold=0.5
  fs = 1.2
  xlabel_space = 1.8
  ind_stage1 = which(lai_ctl[,1]>lai_threshold & t_doys<doy_kgrain_large)
  ind_stage2 = which(lai_ctl[,1]>lai_threshold & t_doys>=doy_kgrain_large)
  lai_ctl_max = apply(lai_ctl,2,max,na.rm=TRUE)
  lai_sen_max = apply(lai_sen,2,max,na.rm=TRUE)
  lai_max_diff = (lai_sen_max - lai_ctl_max)/lai_ctl_max*100
  # xlabel = expression(paste(Delta,"An (t/ha)"))
  xlabel = expression(paste("log(Q) (",mu,"mol m"^"-2","s"^"-1",")",sep=""))
  ylabel = expression(paste(Delta,"Pod (%)"))
  par(mfrow=c(1,2),cex=fs,mar=c(4,4,3,2)) #c(bottom, left, top, right) 
  #p1
  var1_avg = colMeans(var1[ind_stage1,],na.rm=TRUE) #phase1
  plot(var1_avg,podmax_percent,xlab = xlabel, ylab='',pch=16,col = alpha('black', 0.2))
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~var1_avg)
  print(summary(lfit))
  # yhat = lfit$fitted.values
  # lines(var1_avg,yhat,col='red')
  abline(lfit,col='red')
  
  v <- prcomp(cbind(var1_avg,podmax_percent))$rotation
  beta <- v[2,1]/v[1,1]
  intecept <- mean(podmax_percent) - beta*mean(var1_avg)
  yhat_TLS <- beta*var1_avg+intecept
  lines(var1_avg,yhat_TLS,col='blue')
  
  #p2
  var1_avg = colMeans(var1[ind_stage2,],na.rm=TRUE) #phase2
  plot(var1_avg,podmax_percent,xlab = xlabel, ylab='',pch=16,col = alpha('black', 0.2))
  title(ylab=ylabel, line=xlabel_space, cex.lab=fs)
  lfit = lm(podmax_percent~var1_avg)
  print(summary(lfit))
  # yhat = lfit$fitted.values
  # lines(var1_avg,yhat,col='red')
  abline(lfit,col='red')
  
  v <- prcomp(cbind(var1_avg,podmax_percent))$rotation 
  beta <- v[2,1]/v[1,1]
  intecept <- mean(podmax_percent) - beta*mean(var1_avg)
  yhat_TLS <- beta*var1_avg+intecept
  lines(var1_avg,yhat_TLS,col='blue')
}
