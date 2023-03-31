#this result can be generated using SoybeanBioCro-Sensitivity.R
X1 = readRDS(paste0("../results_rds//Ci_hourly_400.rds"))
Ci_sunlit = X1[[1]]
Ci_shaded = X1[[2]]
solar_hourly = X1[[3]]
Ci_sunlit_sen = X1[[4]]
Ci_shaded_sen = X1[[5]]
minA_sunlit = X1[[6]]
minA_shaded = X1[[7]]
minA_sunlit_ctl = minA_sunlit[1,1,,,]
minA_sunlit_sen = minA_sunlit[1,2,,,]

Ci_sunlit_combine_ctl = c()
Ci_shaded_combine_ctl = c()
Ci_sunlit_combine_sen = c()
Ci_shaded_combine_sen = c()
layer_no = 1
for (i in 1:1){
  y=Ci_sunlit[[i]]
  solar = solar_hourly[[i]]
  yy= y[solar>10,layer_no]
  z = as.matrix(yy)
  Ci_sunlit_combine_ctl = c(Ci_sunlit_combine_ctl,c(z))
  y=Ci_shaded[[i]]
  yy= y[solar>10,layer_no]
  z = as.matrix(yy)
  Ci_shaded_combine_ctl = c(Ci_shaded_combine_ctl,c(z))
  
  y=Ci_sunlit_sen[[i]]
  yy= y[solar>10,layer_no]
  z = as.matrix(yy)
  Ci_sunlit_combine_sen = c(Ci_sunlit_combine_sen,c(z))
  y=Ci_shaded_sen[[i]]
  yy= y[solar>10,layer_no]
  z = as.matrix(yy)
  Ci_shaded_combine_sen = c(Ci_shaded_combine_sen,c(z))
}
par(mfrow=c(1,2),cex=1.8)
hist(Ci_sunlit_combine_ctl,xlab="Sunlit Ci (ppm)",main="CTL",ylim=c(0,1500))
hist(Ci_sunlit_combine_sen,xlab="Sunlit Ci (ppm)",main="Jmax+20%",ylim=c(0,1500))

par(mfrow=c(1,2),cex=1.8)
colors = c("orange","blue","purple")
xlabels = c("Ac","Aj","Ap")
graphics::barplot(minA_sunlit_ctl[1,1,],col=colors,ylim=c(0,3000),ylab="frequency",names=xlabels)
graphics::barplot(minA_sunlit_sen[1,1,],col=colors,ylim=c(0,3000),ylab="frequency",names=xlabels)
