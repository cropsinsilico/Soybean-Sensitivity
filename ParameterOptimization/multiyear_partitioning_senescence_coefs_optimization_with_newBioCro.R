library(BioCro)
library(DEoptim)
#library(dfoptim)

# Cost function
source('multiyear_BioCro_optim_YH.R')
print(c("Running DEoptim......"))
run_cwrfsoilwater = FALSE
# Names of parameters being fit
arg_names_all <- c('alphaLeaf','alphaRoot','alphaStem','betaLeaf','betaRoot','betaStem',
               'rateSeneLeaf','rateSeneStem','alphaSeneLeaf','betaSeneLeaf',
               'alphaSeneStem','betaSeneStem','phi1','phi2','alphaPod','betaPod')
# select which args to be fitted
arg_names <- c('alphaLeaf','alphaRoot','alphaStem','betaLeaf','betaRoot','betaStem',
               'rateSeneLeaf','rateSeneStem','alphaSeneLeaf','betaSeneLeaf',
               'alphaSeneStem','betaSeneStem','alphaPod','betaPod') 

# years, sowing dates, and harvesting dates of growing seasons being fit to
year <- c('2002', '2005')
sow.date <- c(152, 148)
harv.date <- c(288, 270)
#year <- c('2005')
#sow.date <- c( 148)
#harv.date <- c( 270)

## Initialize variables for the cost function
soybean_optsolver <- list()
ExpBiomass <- list()
ExpBiomass.std <- list()
RootVals <- list()
weights <- list()
numrows <- vector()
soybean_steadystate_modules0 = soybean$direct_modules 
soybean_derivative_modules0  = soybean$differential_modules  
soybean_initial_state0       = soybean$initial_values 
soybean_initial_state0$Pod   = 1e-5 
soybean_parameters0          = soybean$parameters
soybean_parameters0$alphaPod = soybean_parameters0$alphaStem
soybean_parameters0$betaPod  = soybean_parameters0$betaStem
soybean_parameters0$time_zone_offset= -6

if(run_cwrfsoilwater){
  soybean_initial_state0 = soybean_initial_state0[names(soybean_initial_state0)!='soil_water_content']
}

soybean_solver_params=soybean$ode_solver
soybean_solver_params$type="homemade_euler"

for (i in 1:length(year)) {
  yr <- year[i]
  weather <- read.csv(file = paste0('../Data/Weather_data/', yr,'_Bondville_IL_daylength.csv'))
  cwrf_soil <- read.csv(file = paste0('soilwater_Urbana/cwrf_soilwater_', yr,'.csv'))
  if(run_cwrfsoilwater){
    weather = cbind(weather,cwrf_soil[,c(4,5)])
    colnames(weather)[colnames(weather)=="swc"]="soil_water_content" 
    soybean_parameters0$soil_type_indicator = 5#weather$soiltype[1]
  }
  sd.ind <- which(weather$doy == sow.date[i])[1]
  hd.ind <- which(weather$doy == harv.date[i])[24]
  
  weather.growingseason <- weather[sd.ind:hd.ind,]
  
  soybean_optsolver[[i]] <- partial_run_biocro(soybean_initial_state0, 
 					       soybean_parameters0, 
					       weather.growingseason,
                                               soybean_steadystate_modules0,
					       soybean_derivative_modules0, 
                                               soybean_solver_params,arg_names)
  
  ExpBiomass[[i]] <- read.csv(file=paste0('../Data/biomasses_with_seed/',yr,'_ambient_biomass.csv'))
  colnames(ExpBiomass[[i]])<-c("DOY","Leaf","Stem","Pod0","Seed")
  Pod = ExpBiomass[[i]]$Pod0 - ExpBiomass[[i]]$Seed
#  Pod[which.max(Pod):length(Pod)] = max(Pod) #make Pod not decline
  ExpBiomass[[i]]$Pod = Pod 

  ExpBiomass[[i]]$Pod0 = NULL
  
  ExpBiomass.std[[i]] <- read.csv(file=paste0('../Data/biomasses_with_seed/',yr,'_ambient_biomass_std.csv'))
  colnames(ExpBiomass.std[[i]])<-c("DOY","Leaf","Stem","Pod0","Seed")
  ExpBiomass.std[[i]]$Pod = sqrt((ExpBiomass.std[[i]]$Pod0)^2 + (ExpBiomass.std[[i]]$Seed)^2)
  ExpBiomass.std[[i]]$Pod0 = NULL
  
  RootVals[[i]] <- data.frame("DOY"=ExpBiomass[[i]]$DOY[5], "Root"=0.17*sum(ExpBiomass[[i]][5,2:4])) # See Ordonez et al. 2020, https://doi.org/10.1016/j.eja.2020.126130
  
  numrows[i] <- nrow(weather.growingseason)
  invwts <- ExpBiomass.std[[i]]
  weights[[i]] <- log(1/(invwts[,2:ncol(invwts)]+1e-5))

}

wts2 <- data.frame("Stem" = 1, "Leaf" = 1, "Pod" = 1, "Seed" = 10,"Root" = 0.01)

## Optimization settings
ul = 50 
ll = -50

# parameter upper limit
upperlim<-c(ul,ul,ul,
            0,0,0,
            0.0125,.005,
            ul,0,ul,0,
            1,10,
            ul,0)

# parameter lower limit
lowerlim<-c(0,0,0,
            ll,ll,ll,
            0,0,
            0,ll,0,ll,
            0.001,0.5,
            0,ll)

upperlim = upperlim[arg_names_all%in%arg_names]
lowerlim = lowerlim[arg_names_all%in%arg_names]
#print(cbind(upperlim,lowerlim))
# cost function
cost_func <- function(x){
  multiyear_BioCro_optim_YH(x, soybean_optsolver, ExpBiomass, numrows, weights, wts2, RootVals)
}


rng.seed <- 1234 
set.seed(rng.seed)

# initialize parameters being fitted as randome values from a uniform distribution
opt_pars <- runif(length(arg_names), min = lowerlim, max = upperlim)
# maximum number of iterations
max.iter <- 2000
stopping_error = 10 
cl <- makeCluster(8)

# Call DEoptim function to run optimization
parVars <- c('multiyear_BioCro_optim_YH','soybean_optsolver','ExpBiomass','numrows','weights','wts2','RootVals')
clusterExport(cl, parVars,envir=environment())
optim_result<-DEoptim(fn=cost_func, lower=lowerlim, upper = upperlim, 
		     control=list(itermax=max.iter,parallelType=1,
                     packages=c('BioCro'),parVar=parVars,VTR=stopping_error,cluster=cl))

saveRDS(optim_result,'result_rds/optim_result_DEoptim_withseed_newBioCro_r9.rds')
