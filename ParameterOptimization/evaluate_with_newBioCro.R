library(BioCro)
library(ggplot2) # To ggplot functions
library(reshape2) # For melt function
library(data.table) # For first and last functions
library(gridExtra)

opt_result = 'withseed_newBioCro_r9'
# Cost function
run_cwrfsoilwater = FALSE
arg_names <- c('alphaLeaf','alphaRoot','alphaStem','betaLeaf','betaRoot','betaStem',
               'rateSeneLeaf','rateSeneStem','alphaSeneLeaf','betaSeneLeaf',
               'alphaSeneStem','betaSeneStem','alphaPod','betaPod') 
new_par = readRDS(paste0('result_rds/optim_result_DEoptim_',opt_result,'.rds'))
new_par = new_par$optim$bestmem
names(new_par) = arg_names
print(cbind(arg_names,new_par))

years <- c('2002','2004','2005','2006')

# sowing and harvest DOYs for each growing season
dates <- data.frame("year" = c(2002, 2004:2006),"sow" = c(152,149,148,148), "harvest" = c(288, 289, 270, 270))

# initialize variables
results <- list()
weather.growingseason <- list()
ExpBiomass <- list()
ExpBiomass.std <- list()
soybean_solver <- list()
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

for (i in 1:length(years)) {
  yr <- years[i]
  weather <- read.csv(file = paste0('../Data/Weather_data/', yr,'_Bondville_IL_daylength.csv'))
  cwrf_soil <- read.csv(file = paste0('soilwater_Urbana/cwrf_soilwater_', yr,'.csv'))
  if(run_cwrfsoilwater){
    weather = cbind(weather,cwrf_soil[,c(4,5)])
    colnames(weather)[colnames(weather)=="swc"]="soil_water_content" 
    soybean_parameters0$soil_type_indicator = 5#weather$soiltype[1]
  }
  sowdate <- dates$sow[which(dates$year == yr)]
  harvestdate <- dates$harvest[which(dates$year == yr)]
  sd.ind <- which(weather$doy == sowdate)[1]
  hd.ind <- which(weather$doy == harvestdate)[24]
  
  weather.growingseason <- weather[sd.ind:hd.ind,]
  
  soybean_solver[[i]] <- partial_run_biocro(soybean_initial_state0, 
 					       soybean_parameters0, 
					       weather.growingseason,
                                               soybean_steadystate_modules0,
					       soybean_derivative_modules0, 
                                               soybean_solver_params,arg_names)
  
  soybean_solver_i = soybean_solver[[i]] 
  results[[i]] <- soybean_solver_i(new_par)

  ExpBiomass[[i]] <- read.csv(file=paste0('../Data/biomasses_with_seed/',yr,'_ambient_biomass.csv'))
  colnames(ExpBiomass[[i]])<-c("DOY","Leaf","Stem","Pod0","Seed")
  Pod = ExpBiomass[[i]]$Pod0 - ExpBiomass[[i]]$Seed
#  Pod[which.max(Pod):length(Pod)] = max(Pod) #make Pod not decline
  ExpBiomass[[i]]$Pod = Pod 

  
  ExpBiomass.std[[i]] <- read.csv(file=paste0('../Data/biomasses_with_seed/',yr,'_ambient_biomass_std.csv'))
  colnames(ExpBiomass.std[[i]])<-c("DOY","Leaf","Stem","Pod0","Seed")
  ExpBiomass.std[[i]]$Pod = sqrt((ExpBiomass.std[[i]]$Pod0)^2 + (ExpBiomass.std[[i]]$Seed)^2)
  
  RootVals[[i]] <- data.frame("DOY"=ExpBiomass[[i]]$DOY[5], "Root"=0.17*sum(ExpBiomass[[i]][5,2:4])) # See Ordonez et al. 2020, https://doi.org/10.1016/j.eja.2020.126130
  
  numrows[i] <- nrow(weather.growingseason)
  invwts <- ExpBiomass.std[[i]]
  weights[[i]] <- log(1/(invwts[,2:ncol(invwts)]+1e-5))

}

# Define functions to create plots
plot_all_tissues <- function(res, year, biomass, biomass.std) {
  
  r <- reshape2::melt(res[, c("time","Root","Leaf","Stem","Pod","Grain")], id.vars="time")
  r.exp <- reshape2::melt(biomass[, c("DOY", "Leaf", "Stem", "Pod","Seed")], id.vars = "DOY")
  r.exp.std <- reshape2::melt(biomass.std[, c("DOY", "Leaf", "Stem", "Pod","Seed")], id.vars = "DOY")
  r.exp.std$ymin<-r.exp$value-r.exp.std$value
  r.exp.std$ymax<-r.exp$value+r.exp.std$value
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255","#EE3377")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r, aes(x=time,y=value, colour=variable), show.legend = TRUE, size=0.25)
  f <- f + geom_errorbar(data=r.exp.std, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + labs(title=element_blank(), x=paste0('Day of Year (',year,')'),y='Biomass (Mg / ha)')
  f <- f + coord_cartesian(ylim = c(0,10)) + scale_y_continuous(breaks = seq(0,10,2)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.15,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:5], guide = FALSE)
  f <- f + scale_colour_manual(values = col.palette.muted, labels=c('Root','Leaf','Stem','Pod','Seed'))
  
  return(f)
}


saveRDS(results,paste0("eval_results/results_",opt_result,".rds"))

plot_list = list()
for (i in 1:length(years)){
	yr = years[i]
	FigA <- plot_all_tissues(results[[i]], yr, ExpBiomass[[i]], ExpBiomass.std[[i]])
	plot_list[[i]] = FigA
}
pdf_name = paste0("figs/fig_",opt_result,".pdf")
pdf(pdf_name,height = 8, width=8,bg='transparent')
grid.arrange(grobs = plot_list,nrow=2,ncol=2)
dev.off()


