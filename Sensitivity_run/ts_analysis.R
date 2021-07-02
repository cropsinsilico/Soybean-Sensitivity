library(ggplot2)
library(gridExtra)
library(reshape2)
yr = 2006
solar_threshold = 10
sow_day     =   150
harvest_day =   280
convert_rate_Assim = 1/(3600 * 1e-6 * 30 * 1e-6 * 10000) #Mg/ha/hr to umol/m2/s
convert_rate_Trans = 1/(3600 * 1e-3 * 18 * 1e-6 * 10000) #Mg/ha/hr to mmol/m2/s 
weather <- read.csv(file = paste0('./WeatherData/',yr,'/', yr, '_Bondville_IL_daylength.csv'))
sd.ind <- which(weather$doy == sow_day)[1]      # start of sowing day
hd.ind <- which(weather$doy == harvest_day)[24] # end of harvest day
weather.growingseason<- weather[sd.ind:hd.ind,]
solar = weather.growingseason$solar
temp  = weather.growingseason$temp
rh    = weather.growingseason$rh
precip = weather.growingseason$precip

ctl = readRDS(paste0("results_",yr,"_CTL.rds"))
sen = readRDS(paste0("results_",yr,"_SEN.rds"))
dvi0 = ctl$doy[tail(which(ctl$DVI<1),1)]
DOY = ctl$doy
doy_unique = unique(DOY)
doy_unique = doy_unique[!is.na(doy_unique)]

plot(ctl$doy,ctl$lai,type="l",col="blue")
lines(sen$doy,sen$lai)
lines(c(dvi0,dvi0),c(-10,10),col="red")

plot(ctl$doy,sen$lai-ctl$lai)
lines(c(dvi0,dvi0),c(-10,10),col="red")

plot(ctl$doy,ctl$canopy_transpiration_rate,type="l",col="blue")
lines(sen$doy,sen$canopy_transpiration_rate)
lines(c(dvi0,dvi0),c(-10,10),col="red")

plot(ctl$doy,sen$Grain-ctl$Grain,type="p",col="blue")
lines(c(dvi0,dvi0),c(-10,10),col="red")

#get daily values
canopy_assim_dmean_ctl = c()
canopy_trans_dmean_ctl = c()
canopy_assim_dmean_sen = c()
canopy_trans_dmean_sen = c()
solar_dmean = c()
temp_dmean = c()
rh_dmean = c()
precip_dmean = c()
for (ss in 1:length(doy_unique)){
  d_ss = doy_unique[ss]
  doy_ind = which(DOY==d_ss)
  solar_dayi  = solar[doy_ind]
  temp_dayi   = temp[doy_ind]
  rh_dayi     = rh[doy_ind]
  precip_dayi = precip[doy_ind]
  daytime_ind = which(solar_dayi>solar_threshold)
  #ctl
  canopy_assim = ctl$canopy_assimilation_rate
  canopy_trans = ctl$canopy_transpiration_rate
  canopy_assim_dayi = canopy_assim[doy_ind]
  canopy_trans_dayi = canopy_trans[doy_ind]
  canopy_assim_dmean_ctl = c(canopy_assim_dmean_ctl,mean(canopy_assim_dayi[daytime_ind],na.rm=TRUE))
  canopy_trans_dmean_ctl = c(canopy_trans_dmean_ctl,mean(canopy_trans_dayi[daytime_ind],na.rm=TRUE))
  #sen
  canopy_assim = sen$canopy_assimilation_rate
  canopy_trans = sen$canopy_transpiration_rate
  canopy_assim_dayi = canopy_assim[doy_ind]
  canopy_trans_dayi = canopy_trans[doy_ind]
  canopy_assim_dmean_sen = c(canopy_assim_dmean_sen,mean(canopy_assim_dayi[daytime_ind],na.rm=TRUE))
  canopy_trans_dmean_sen = c(canopy_trans_dmean_sen,mean(canopy_trans_dayi[daytime_ind],na.rm=TRUE))
  solar_dmean  = c(solar_dmean, mean(solar_dayi[daytime_ind]))
  temp_dmean   = c(temp_dmean, mean(temp_dayi[daytime_ind]))
  rh_dmean     = c(rh_dmean, mean(rh_dayi[daytime_ind]))
  precip_dmean = c(precip_dmean, sum(precip_dayi[daytime_ind]))
}
wue_ctl = canopy_assim_dmean_ctl*convert_rate_Assim/(canopy_trans_dmean_ctl*convert_rate_Trans)
wue_sen = canopy_assim_dmean_sen*convert_rate_Assim/(canopy_trans_dmean_sen*convert_rate_Trans)
# x_diff = wue_sen-wue_ctl
x_diff = canopy_assim_dmean_sen - canopy_assim_dmean_ctl
# x_diff = canopy_trans_dmean_ctl*convert_rate_Trans# canopy_trans_dmean_sen - canopy_trans_dmean_ctl
plot(doy_unique,x_diff,type="p",col="blue")
lines(c(dvi0,dvi0),c(-10,10),col="red")

doy_negative = doy_unique[which(x_diff<0)]
# doy_positive = doy_unique[which(x_diff>0)]
doy_min = 180
doy_max = 260
# doy_positive = doy_positive[doy_positive > doy_min & doy_positive < doy_max]
# x=temp[is.element(DOY,doy_negative)]

df1 = data.frame(doy=doy_unique,solar=solar_dmean,temp=temp_dmean,rh=rh_dmean,precip=precip_dmean)
negative_ind = which(x_diff<0)
df2 = data.frame(doy=doy_unique[negative_ind],solar=solar_dmean[negative_ind],temp=temp_dmean[negative_ind],
                 rh=rh_dmean[negative_ind],precip=precip_dmean[negative_ind])
df1$Dataset = "DF1"
df2$Dataset = "DF2"
DF <- rbind(df1,df2)
colnames <- names(DF)[-c(1)]
plist=list()
for (i in 1:4){
  p <- ggplot(DF, aes_string(x = "doy", y = colnames[i], color = "Dataset"))+
       geom_point(size=3)+
       scale_color_manual(values=c("blue","red"),labels= c("ALL","ABNORMAL"))+
       xlim(doy_min, doy_max)
  plist[[i]] = p
}
pdfname="debug_negative.pdf"
pdf(pdfname,height = 6, width=15)
grid.arrange(grobs = plist,nrow=2,ncol=2)
dev.off()

stop()
plot(doy_unique,precip_dmean,type="p",col="blue")
lines(doy_negative,precip_dmean[which(x_diff<0)],col="red",type="p")

years=2006:2015
z=readRDS("results_rds/results_CO2_400_Jmax_and_Vmax.rds")
pod0 = z[[4]]
pod = pod0[,,,3]
plot(years,pod[6,6,],type="l",col="blue",ylim=c(4,8))
lines(years,pod[8,8,])
