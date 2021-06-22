pow <- function(x,y) return(x^y)
tempFunc<-function(Temp,  Tmin,  Topt,  Tmax){

  if (Temp > Tmin && Temp < Tmax) {
     alpha = log(2.0) / log((Tmax - Tmin) / (Topt - Tmin)) # dimensionless
     fT_num_pt1 = 2.0 * pow(Temp - Tmin, alpha) * pow(Topt - Tmin, alpha)  # dimensionless
     fT_num_pt2 = pow(Temp - Tmin, 2.0 * alpha) # dimensionless
     fT_denom = pow(Topt - Tmin, 2.0 * alpha) # dimensionless
    
    fT = (fT_num_pt1 - fT_num_pt2) / fT_denom  # dimensionless
  } else {
    fT = 0.0  # dimensionless
  }
  
  return(fT)  # dimensionless
}
gdd_calc <- function(DVI_vec,temp_vec){
if(length(DVI_vec) != length(temp_vec)) stop("length error!")
  Tbase_emr             =                 10          # degrees C
  TTemr_threshold       =                 60          # degrees C * days
  Rmax_emrV0            =                 0.1990      # day^-1; Setiyono et al., 2007 (https://doi.org/10.1016/j.fcr.2006.07.011), Table 2
  Tmin_emrV0            =                 5.0         # degrees C; Setiyono et al., 2007, Table 2
  Topt_emrV0            =                 31.5        # degrees C; Setiyono et al., 2007, Table 2
  Tmax_emrV0            =                 45.0        # degrees C; Setiyono et al., 2007, Table 2
  Tmin_R0R1             =                 5.0         # degrees C; Setiyono et al., 2007, Table 2 (used emrV0 values)
  Topt_R0R1             =                 31.5        # degrees C; Setiyono et al., 2007, Table 2 (used emrV0 values)
  Tmax_R0R1             =                 45.0        # degrees C; Setiyono et al., 2007, Table 2 (used emrV0 values)
  Tmin_R1R7             =                 0.0         # degrees C; Setiyono et al., 2007, Table 2
  Topt_R1R7             =                 21.5        # degrees C; Setiyono et al., 2007, Table 2
  Tmax_R1R7             =                 38.7        # degrees C; Setiyono et al., 2007, Table 2
gdd_sum = 0
for (i in 1:length(DVI_vec)){
  DVI  = DVI_vec[i]
  temp = temp_vec[i]
  if (DVI < (-1)) {
    # error, DVI out of bounds, this should never occur unless initial DVI
    # state is less than -1.
    soybean_development_rate = 0 # day^-1
    gdd_rate = 0  #degree day-1
  } else if (DVI < 0) {
    # 1. Sowing to emergence
    temp_diff = temp - Tbase_emr # degrees C
    gdd_rate = temp_diff
  } else if (DVI < 0.333) {
    # 2a. Emergence - V0 (cotyledon stage) r = Rmax * f(T)
    gdd_rate = tempFunc(temp, Tmin_emrV0, Topt_emrV0, Tmax_emrV0) * (Topt_emrV0 - Tmin_emrV0)
  } else if (DVI < 0.667) {
    # 2b. V0 (cotyledon) - R0 (end of floral induction) r = Rmax * f(P)
    gdd_rate = tempFunc(temp, Tmin_emrV0, Topt_emrV0, Tmax_emrV0) * (Topt_emrV0 - Tmin_emrV0)
  } else if (DVI < 1) {
    # 2c. R0 (end of floral induction) - R1 (flowering) r = Rmax * f(T)
    gdd_rate = tempFunc(temp, Tmin_R0R1, Topt_R0R1, Tmax_R0R1) * (Topt_R0R1 - Tmin_R0R1)
  } else {
    # 3. Reproductive stages, R1 (flowering) - R7 (maturity)
    gdd_rate = tempFunc(temp, Tmin_R1R7, Topt_R1R7, Tmax_R1R7) * (Topt_R1R7 - Tmin_R1R7)
  }
  gdd_rate = gdd_rate / 24.0 # degree C hr^-1
  gdd_sum = gdd_sum + gdd_rate
}
  return(gdd_sum)
}
