#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#         This script serves to calculate weather covariates of interest. 
#         Currently Growing degree days at 5C, Growing degree days at 0C, Number of chill days, and Growing season mean temperature
# Inputs: Lubridate package
# Outputs:This function will take a data frame of daily weather data and produce the following summary statistics
#         GDD5 = Growing degree days at 5 degrees C 
#         GDD0 = Growing degree days at 0 degrees C
#         NCD = Number of chilling days 
#         GTmean = Growing season mean temperature
# Notes: The defaults for this funcion are
#       Starting year of interest                       y_start = 1975
#       Ending year of interest                         y_end = 2019
#       Julian yday for start of growing season         g_start = 1
#       Julian yday for end of growing season           g_end = 120
#
#-----------------------------------------------------------------------------------------------------------------------------------#
# Calculating the Tmean for the growing season of that year
weather_calc <- function(met.all, y_start=1975, y_end=2019, g_start=1, g_end=120){
  
#Pulling out useful date statistics. Only YDAY and YEAR are used in this function but others are added to the output data frame for future use
met.all$YEAR <- lubridate::year(met.all$DATE)
met.all$MONTH <- lubridate::month(met.all$DATE)
met.all$DAY <- lubridate::day(met.all$DATE)
met.all$YDAY <- lubridate::yday(met.all$DATE)

#Subsetting to a useful range of years. Currently the max so doesn't do much
met.all <- met.all[met.all$YEAR>=y_start & met.all$YEAR<=y_end,]

#Calculating mean temperature, growing degree days using 5C, and gorwing degree days using 0c
met.all$TMEAN <- (met.all$TMAX + met.all$TMIN)/2
met.all$GDD5 <- ifelse(met.all$TMEAN>5, met.all$TMEAN-5, 0)
met.all$GDD0 <- ifelse(met.all$TMEAN>0, met.all$TMEAN, 0)

#Creating empty columns to fill in the loop
met.all$GDD5.cum <- NA
met.all$GDD0.cum <- NA
met.all$NCD <- NA

#Setting the beginning and end (using julian yday) of the growing season for our Growing season mean
g_start <- 1
g_end <- 120


# Calculate the cumulative growing degree days for each day/year
for(YR in unique(met.all$YEAR)){
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  dat.gtmean <- dat.tmp[(dat.tmp$YDAY>=g_start & dat.tmp$YDAY<=g_end), ]
  gtmean <- mean(dat.gtmean$TMEAN, na.rm = TRUE)
  
  if(min(dat.tmp$DATE)>as.Date(paste0(YR, "-01-01"))) next
  gdd5.cum=0; gdd0.cum=0
  d5.miss = 0; d0.miss=0
  ncd = 0
  for(i in 1:nrow(dat.tmp)){
    if(is.na(dat.tmp$GDD5[i]) & d5.miss<=7){ #YOU CHANGED THIS TO 7 FOR NOW BUT CHANGE BACK
      d5.miss <- d5.miss+1 # Let us miss up to 3 consecutive days
      gdd5.cum <- gdd5.cum+0
    } else {
      d5.miss = 0 # reset to 0
      gdd5.cum <- gdd5.cum+dat.tmp$GDD5[i] 
    }
    
    if(is.na(dat.tmp$GDD0[i]) & d0.miss<=7){ #YOU CHANGED THIS TO 7 FOR NOW BUT CHANGE BACK
      d0.miss <- d0.miss+1 # Let us miss up to 3 consecutive days
      gdd0.cum <- gdd0.cum+0
    } else {
      d0.miss = 0 # reset to 0
      gdd0.cum <- gdd5.cum+dat.tmp$GDD0[i] 
    }
    if(!is.na(dat.tmp$TMEAN[i]) & dat.tmp$TMEAN[i] < 0){
      ncd <- ncd + 1
    }
    
    dat.tmp[i,"GDD5.cum"] <- gdd5.cum
    dat.tmp[i,"GDD0.cum"] <- gdd0.cum
    dat.tmp[i, "NCD"] <- ncd
    dat.tmp[i, "GTmean"] <- gtmean
  }
    
  met.all[met.all$YEAR==YR, "GDD5.cum"] <- dat.tmp$GDD5.cum
  met.all[met.all$YEAR==YR, "GDD0.cum"] <- dat.tmp$GDD0.cum
  met.all[met.all$YEAR==YR, "NCD"] <- dat.tmp$NCD
  met.all[met.all$YEAR==YR, "GTmean"] <- dat.tmp$GTmean
}
return(met.all)
}
