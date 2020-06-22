#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#         This script serves to calculate weather covariates of interest from a daymetr dataframe. 
#         Currently Growing degree days at 5C, Number of chill days, and Growing season mean temperature
# Inputs: Lubridate package
# Outputs:This function will take a data frame of daily weather data and produce the following summary statistics
#         GDD5 = Growing degree days at 5 degrees C 
#         NCD = Number of chilling days 
#         GTmean = Growing season mean temperature
# Notes: The defaults for this funcion are
#       Julian yday for start of growing season         g_start = 1
#       Julian yday for end of growing season           g_end = 120
#
#-----------------------------------------------------------------------------------------------------------------------------------#

#inputs ystart, y_end, data frame from daymetr::download_daymet_batch function
Daymetr_weather_calc <- function(lat.list, g_start = 1, g_end = 120){
  
y_start <- min(lat.list[[1]]$data$year)
y_end <- max(lat.list[[1]]$data$year)

yrlength <- ((y_end-y_start)+1)
pb <- txtProgressBar(min=0, max=length(lat.list)*yrlength, style=3)
pb.ind=0

#Creating a dataframe to hold weather summary statistics
df.loc <- data.frame(latitude=rep(lat.list[[1]]$latitude, 365* yrlength) ,
                     longitude=rep(lat.list[[1]]$longitude, 365 * yrlength))

#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 1
i <- 1
YR <- y_start
gcount <- 1
for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  df.tmp$GDD5.cum <- NA
  df.tmp$NCD <- NA
  df.tmp$GTmean <- NA
  
  #Setting the parameters of the growing season length for the growing season temp mean
  g_start <- 1
  g_end <- 120
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
    
    df.yr <- df.tmp[df.tmp$year==YR,]
    start <- paste(as.character(df.tmp$year), "-01-01", sep="")
    df.tmp$Date <- as.Date((df.yr$yday-1), origin = start)
    dat.gtmean <- df.yr[(df.yr$yday>=g_start & df.yr$yday<=g_end), ]
    gtmean <- mean(dat.gtmean$TMEAN, na.rm = TRUE)
    
    gdd.cum=0
    d.miss = 0
    ncd = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[j]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      if(!is.na(df.yr$TMEAN[j]) & df.yr$TMEAN[j] < 0){
        ncd <- ncd + 1
      }
      df.yr[j, "NCD"] <- ncd
      df.yr[j,"GDD5.cum"] <- gdd.cum
      df.yr[i, "GTmean"] <- gtmean
    }
    df.tmp[df.tmp$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
    df.tmp[df.tmp$year==YR, "NCD"] <- df.yr$NCD
    df.tmp[df.tmp$year==YR, "GTmean"] <- df.yr$GTmean
  }
  
  df.loc[count:((count-1) + (365 * yrlength)), "latitude"] <- lat.list[[i]]$latitude
  df.loc[count:((count-1) + (365 * yrlength)), "longitude"] <- lat.list[[i]]$longitude
  df.loc[count:((count-1) + (365 * yrlength)), "year"] <- df.tmp$year
  df.loc[count:((count-1) + (365 * yrlength)), "TMAX"] <- df.tmp$tmax..deg.c.
  df.loc[count:((count-1) + (365 * yrlength)), "TMIN"] <- df.tmp$tmin..deg.c.
  df.loc[count:((count-1) + (365 * yrlength)), "TMEAN"] <- df.tmp$TMEAN
  df.loc[count:((count-1) + (365 * yrlength)), "PRCP"] <- df.tmp$prcp..mm.day.
  df.loc[count:((count-1) + (365 * yrlength)), "SNOW"] <- df.tmp$srad..W.m.2.
  df.loc[count:((count-1) + (365 * yrlength)), "YDAY"] <- df.tmp$yday
  df.loc[count:((count-1) + (365 * yrlength)), "Date"] <- df.tmp$Date
  df.loc[count:((count-1) + (365 * yrlength)), "GDD5"] <- df.tmp$GDD5
  df.loc[count:((count-1) + (365 * yrlength)), "GDD5.cum"] <- df.tmp$GDD5.cum
  df.loc[count:((count-1) + (365 * yrlength)), "NCD"] <- df.tmp$NCD
  df.loc[count:((count-1) + (365 * yrlength)), "GTmean"] <- df.tmp$GTmean
  
  count <- count + (365 * yrlength)
  
}
return(df.loc)
}
