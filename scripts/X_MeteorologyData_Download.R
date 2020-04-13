# ---------------------------------
# Script to pull, organize, and store the latest met observations and forecasts for use in forecasting phenology.
# Primary author: Christy Rollinson, crollinson@mortonarb.org

# Two Key Datasets:
# 1. Recent past weather data: NOAA COOP Station ID 
#    - The Morton Arboretum current Station
# 2. NOAA Weather forecast (ideally with uncertainty)
# 
# What we need from each/steps
# - Currently just temperature, but sun and precip may become 
#   important, so lets get what we can
# 
# General Workflow for each dataset:
# 1. Identify window we need based on current date and what data exists
#    - for COOP station data, we just need what we don't have yet, so no 
#      need to re-extract data in hand
#    - for forecast data, we'll want to overwrite existing forecast 
#      because it will be continually updated and once a day has happened, 
#      it should be in the COOP station data
# 2. Calculate key values:
#     - Growing Degree Days (base 0 C, 5 C)
#     - Think about For Future:
#       - Cold thresholds (base 0 C)
#       - Cumulative precip
#       - Days without rain: total, current tally
# 3. Write files to existing structure to feed into forecast

# NOTES
# Currently running for all GHCN station data; 
#  - probably want to edit so we're only doing the current year for the sake of time
# Probably want to convert this script to a function 
#  - so it can be executed more easily & transparently
# ---------------------------------

# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
library(ggplot2)

path.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting"

dir.met <- "../data_raw/meteorology"
dir.create(dir.met, recursive=T, showWarnings = F)
# ---------------------------------


# ---------------------------------
# 1. NOAA COOP Station data
# Use FedData Package to download station data and put it here
# ---------------------------------
ID="USC00115097"
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
path.save=c("../data_raw/meteorology/GHCN_extracted/")
dir.raw="../data_raw/meteorology/GHCN_raw/"

source("met_download_GHCN.R")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.save, dir.raw=dir.raw, gapfill=T)
# -------------------------------------

# ---------------------------------
# 2. Forecast Data 
# ---------------------------------
lat.in=41.812739
lon.in=-88.072749
vars.in <- c("tmax", "tmin", "precip")
forecast.start = Sys.Date()-7
forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")

# Note CFS is what Shawn Taylor used in his Ecol App Pub
# -- he downloaded the 5 most recent forecasts to get uncertainty
source("met_download_CFS.R")
path.save.cfs="../data_raw/meteorology/CFS_Forecast"
vars.in <- c("tmax", "tmin", "prate")
download.cfs(vars.in=vars.in, lat.in=lat.in, lon.in=lon.in, forecast.start=forecast.start, forecast.end=forecast.end, path.save=path.save.cfs)

source("met_download_NMME.R")
mod.use <- c("CCSM4", "CanCM4")
for(MOD in mod.use){
  path.save=file.path("../data_raw/meteorology", MOD)
  
  download.nmme(mod.use=MOD, vars.in, lat.in, lon.in, path.save, forecast.start = forecast.start, forecast.end = forecast.end)
}

# -------------------------------------



# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
yr.min <- 2008
yr.max <- lubridate::year(Sys.Date())
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]

  dat.ghcn[rows.yr, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum")] <- cumsum(dat.ghcn[rows.yr, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP")])
  
  dat.ghcn[rows.yr, c("NORAIN.cum")] <- cumsum(ifelse(dat.ghcn[rows.yr,"PRCP"]>0, 1, 0))
  
  # Calculating days since rain just in case
  dat.ghcn$DaysNoRain[rows.yr[1]] <- ifelse(dat.ghcn$PRCP[rows.yr[1]]>0, 0, 1)
  for(i in 2:length(rows.yr)){
    dat.ghcn$DaysNoRain[rows.yr[i]] <- ifelse(dat.ghcn$PRCP[rows.yr[i]]>0, dat.ghcn$DaysNoRain[rows.yr[i-1]]+1, 0)
  }
}
summary(dat.ghcn)

write.csv(dat.ghcn, file.path(path.out, "data", "weather_ArbCOOP_latest.csv"), row.names=F)
# -------------------------------------



tmax <- read.csv(file.path(path.save.cfs, "tmax_cfs_latest.csv"))
names(tmax) <- c("time", "latitude", "longitude", "vertCoord", "tmax")
tmax$date <- as.Date(substr(tmax$time, 1, 10))
tmax <- aggregate(tmax ~ date + latitude + longitude, data=tmax, FUN=max)
summary(tmax)
library(ggplot2)
ggplot(data=tmax) +
  geom_line(aes(x=date, y=tmax-273))
# ---------------------------------
