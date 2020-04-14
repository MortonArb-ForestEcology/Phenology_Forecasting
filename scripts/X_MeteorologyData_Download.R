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
path.ghcn=c("../data_raw/meteorology/GHCN_extracted/")
dir.raw="../data_raw/meteorology/GHCN_raw/"

source("met_download_GHCN.R")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T)
# -------------------------------------

# ---------------------------------
# 2. Forecast Data 
# ---------------------------------
lat.in=41.812739
lon.in=-88.072749
forecast.start = Sys.Date()-7
forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")

# Note CFS is what Shawn Taylor used in his Ecol App Pub
# -- he downloaded the 5 most recent forecasts to get uncertainty
source("met_download_CFS.R")
path.save.cfs="../data_raw/meteorology/CFS_Forecast"
vars.in <- c("tmax", "tmin", "prate")
download.cfs(vars.in=vars.in, lat.in=lat.in, lon.in=lon.in, forecast.start=forecast.start, forecast.end=forecast.end, path.save=path.save.cfs)

source("met_download_NMME.R")
vars.in <- c("tmax", "tmin", "precip")
mod.use <- c("CCSM4", "CanCM4")
for(MOD in mod.use){
  path.save=file.path("../data_raw/meteorology", MOD)
  
  download.nmme(mod.use=MOD, vars.in, lat.in, lon.in, path.save, forecast.start = forecast.start, forecast.end = forecast.end)
}

# -------------------------------------



# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
# Create a function to calculate values we're interested in for prediction
calc.indices <- function(dat){
  # Assumes upper case column names of: TMAX, TMIN, PRCP, YDAY
  # For chilling days, only start after the solstice (June 20th)
  dat$TMEAN <- apply(dat[,c("TMAX", "TMIN")], 1, mean)
  dat$GDD0 <- ifelse(dat$TMEAN>0, dat$TMEAN-0, 0)
  dat$GDD5 <- ifelse(dat$TMEAN>5, dat$TMEAN-5, 0)
  dat$CDD0 <- ifelse(dat$YDAY>172 & dat$TMEAN<0, 0-dat$TMEAN, 0)
  dat$CDD2 <- ifelse(dat$YDAY>172 & dat$TMEAN< -2, -2-dat$TMEAN, 0)
  dat$DaysNoRain <- NA
  
  dat[, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum")] <- cumsum(dat[, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP")])
  
  dat[, c("NORAIN.cum")] <- cumsum(ifelse(dat[,"PRCP"]>0, 1, 0))
  
  # Calculating days since rain just in case
  dat$DaysNoRain[1] <- ifelse(dat$PRCP[1]>0, 0, 1)
  for(i in 2:nrow(dat)){
    dat$DaysNoRain[i] <- ifelse(dat$PRCP[i]>0, dat$DaysNoRain[i-1]+1, 0)
  }
  return(dat)
}

# For the "historical" GHCN data
dat.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)
summary(dat.ghcn)

yr.min <- 2008
yr.max <- lubridate::year(Sys.Date())
dat.ghcn2 <- data.frame()
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]
  dat.tmp <- calc.indices(dat=dat.ghcn[rows.yr,])
  dat.ghcn2 <- rbind(dat.ghcn2, dat.tmp)
}
summary(dat.ghcn2)

write.csv(dat.ghcn2, file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"), row.names=F)


# Load and format the forecast ensembles data
# 1. Load data
# 2. turn to daily
# 3. calculate indices
# 4. flatten and save

write.csv(dat.forecast, file.path(path.out, "data", "Weather_Arb_forecast_ensemble_latest.csv"), row.names=F)
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
