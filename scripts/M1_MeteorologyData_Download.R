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

source("met_download_GHCN.R"); source("met_gapfill.R")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")
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
# dat.ghcn2[dat.ghcn2$DATE=="2020-04-09",]

write.csv(dat.ghcn2, file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"), row.names=F)

# Load and format the forecast ensembles data
# 1. Load data -- need columns of YDAY, TMAX, TMIN, PRCP
# 2. turn to daily
# 3. calculate indices
# 4. flatten and save

# Read in GHCN as training data for bias-correction
dat.ghcn2 <- read.csv(file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"))
dat.ghcn2$DATE <- as.Date(dat.ghcn2$DATE)

path.met <- "../data_raw/meteorology"
forecast.gcm <- c("CCSM4", "CanCM4", "CFSV2")
met.ens <- list()
for(GCM in forecast.gcm){
  path.gcm <- ifelse(GCM=="CFSV2", "CFS_Forecast", GCM)
  var.pcp <- ifelse(GCM=="CFSV2", "prate", "precip")
  
  if(GCM=="CFSV2"){
    mems=NA
  } else {
    mems <- dir(file.path("../data_raw/meteorology/", GCM))
  }
  
    for(i in 1:length(mems)){
      if(length(mems)>1){
        ens <- strsplit(mems[i], "_")[[1]][3]
        path.mem <-file.path(path.met, path.gcm, mems[i])
      } else {
        ens="01"
        path.mem <-file.path(path.met, path.gcm)
      }
      fls <- dir(path.mem)
      
      dat.tmx <- read.csv(file.path(path.mem, fls[grep("tmax", fls)]))
      dat.tmn <- read.csv(file.path(path.mem, fls[grep("tmin", fls)]))
      dat.prp <- read.csv(file.path(path.mem, fls[grep(var.pcp, fls)]))
      
      # Different models label things differently, but data should always be the last column
      dat.tmp <- data.frame(DATE=as.Date(substr(dat.tmx$time, 1, 10)),
                            HOUR=format(strptime(substr(dat.tmx$time, 12, 19), format="%T"), "%H"),
                            TMAX=dat.tmx[,ncol(dat.tmx)],
                            TMIN=dat.tmn[,ncol(dat.tmn)])
      if(nrow(dat.prp)==nrow(dat.tmx)){
        dat.tmp$PRCP <- dat.prp[,ncol(dat.prp)]
      } else {
        dat.prp$PRCP <- dat.prp[,ncol(dat.prp)]
        dat.prp$DATE <- as.Date(substr(dat.prp$time, 1, 10))
        dat.prp$HOUR <- format(strptime(substr(dat.prp$time, 12, 19), format="%T"), "%H")
        
        dat.tmp <- merge(dat.tmp, dat.prp[,c("DATE", "HOUR", "PRCP")], all=T)
        dat.tmp$PRCP[is.na(dat.tmp$PRCP)] <- -9999
      }
      dat.tmp$YDAY <- lubridate::yday(dat.tmp$DATE)
      summary(dat.tmp)
      
      # Aggregate to daily values; note: PRCP will require unit conversion
      dat.mod <- aggregate(TMAX ~ DATE + YDAY, data=dat.tmp, FUN=max)
      dat.mod$TMIN <- aggregate(TMIN ~ DATE + YDAY, data=dat.tmp, FUN=min)$TMIN
      dat.mod$PRCP <- aggregate(PRCP ~ DATE + YDAY, data=dat.tmp, FUN=sum)$PRCP
      dat.mod$PRCP[dat.mod$PRCP==-9999] <- NA
      
      # Add meta vars; do unit conversion
      dat.mod$MODEL <- as.factor(GCM)
      dat.mod$ENS <- as.factor(ens)
      dat.mod[,c("TMAX", "TMIN")] <- dat.mod[,c("TMAX", "TMIN")]-273.15
      dat.mod$PRCP <- dat.mod$PRCP*60*60*24
      summary(dat.mod)  
      
      # Precip has some wonky things going on
      if(GCM=="CCSM4") dat.mod$PRCP <- dat.mod$PRCP*1e3
      dat.mod$PRCP <- dat.mod$PRCP*10 # Something seems off
      
      # ----------------------
      # Bias-correct our variables using all of the GHCN data
      # ----------------------
      # for temperature, just correct the seasonal cycle & climatology; 
      #   leave the anomalies alone
      for(VAR in c("TMAX", "TMIN")){
        ghcn.tmp <- data.frame(VAR=dat.ghcn2[dat.ghcn2$YDAY %in% dat.mod$YDAY,VAR],
                               YDAY=dat.ghcn2[dat.ghcn2$YDAY %in% dat.mod$YDAY, "YDAY"])
        
        mod.tmp <- data.frame(VAR=dat.mod[,VAR],
                              YDAY=dat.mod$YDAY)
        
        # Set our spline based on our forecst length;
        # For other work I use 6 knots for a year, so lets 
        # use similar scaling so it's not over-flexible
        k.use <- max(round(nrow(dat.mod)/60), 2)
        
        mod.ghcn <- mgcv::gam(VAR ~ s(YDAY, k=k.use), data=ghcn.tmp)
        mod.gcm <- mgcv::gam(VAR ~ s(YDAY, k=k.use), data=mod.tmp)
        
        # Overwrite TMAX with the new bias-corrected TMAX 
        #   as the GCHN seasonal cycle plus our observed 
        #   residuals from the GCM trend
        dat.mod[,VAR] <- predict(mod.ghcn, newdata = mod.tmp) + resid(mod.gcm)
      }
      
      # For Precip, we need to adjust the variance we see
      rain.ghcn <- dat.ghcn2$PRCP[dat.ghcn2$PRCP>0 & dat.ghcn2$YDAY %in% dat.mod$YDAY]
      rain.gcm <- dat.mod$PRCP[dat.mod$PRCP>min(rain.ghcn) & !is.na(dat.mod$PRCP)]
      # Do a median-based bias-correction
      rain.adj <- median(rain.ghcn)/median(rain.gcm)
      
      dat.mod$PRCP <- dat.mod$PRCP*rain.adj
      # ----------------------
      

      # Truncate our forecast to just the range we need  & then add the 
      # observed so we can 
      dat.mod <- dat.mod[dat.mod$DATE>max(dat.ghcn2$DATE),]
      dat.mod$ID <- paste(GCM, ens, sep="-")
      dat.mod$TYPE <- as.factor("forecast")
      
      dat.fill <- data.frame(ID=paste(GCM, ens, sep="-"),
                             MODEL=GCM, 
                             ENS=ens, 
                             TYPE="observed",
                             dat.ghcn2[lubridate::year(dat.ghcn2$DATE)==min(lubridate::year(dat.mod$DATE)), c("DATE", "YDAY", "PRCP", "TMAX", "TMIN")])
      
      # CCSM4 has weird precip units --> orders of magnitude off; 
      #  -- MUST be m precip/m/s rather than kg/m2/s 
      
      met.ens[[paste(GCM, ens, sep="-")]] <- rbind(dat.fill, dat.mod[,c("ID", "MODEL", "ENS", "TYPE", "DATE", "YDAY", "PRCP", "TMAX", "TMIN")])
      
    }
}
summary(met.ens$`CCSM4-02`[met.ens$`CCSM4-02`$TYPE=="forecast",])
# summary(dat.ghcn2$PRCP)

# Calculate our indices using 
met.ens2 <- lapply(met.ens, calc.indices)
summary(met.ens2[[3]])
# plot(met.ens2[[5]]$PRCP.cum); abline(v=max(dat.ghcn2$YDAY[dat.ghcn2$YEAR==2020]), col="red")

# Turn our forecast dataset into a long format to make it easier to save
dat.forecast <- do.call(rbind, met.ens2)
summary(dat.forecast)

write.csv(dat.forecast, file.path(path.out, "data", "Weather_Arb_forecast_ensemble_latest.csv"), row.names=F)
# -------------------------------------



# ---------------------------------
