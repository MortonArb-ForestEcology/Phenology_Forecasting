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

dir.met <- "../data_raw/meteorology"
dir.create(dir.met, recursive=T, showWarnings = F)
# ---------------------------------


# ---------------------------------
# 1. NOAA COOP Station data
# Use FedData Package to download station data and put it here
# ---------------------------------
ID="USC00115097"
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
# test <- FedData::download_ghcn_daily_station(ID=ID, raw.dir=dir.met)
dat.raw <- FedData::get_ghcn_daily_station(ID=ID, raw.dir=dir.met, force.redo = T)

# Finding an issue where not all variables may line up (weird; but we can deal)
dat.ghcn <- data.frame(STATION=ID)
for(MET in vars.want){
  dat.var <- data.frame(YEAR=dat.raw[[MET]]$YEAR, 
                        MONTH=dat.raw[[MET]]$MONTH,
                        DAY=rep(1:31, each=nrow(dat.raw[[MET]])))
  dat.var[,MET] <- stack(dat.raw[[MET]][grep("D", names(dat.raw[[MET]]))])[,1]
  
  dat.ghcn <- merge(dat.ghcn, dat.var, all=T)
}
summary(dat.ghcn)

dat.ghcn[,c("TMAX", "TMIN", "PRCP")] <- dat.ghcn[,c("TMAX", "TMIN", "PRCP")]*0.1 # Unit correction
dat.ghcn$DATE <- as.Date(paste(dat.ghcn$YEAR, dat.ghcn$MONTH, dat.ghcn$DAY, sep="-"))
dat.ghcn <- dat.ghcn[!is.na(dat.ghcn$DATE),]
dat.ghcn <- dat.ghcn[dat.ghcn$DATE < Sys.Date(),] # Get rid of anything in the future
dat.ghcn$YDAY <- lubridate::yday(dat.ghcn$DATE)
dat.ghcn <- dat.ghcn[order(dat.ghcn$DATE),] # Ordering just to make life easier
# dat.ghcn[,]
summary(dat.ghcn)
tail(dat.ghcn)

# -------------------------------------
# Missing days are going to be a pain in the butt, so lets do dumb gap-filling for now. 
#  - For temperature, linearly interpolate
#  - For precipitation, just assume no rain
#  ** Make sure to add column about being gapfilled
# -------------------------------------
source("met_gapfill.R")
dat.ghcn$flag.TMAX <- as.factor(ifelse(is.na(dat.ghcn$TMAX), "gapfill", "observed"))
dat.ghcn$flag.TMIN <- as.factor(ifelse(is.na(dat.ghcn$TMIN), "gapfill", "observed"))
dat.ghcn$flag.PRCP <- as.factor(ifelse(is.na(dat.ghcn$PRCP), "gapfill", "observed"))
dat.ghcn$flag.SNOW <- as.factor(ifelse(is.na(dat.ghcn$SNOW), "gapfill", "observed"))
dat.ghcn$flag.SNWD <- as.factor(ifelse(is.na(dat.ghcn$SNWD), "gapfill", "observed"))

# Assuming missing precip (& snow) is 0
dat.ghcn$PRCP[dat.ghcn$flag.PRCP=="gapfill"] <- 0
dat.ghcn$SNOW[dat.ghcn$flag.SNOW=="gapfill"] <- 0
summary(dat.ghcn$flag.PRCP)

# Initial (& last) snow depth is missing, so lets assume 0
if(is.na(dat.ghcn$SNWD[1])) dat.ghcn$SNWD[1] <- 0
if(is.na(dat.ghcn$SNWD[nrow(dat.ghcn)])) dat.ghcn$SNWD[nrow(dat.ghcn)] <- 0

dat.ghcn$TMAX <- met.gapfill(met.data = dat.ghcn, met.var="TMAX")
dat.ghcn$TMIN <- met.gapfill(met.data = dat.ghcn, met.var="TMIN")
dat.ghcn$SNWD <- met.gapfill(met.data = dat.ghcn, met.var="SNWD")
summary(dat.ghcn)

dat.ghcn$TMEAN <- apply(dat.ghcn[,c("TMAX", "TMIN")], 1, mean)
summary(dat.ghcn)

# -------------------------------------



# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
yr.min <- 2008
yr.max <- lubridate::year(Sys.Date())
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  dat.yr <- dat.ghcn[rows.yr,]
  # dat.yr$TMEAN <- (dat.yr$TMAX + dat.yr$TMIN)/2
  # tmean.tmp
  
  dat.yr$GDD0 <- ifelse(dat.yr$TMEAN>0, dat.yr$TMEAN-0, 0)
  dat.yr$GDD5 <- ifelse(dat.yr$TMEAN>5, dat.yr$TMEAN-5, 0)
  dat.yr$CDD0 <- ifelse(dat.yr$TMEAN<0, 0-dat.yr$TMEAN, 0)
  
  dat.yr[1, c("GDD0.cum", "GDD5.cum", "CDD0.cum")] <- dat.yr[1, c("GDD0", "GDD5", "CDD0")]
  dat.yr[1  , c("NORAIN.cum", "NORAIN.spell")] <- ifelse(dat.yr[1,"PRCP"]>0, 0, 1)
  for(i in 2:nrow(dat.yr)){
    # PICK UP HERE
  }
}
# -------------------------------------

ggplot(data=dat.ghcn, aes(x=YDAY, y=TMAX, group=YEAR)) +
  geom_line(alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], col="blue2")

ggplot(data=dat.ghcn, aes(x=YDAY, y=TMIN, group=YEAR)) +
  geom_line(alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], col="blue2")

ggplot(data=dat.ghcn, aes(x=YDAY, y=PRCP, group=YEAR)) +
  geom_line(alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], col="blue2")

ggplot(data=dat.ghcn, aes(x=YDAY, y=SNWD, group=YEAR)) +
  geom_line(alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], col="blue2")



# ---------------------------------

# ---------------------------------
# 2. Forecast Data 
# ---------------------------------
# ---------------------------------
