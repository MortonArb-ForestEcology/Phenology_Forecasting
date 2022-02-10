# --------------------------------------------------------------
# An updated test workflow for downloading the met driver data for phenology forecasts
# --------------------------------------------------------------
# Current Data Products:
#   1. Arboretum COOP station data (observed Temp & Precip)
#   2. NOAA CFS forecast -- single ensemble member, but long-range forecast (9 months)
#   3. NOAA GEFS Forecast -- 31-member forecast, but short-term (16 day) with 
#       - potential slow option for longer-range (35 days), but relies on grib format :-( 
#   X. Once upon a time the North American Meso-Scale Ensemble was working, but currently broken
# What needs to happen
#   1. Download all datasets
#       - For forecast products, keep past 7 days overlapping with Arb observations
#   2. Format forecast products to bring into daily res
#   2. Bias-correct forecast products to better match Arb meteorology; 2 stages
#       Option 1: Simple means correction lm(Arb ~ Forecast)
#       Option 2: Formal Data Assimilation with uncertainty propagation
#       Stage 1: Correct all(? just GEFS?) products with Arb; one week overlap?
#       Stage 2: Correct CFS long-range to match GEFS forecast for period of overlap
#         - make sure to propagate uncertainty here
#   3. Splice products to continuous datasets
#   4. Calculate met indices
#   5. Save met driver ensemble 
# --------------------------------------------------------------

# -------------------------------------------------
# 0. Setup
# -------------------------------------------------
#path.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting"
path.out <- "data_raw/meteorology/data"
path.shiny <- "shiny_app/data_raw/meteorology/"
if(!dir.exists(path.shiny)) dir.create(path.shiny, recursive=T, showWarnings = F)


dir.met <- "data_raw/meteorology"
if(!dir.exists(dir.met)) dir.create(dir.met, recursive=T, showWarnings = F)

path.ghcn=c("data_raw/meteorology/GHCN_extracted/")
out.cfs="data_raw/meteorology/CFS_Forecast"
out.gefs <- "data_raw/meteorology/GEFS"

# Params for extracting NOAA products
site.name="MortonArb"
lat.in=41.812739
lon.in=-88.072749

# -------------------------------------------------

# -------------------------------------------------
# 1. Download and save all raw datasets
# -------------------------------------------------
# ---------------------------------
# 1.1. NOAA COOP Station data
# Use FedData Package to download station data and put it here
# ---------------------------------
ID="USC00115097"
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
dir.raw="data_raw/meteorology/GHCN_raw/"

source("../met_download_GHCN.R"); source("../met_gapfill.R")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")
# -------------------------------------

# ---------------------------------
# 1.2. Forecast Data 
# ---------------------------------
# ----------------
# A bit of setup to get the data
# ----------------
# Load GHCN data to get the window we need
# Load GHCN data to get the window we need
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
summary(met.ghcn)

# Set up some parameters for downloading the data
ghcn.overlap = max(met.ghcn$DATE)-7
forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")
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

yr.min <- 2008
yr.max <- lubridate::year(Sys.Date()) #I had to do this because of a discrepancy over the holdays where it hasn't updated yet
met.ghcn2 <- data.frame()
for(YR in yr.min:yr.max){
  rows.yr <- which(met.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]
  dat.tmp <- calc.indices(dat=met.ghcn[rows.yr,])
  met.ghcn2 <- rbind(met.ghcn2, dat.tmp)
}
summary(met.ghcn2)
# dat.ghcn2[dat.ghcn2$DATE=="2020-04-09",]

#This is where the "observed" data comes from
write.csv(met.ghcn2, file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"), row.names=F)
# ----------------

# ----------------
# 1.2.1. CFS data (1 ensemble member, but long-range) 
# Note: CFS is what Shawn Taylor used in his Ecol App Pub
# -- he downloaded the 5 most recent forecasts to get uncertainty
# ----------------
source("../met_download_CFS.R")
vars.in <- c("tmax", "tmin", "prate")

cfs.dates <- as.Date(dir(file.path(out.cfs, site.name)))

if(!ghcn.overlap %in% cfs.dates){
  cfs.start <- max(ghcn.overlap, max(cfs.dates+1, Sys.Date()-3)) # Can't go back a full week apparently :-/
} else {
  cfs.start <- min(max(cfs.dates)+1, Sys.Date())
}

dates.cfs <- seq.Date(cfs.start, Sys.Date(), by=1)
# Loop through the dates as needed --> beyond first time this shouldn't be needed, but 
#  if there's a hiccup, it'll have to start over
dates.cfs <- as.character(dates.cfs)

for(FCST in dates.cfs){
  # print(FCST)
  dir.create(file.path(out.cfs, site.name, FCST), recursive=T, showWarnings =F)
  download.cfs(vars.in=vars.in, lat.in=lat.in, lon.in=lon.in, forecast.start=as.Date(FCST), forecast.end=forecast.end, path.save=file.path(out.cfs, site.name, FCST))
}
# ----------------

# ----------------
# 1.2.2. GEFS data (31 ensemble members, but shorter-range)
# Note: GEFS is what the EFI forecasting challenge is using
#   - Using Quinn's temporal downscaling to get better Tmin/Tmax estimates
# ----------------
# For GEFS, Quinn has it set up to download a window prior to today w/ each in a separate window; we want to do that only if needed to

# Note: If we get the longer-range forecast extraction working, might update this
gefs.dates <- as.Date(dir(file.path(out.gefs, "NOAAGEFS_1hr", site.name)))

# For our start date, we need to go back to our 7-day overlap if we need to, otherwise we want to start with the day after the latest GEFS forecast
# Note: tried setting this up as an ifelse() statement and had format trouble, so separated it out
if(!ghcn.overlap %in% gefs.dates){
  gefs.start <- max(ghcn.overlap, max(gefs.dates+1, Sys.Date()-3)) # Can't go back a full week apparently :-/
} else {
  gefs.start <- min(max(gefs.dates)+1, Sys.Date())
}

dates.gefs <- seq.Date(gefs.start, Sys.Date(), by=1)
# Loop through the dates as needed --> beyond first time this shouldn't be needed, but 
#  if there's a hiccup, it'll have to start over
dates.gefs <- as.character(dates.gefs)

for(FCST in dates.gefs){
  noaaGEFSpoint::noaa_gefs_download_downscale(site_list = site.name,
                                              lat_list = lat.in,
                                              lon_list= lon.in,
                                              output_directory = out.gefs,
                                              forecast_time = "00",
                                              forecast_date = FCST,
                                              downscale = TRUE,
                                              run_parallel = FALSE,
                                              num_cores = 1,
                                              method = "point",
                                              overwrite = TRUE)
  
}
# ----------------
# -------------------------------------------------

# -------------------------------------------------
# 2. Format raw datasets
# -------------------------------------------------
# ----------------
# Base Data: GHCN
# ----------------
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
summary(met.ghcn)

# Window for dates we want to overlap if possible
ghcn.overlap = max(met.ghcn$DATE)-7
# ----------------

# ----------------
# Get the GEFS files we want
# ----------------
gefs.dates <- as.Date(dir(file.path(out.gefs, "NOAAGEFS_1hr", site.name)))
gefs.dates <- rev(as.character(gefs.dates[gefs.dates>=ghcn.overlap])) # Reversed to go newest to oldest

# To get the lead-in and gap-fill, we'll want the first 24 hrs of each forecast (the best-guess) except for the most recent, where we want all time
ftoday <- dir(file.path(out.gefs, "NOAAGEFS_1hr", site.name, gefs.dates[1], "00"))

ncT <- ncdf4::nc_open(file.path(out.gefs, "NOAAGEFS_1hr",site.name, gefs.dates[1], "00", ftoday[1]))
summary(ncT$var)
nval <- length(ncT$dim$time$vals)
ncdf4::nc_close(ncT)

gefs.1hr <- data.frame(Timestamp=seq.POSIXt(as.POSIXct(paste(gefs.dates[1], "00:00")), by="1 hour", length.out=nval), ens=rep(1:length(ftoday), each=nval), tair=NA, prcp=NA)
gefs.1hr$Date <- as.Date(substr(gefs.1hr$Timestamp, 1, 10))

for(i in 1:length(ftoday)){
  row.ind <- which(gefs.1hr$ens==i)
  
  ncT <- ncdf4::nc_open(file.path(out.gefs, "NOAAGEFS_1hr", site.name, Sys.Date(), "00", ftoday[i]))
  gefs.1hr$tair[row.ind] <- ncdf4::ncvar_get(ncT, "air_temperature")
  gefs.1hr$prcp[row.ind] <- ncdf4::ncvar_get(ncT, "precipitation_flux")
  
  ncdf4::nc_close(ncT)
}
# the first value for all precip is NA, so let just make it 0 for our snaity
gefs.1hr$prcp[is.na(gefs.1hr$prcp)] <- 0
summary(gefs.1hr)

# Now Loop through the other days; pulling just the first 24 hrs for each date
nval=24
for(i in 2:length(gefs.dates)){
  ftoday <- dir(file.path(out.gefs, "NOAAGEFS_1hr", site.name, gefs.dates[i], "00"))
  
  df.tmp <- data.frame(Timestamp=seq.POSIXt(as.POSIXct(paste(gefs.dates[i], "00:00")), by="1 hour", length.out=nval), ens=rep(1:length(ftoday), each=nval), tair=NA, prcp=NA)
  df.tmp$Date <- as.Date(substr(df.tmp$Timestamp, 1, 10))
  
  for(j in 1:length(ftoday)){
    row.ind <- which(df.tmp$ens==j)
    
    ncT <- ncdf4::nc_open(file.path(out.gefs, "NOAAGEFS_1hr", site.name, gefs.dates[i], "00", ftoday[j]))
    df.tmp$tair[row.ind] <- ncdf4::ncvar_get(ncT, "air_temperature", count=c(24,1,1))
    df.tmp$prcp[row.ind] <- ncdf4::ncvar_get(ncT, "precipitation_flux", count=c(24,1,1))
    ncdf4::nc_close(ncT)
  }
  # the first value for all precip is NA, so let just make it 0 for our snaity
  df.tmp$prcp[is.na(df.tmp$prcp)] <- 0
  summary(df.tmp)
  
  gefs.1hr <- rbind(df.tmp, gefs.1hr)
  
  rm(df.tmp)
}
summary(gefs.1hr)
# head(df.1hr)
# 
# ggplot(data=df.1hr) +
#   geom_line(aes(x=Timestamp, y=tair, group=ens))

# Convert into daily max/mins
gefs.day <- aggregate(cbind(tair, prcp) ~ Date + ens, data=gefs.1hr, FUN=mean)
gefs.day$tmax <- aggregate(tair ~ Date + ens, data=gefs.1hr, FUN=max)$tair
gefs.day$tmin <- aggregate(tair ~ Date + ens, data=gefs.1hr, FUN=min)$tair
gefs.day$prcp.day <- gefs.day$prcp*60*60*24 # should convert to mm/day 
summary(gefs.day)

# ggplot(data=gefs.day) +
#   geom_line(aes(x=Date, y=tair, group=ens))
write.csv(gefs.day, file.path(out.gefs, paste0(site.name, "_GEFS_daily_latest.csv")), row.names=F)
# ----------------


# ----------------
# Get the CFS files we want
# ----------------
cfs.dates <- as.Date(dir(file.path(out.cfs, site.name)))
cfs.dates <- rev(as.character(cfs.dates[cfs.dates>=ghcn.overlap]))

cfs.tmx <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], "tmax_cfs_latest.csv"))
cfs.tmn <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], "tmin_cfs_latest.csv"))
cfs.prp <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], "prate_cfs_latest.csv"))

# We'll just take the first 4 rows of each file
for(i in 2:length(cfs.dates)){
  tmx.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], "tmax_cfs_latest.csv"))
  tmn.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], "tmin_cfs_latest.csv"))
  prp.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], "prate_cfs_latest.csv"))
  cfs.tmx <- rbind(tmx.tmp[substr(tmx.tmp$time, 1, 10)==cfs.dates[i],], cfs.tmx)
  cfs.tmn <- rbind(tmn.tmp[substr(tmn.tmp$time, 1, 10)==cfs.dates[i],], cfs.tmn)
  cfs.prp <- rbind(prp.tmp[substr(prp.tmp$time, 1, 10)==cfs.dates[i],], cfs.prp)
}
head(cfs.tmx)

#Subsetting because the precipitation has a longer forecast
cfs.prp <- cfs.prp[cfs.prp$time <= max(cfs.tmn$time), ]

dat.cfs <- data.frame(Timestamp=cfs.tmx$time, Date=as.Date(substr(cfs.tmx$time, 1, 10)), 
                      tmax=cfs.tmx$Maximum_temperature_height_above_ground.unit.K., 
                      tmin=cfs.tmn$Minimum_temperature_height_above_ground.unit.K.,
                      prcp=cfs.prp$Precipitation_rate_surface.unit.kg.m.2.s.1.)
dat.cfs$tair <- apply(dat.cfs[,c("tmax", "tmin")], 1, FUN=mean)
summary(dat.cfs)

cfs.day <- aggregate(cbind(tair, prcp) ~ Date, data=dat.cfs, FUN=mean)
cfs.day$tmax <- aggregate(tmax ~ Date, data=dat.cfs, FUN=max)$tmax
cfs.day$tmin <- aggregate(tmin ~ Date, data=dat.cfs, FUN=min)$tmin
cfs.day$prcp.day <- cfs.day$prcp*60*60*24 # should convert to mm/day 
summary(cfs.day)

write.csv(cfs.day, file.path(out.cfs, paste0(site.name, "_CFS_daily_latest.csv")), row.names=F)
# ----------------
# -------------------------------------------------

# -------------------------------------------------
# 3. Bias-correct datasets & Calculate Indices
# -------------------------------------------------
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

# ----------------
# Base Data: GHCN
# ----------------
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
met.ghcn$TYPE <- "observed"
summary(met.ghcn)
# ----------------

# ----------------
# CFS -- lets start simple
# ----------------
met.cfs <- read.csv(file.path(out.cfs, paste0(site.name, "_CFS_daily_latest.csv")))
met.cfs[,c("tair", "tmax", "tmin")] <- met.cfs[,c("tair", "tmax", "tmin")]-273.15
met.cfs$Date <- as.Date(met.cfs$Date)
summary(met.cfs)

# Loading the data we have to evaluate the forecast
cfs.comp <- met.cfs[met.cfs$Date %in% met.ghcn$DATE,]
cfs.comp[,c("GHCN.tmax", "GHCN.tmin", "GHCN.PRCP")] <- met.ghcn[met.ghcn$DATE %in% cfs.comp$Date, c("TMAX", "TMIN", "PRCP")]

# Set up a bias-corrected data frame
bc.cfs <- data.frame(TYPE="forecast", DATE=met.cfs$Date, YDAY=lubridate::yday(met.cfs$Date))
summary(bc.cfs)
# If there's only a tiny bit of data, just use the mean off-sets

#LUCIEN CHANGED THIS TO FORCE THE MEAN OFFSET TEMPORARILY
#BECAUSE I M DOING THIS IN JANUARY THE BIAS CORRECTION CAUSES ISSUES
#IT COULD BE FOR OTHER REASONS BUT I DON"T KNOW ENOUGH I WIL DISCUSS THIS SOON
#if(nrow(cfs.comp)<=3){ #This is the standard
if(nrow(cfs.comp) != 0){
  if(nrow(cfs.comp)<=14){
  
    bc.cfs$TMAX <- met.cfs$tmax + mean(cfs.comp$GHCN.tmax-cfs.comp$tmax)
    bc.cfs$TMIN <- met.cfs$tmin + mean(cfs.comp$GHCN.tmin-cfs.comp$tmin)
    
    if(all(cfs.comp[,c("prcp.day")]==0) | all(cfs.comp[,c("GHCN.PRCP")]==0)){
      bc.cfs$PRCP <- met.cfs$prcp.day
    } else {
      row.rain <- which(cfs.comp$prcp.day>0)
      bc.cfs$PRCP <- met.cfs$prcp.day*mean(cfs.comp$GHCN.PRCP[row.rain]/cfs.comp$prcp.day[row.rain])
    }
    
  } else {
    mod.tmax <- lm(GHCN.tmax ~ tmax, data=cfs.comp)
    mod.tmin <- lm(GHCN.tmin ~ tmin, data=cfs.comp)
    mod.prcp <- lm(GHCN.PRCP ~ prcp, data=cfs.comp)
    bc.cfs$TMAX <- predict(mod.tmax, newdata = met.cfs)
    bc.cfs$TMIN <- predict(mod.tmin, newdata = met.cfs)
    bc.cfs$PRCP <- predict(mod.prcp, newdata = met.cfs)
  } 
} else{
  bc.cfs$TMAX <- met.cfs$tmax
  bc.cfs$TMIN <- met.cfs$tmin
  bc.cfs$PRCP <- met.cfs$prcp.day
}
summary(bc.cfs)

dat.cfs <- rbind(met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TMAX", "TMIN", "PRCP")], bc.cfs)
dat.cfs$MODEL <- "CFS"
dat.cfs$ENS <- "CFS1"

summary(dat.cfs)

dat.cfs <- calc.indices(dat.cfs)

write.csv(dat.cfs, file.path(out.cfs, paste0(site.name, "_CFS_daily_FORECAST-READY.csv")))
# ggplot(data=dat.cfs) +
#   geom_line(aes(x=DATE, y=TMIN, color=TYPE))
# ----------------

# ----------------
# GEFS -- The short-range ensemble
# ----------------
met.gefs <- read.csv(file.path(out.gefs, paste0(site.name, "_GEFS_daily_latest.csv")))
met.gefs[,c("tair", "tmax", "tmin")] <- met.gefs[,c("tair", "tmax", "tmin")]-273.15
met.gefs$Date <- as.Date(met.gefs$Date)
summary(met.gefs)

# Loading the data we have to evaluate the forecast
gefs.comp <- met.gefs[met.gefs$Date %in% met.ghcn$DATE,]
gefs.comp[,c("GHCN.tmax", "GHCN.tmin", "GHCN.PRCP")] <- met.ghcn[met.ghcn$DATE %in% gefs.comp$Date, c("TMAX", "TMIN", "PRCP")]

# Set up a bias-corrected data frame
bc.gefs <- data.frame(TYPE="forecast", ENS=met.gefs$ens, DATE=met.gefs$Date, YDAY=lubridate::yday(met.gefs$Date))
summary(bc.gefs)

# THis is a stage that would be a natural fit for data assimilation
for(ENS in unique(met.gefs$ens)){
  ens.row <- which(met.gefs$ens==ENS)
  comp.row <- which(gefs.comp$ens==ENS)
  
  # If there's only a tiny bit of data, just use the mean off-sets
  #if(length(comp.row)<=3){#This is the standard
  if(is.null(comp.row)){
    if(length(comp.row)<=14){  
      bc.gefs$TMAX[ens.row] <- met.gefs$tmax[ens.row] + mean(gefs.comp$GHCN.tmax[comp.row]-gefs.comp$tmax[comp.row])
      bc.gefs$TMIN[ens.row] <- met.gefs$tmin[ens.row] + mean(gefs.comp$GHCN.tmin[comp.row]-gefs.comp$tmin[comp.row])
      
      if(all(gefs.comp[,c("prcp.day", "GHCN.PRCP")]==0)){
        bc.gefs$PRCP[ens.row] <- met.gefs$prcp.day[ens.row]
      } else {
        comp2 <- which(gefs.comp$ens==ENS & gefs.comp$prcp.day>0)
        bc.gefs$PRCP[ens.row] <- met.gefs$prcp.day[ens.row]*mean(gefs.comp$GHCN.PRCP[comp2]/gefs.comp$prcp.day[comp2])
      }
      
    } else {
      mod.tmax <- lm(GHCN.tmax ~ tmax, data=gefs.comp[comp.row,])
      mod.tmin <- lm(GHCN.tmin ~ tmin, data=gefs.comp[comp.row,])
      mod.prcp <- lm(GHCN.PRCP ~ prcp, data=gefs.comp[comp.row,])
      #Ok I added a comma to this bottom section to make it wokr but I hope I'm not changing functionality
      bc.gefs$TMAX[ens.row] <- predict(mod.tmax, newdata = met.gefs[ens.row,])
      bc.gefs$TMIN[ens.row] <- predict(mod.tmin, newdata = met.gefs[ens.row,])
      bc.gefs$PRCP[ens.row] <- predict(mod.prcp, newdata = met.gefs[ens.row,])
    }
  } else{
    bc.gefs$TMAX[ens.row] <- met.gefs$tmax[ens.row]
    bc.gefs$TMIN[ens.row] <- met.gefs$tmin[ens.row]
    bc.gefs$PRCP[ens.row] <- met.gefs$prcp.day[ens.row]
  }
}
summary(bc.gefs)

ghcn.ens <- merge(data.frame(ENS=unique(bc.gefs$ENS)), met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TMAX", "TMIN", "PRCP")], all=T)

dat.gefs <- rbind(ghcn.ens[,names(bc.gefs)], bc.gefs[bc.gefs$DATE>max(ghcn.ens$DATE),])
dat.gefs$MODEL <- "GEFS"
dat.gefs <- dat.gefs[order(dat.gefs$ENS, dat.gefs$DATE),]
head(dat.gefs)

gefs.indices <- data.frame()
for(ENS in unique(dat.gefs$ENS)){
  row.ens <- which(dat.gefs$ENS==ENS)
  gfs.tmp <- calc.indices(dat.gefs[row.ens,])
  
  gefs.indices <- rbind(gefs.indices, gfs.tmp)
}

summary(gefs.indices)

write.csv(gefs.indices, file.path(out.gefs, paste0(site.name, "_GEFS_daily_FORECAST-READY.csv")))
# ggplot(data=dat.gefs) +
#   geom_line(aes(x=DATE, y=TMAX, color=TYPE, group=ENS))
# ----------------


# ----------------
# Doing an extension of GFS with the CFS forecast
# ----------------
summary(bc.cfs)
summary(bc.gefs)

bc.gefs2 <- data.frame(TYPE="forecast", ENS=rep(unique(met.gefs$ens), each=nrow(bc.cfs)), DATE=bc.cfs$DATE, YDAY=bc.cfs$YDAY)
summary(bc.gefs2)

# This is a stage where we could shift to doing data assimliation
for(ENS in unique(bc.gefs2$ENS)){
  row.ens <- which(bc.gefs2$ENS==ENS & bc.gefs2$DATE>max(bc.gefs$DATE))
  row.cfs <- which(bc.cfs$DATE>max(bc.gefs$DATE))
  
  bc.gefs2[bc.gefs2$ENS==ENS & bc.gefs2$DATE %in% bc.gefs$DATE, c("TMAX", "TMIN", "PRCP")] <- bc.gefs[bc.gefs$ENS==ENS,c("TMAX", "TMIN", "PRCP")]
  # Set up the comparison data frame
  for.comp <- bc.cfs[bc.cfs$DATE %in% bc.gefs$DATE, ]
  
  # for.comp <- bc.gefs[bc.gefs$ENS==ENS,]
  for.comp[, c("GEFS.TMAX", "GEFS.TMIN", "GEFS.PRCP")] <- bc.gefs[bc.gefs$ENS==ENS & bc.gefs$DATE %in% for.comp$DATE, c("TMAX", "TMIN", "PRCP")]
  
  tmax.adj <- mean(for.comp$GEFS.TMAX-for.comp$TMAX)
  print(paste(ENS, "tmax.adj", tmax.adj ))
  bc.gefs2$TMAX[row.ens] <- bc.cfs$TMAX[row.cfs] + mean(for.comp$GEFS.TMAX-for.comp$TMAX)
  bc.gefs2$TMIN[row.ens] <- bc.cfs$TMIN[row.cfs] + mean(for.comp$GEFS.TMIN-for.comp$TMIN)
  
  if(all(for.comp[,c("PRCP", "GEFS.PRCP")]==0)){
    bc.gefs2$PRCP[row.ens] <- bc.cfs$prcp.day[row.cfs]
  } else {
    bc.gefs2$PRCP[row.ens] <- bc.cfs$PRCP[row.cfs]*mean(for.comp$GEFS.PRCP[for.comp$PRCP>0]/for.comp$PRCP[for.comp$PRCP>0])
  }
  
  # mod.tmax <- lm(GEFS.TMAX ~ TMAX, data=for.comp)
  # mod.tmin <- lm(GEFS.TMIN ~ TMIN, data=for.comp)
  # mod.prcp <- lm(GEFS.PRCP ~ PRCP, data=for.comp)
  # bc.gefs2$TMAX[ens.row] <- predict(mod.tmax, newdata = bc.cfs)
  # bc.gefs2$TMIN[ens.row] <- predict(mod.tmin, newdata = bc.cfs)
  # bc.gefs2$PRCP[ens.row] <- predict(mod.prcp, newdata = bc.cfs)
}
summary(bc.gefs2[,])


ghcn.ens <- merge(data.frame(ENS=unique(bc.gefs2$ENS)), met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TMAX", "TMIN", "PRCP")], all=T)

dat.gefs2 <- rbind(ghcn.ens[,names(bc.gefs)], bc.gefs2[bc.gefs2$DATE>max(ghcn.ens$DATE),])
dat.gefs2$MODEL[dat.gefs2$DATE %in% bc.gefs$DATE] <- "GEFS"
dat.gefs2$MODEL[dat.gefs2$DATE > max(bc.gefs$DATE)] <- "CFS-adj"
dat.gefs2$MODEL[dat.gefs2$DATE < min(bc.gefs$DATE)] <- "Observed"
dat.gefs2 <- dat.gefs2[order(dat.gefs2$ENS, dat.gefs2$DATE),]
head(dat.gefs2)

dat.gefs2 <- dat.gefs2[!is.na(dat.gefs2$MODEL),]
gefs.indices2 <- data.frame()
for(ENS in unique(dat.gefs2$ENS)){
  row.ens <- which(dat.gefs2$ENS==ENS)
  gfs.tmp2 <- calc.indices(dat.gefs2[row.ens,])
  
  gefs.indices2 <- rbind(gefs.indices2, gfs.tmp2)
}

summary(gefs.indices2)

check <- gefs.indices2[gefs.indices2$DATE == "2022-05-03",]

#This is the main forecast file we will be working with
write.csv(gefs.indices2, file.path(out.gefs, paste0(site.name, "_GEFS_daily_FORECAST-READY-LONGRANGE_latest.csv")), row.names = F)
#write.csv(gefs.indices2, file.path(paste0(dir.met,"/GEFS/","MortonArb_GEFS_daily_FORECAST-READY-LONGRANGE_" , Sys.Date() ,".csv")), row.names = F)

#Loading them into the shiny app
write.csv(gefs.indices2, file.path(paste0(path.shiny,"Previous-Forecast_", Sys.Date() ,".csv")), row.names = F)

