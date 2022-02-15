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
library(ggplot2)
# -------------------------------------------------
# 0. Setup
# -------------------------------------------------
path.shiny <- "MortonArb_PhenoForecast/data_raw/meteorology/"
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
ghcn.overlap = max(met.ghcn$DATE)-14
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
  cfs.start <- max(ghcn.overlap, max(cfs.dates+1, Sys.Date()-14)) # Can't go back a full week apparently :-/
} else {
  cfs.start <- min(max(cfs.dates)+1, Sys.Date())
}

if(cfs.start <= Sys.Date()){
  dates.cfs <- seq.Date(cfs.start, Sys.Date(), by=1)
  # Loop through the dates as needed --> beyond first time this shouldn't be needed, but 
  #  if there's a hiccup, it'll have to start over
  dates.cfs <- as.character(dates.cfs)
  
  for(FCST in dates.cfs){
    # print(FCST)
    path.save <- file.path(out.cfs, site.name, FCST)
    
    dir.create(path.save, recursive=T, showWarnings =F)
    download.cfs(vars.in=vars.in, lat.in=lat.in, lon.in=lon.in, forecast.start=as.Date(FCST), forecast.end=forecast.end, path.save=path.save)
    
    if(length(dir(path.save))==0) unlink(path.save, recursive=T)
  }
}
# if(length(dir(path.save))==0) unlink(path.save, recursive=T) # Just in case the last one didn't actually run
# ----------------

# ----------------
# 1.2.2. GEFS data (31 ensemble members, but shorter-range)
# Note: GEFS is what the EFI forecasting challenge is using
#   - Using Quinn's temporal downscaling to get better Tmin/Tmax estimates
# This package can be downloaded from here: https://github.com/rqthomas/noaaGEFSpoint
# ----------------
# For GEFS, Quinn has it set up to download a window prior to today w/ each in a separate window; we want to do that only if needed to

# Note: If we get the longer-range forecast extraction working, might update this
gefs.dates <- as.Date(dir(file.path(out.gefs, "NOAAGEFS_1hr", site.name)))

# For our start date, we need to go back to our 7-day overlap if we need to, otherwise we want to start with the day after the latest GEFS forecast
# Note: tried setting this up as an ifelse() statement and had format trouble, so separated it out
if(!ghcn.overlap %in% gefs.dates){
  gefs.start <- max(ghcn.overlap, max(gefs.dates+1, Sys.Date()-7)) # Can't go back a full week apparently :-/
} else {
  gefs.start <- min(max(gefs.dates)+1, Sys.Date())
}

# Make sure we're not trying to get tomorrow's forecast today!
if(gefs.start <= Sys.Date()){
  dates.gefs <- seq.Date(gefs.start, Sys.Date(), by=1)
  dates.gefs <- dates.gefs[!paste(dates.gefs) %in% paste(gefs.dates)] # Get rid of dates we already have
  
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
                                                overwrite = FALSE)
    
  }
}
# ----------------
# -------------------------------------------------

# -------------------------------------------------
# 2. Format raw datasets -- put things into a single latest file
# In here we set a target window for overlap for our data fitting; ideally it will be 2 weeks
# -------------------------------------------------
# ----------------
# Base Data: GHCN
# ----------------
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
summary(met.ghcn)
# ----------------

# ----------------
# Get the GEFS files we want
# NOTE: If the GHCN data hasn't synced yet, fill the missing dates with GEFS as possible. 
#       Because GEFS gives us an ensemble, we don't otherwise need to use the past dates to get uncertainty
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
ggplot(data=gefs.1hr) +
  geom_line(aes(x=Timestamp, y=tair, group=ens))

# Convert into daily max/mins
gefs.day <- aggregate(cbind(tair, prcp) ~ Date + ens, data=gefs.1hr, FUN=mean)
gefs.day$tmax <- aggregate(tair ~ Date + ens, data=gefs.1hr, FUN=max)$tair
gefs.day$tmin <- aggregate(tair ~ Date + ens, data=gefs.1hr, FUN=min)$tair
gefs.day$prcp.day <- gefs.day$prcp*60*60*24 # should convert to mm/day 
summary(gefs.day)

ggplot(data=gefs.day) +
  geom_line(aes(x=Date, y=tair, group=ens))

# Rather than labelling just "latest", lets use the system date & then sort moving forward
write.csv(gefs.day, file.path(out.gefs, paste0(site.name, "_GEFS_daily_", Sys.Date(), "_latest.csv")), row.names=F)
# ----------------


# ----------------
# Get the CFS files we want
# Because the CFS forecast goes so long, the correlation with others is totally bonk
# To get some inherent CFS, lets copy Taylor & White and use the past 7-14 days. I think this will get us process uncertainty
# We can also use CFS to fill in any remaining missing days we need
# ----------------
cfs.dates <- as.Date(dir(file.path(out.cfs, site.name)))
cfs.dates <- rev(as.character(cfs.dates[cfs.dates>=ghcn.overlap])) # This should give us at 14 days as possible

cfs.tmx <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], paste0("tmax_cfs_",cfs.dates[1],"_latest.csv")))
cfs.tmn <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], paste0("tmin_cfs_",cfs.dates[1],"_latest.csv")))
cfs.prp <- read.csv(file.path(out.cfs, site.name, cfs.dates[1], paste0("prate_cfs_",cfs.dates[1],"_latest.csv")))
cfs.tmx$ens <- cfs.tmn$ens <- cfs.prp$ens <- 0

# We'll just take the first 4 rows of each file
for(i in 2:length(cfs.dates)){
  tmx.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], paste0("tmax_cfs_",cfs.dates[i],"_latest.csv")))
  tmn.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], paste0("tmin_cfs_",cfs.dates[i],"_latest.csv")))
  prp.tmp <- read.csv(file.path(out.cfs, site.name, cfs.dates[i], paste0("prate_cfs_",cfs.dates[i],"_latest.csv")))
  tmx.tmp$ens <- tmn.tmp$ens <- prp.tmp$ens <- 1-i # This will give us Ensembe numbers of days ago
  
  ggplot(data=cfs.tmx) +
    geom_line(aes(x=time, y=Maximum_temperature_height_above_ground.unit.K.), color="black", size=0.5) +
    geom_line(data=tmx.tmp, aes(x=time, y=Maximum_temperature_height_above_ground.unit.K.), color="red", size=0.5)
  
  cfs.tmx <- rbind(tmx.tmp, cfs.tmx)
  cfs.tmn <- rbind(tmn.tmp, cfs.tmn)
  cfs.prp <- rbind(prp.tmp, cfs.prp)
}
summary(as.Date(cfs.tmx$time))
head(cfs.tmx)


#Subsetting because the precipitation has a longer forecast
cfs.prp <- cfs.prp[cfs.prp$time <= max(cfs.tmn$time), ]

dat.cfs <- data.frame(Timestamp=as.POSIXct(strptime(cfs.tmx$time, format="%Y-%m-%dT%H:%M:%S")), 
                      Date=as.Date(substr(cfs.tmx$time, 1, 10)), 
                      ens=cfs.tmx$ens,
                      tmax=cfs.tmx$Maximum_temperature_height_above_ground.unit.K., 
                      tmin=cfs.tmn$Minimum_temperature_height_above_ground.unit.K.,
                      prcp=cfs.prp$Precipitation_rate_surface.unit.kg.m.2.s.1.)
dat.cfs$tair <- apply(dat.cfs[,c("tmax", "tmin")], 1, FUN=mean)
summary(dat.cfs)

cfs.day <- aggregate(cbind(tair, prcp) ~ Date + ens, data=dat.cfs, FUN=mean, na.rm=T)
cfs.day$tmax <- aggregate(tmax ~ Date + ens, data=dat.cfs, FUN=max)$tmax
cfs.day$tmin <- aggregate(tmin ~ Date + ens, data=dat.cfs, FUN=min)$tmin
cfs.day$prcp.day <- cfs.day$prcp*60*60*24 # should convert to mm/day 
summary(cfs.day)

ggplot(data=cfs.day[cfs.day$Date<=Sys.Date()+30,]) +
  geom_line(aes(x=Date, y=tair, group=ens))

write.csv(cfs.day, file.path(out.cfs, paste0(site.name, "_CFS_daily_",Sys.Date(),"_latest.csv")), row.names=F)
# ----------------
# -------------------------------------------------

# -------------------------------------------------
# 3. Bias-correct datasets & Calculate Indices
## CR Update: We want to propagate the uncertainty from overlap with the GEFS forecast now that we have it
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

# -------------------------------
# Load all Met datasets so we can have them and splice as needed
# -------------------------------
# ----------------
# Base Observed Data: GHCN for Morton Arb
# ----------------
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
met.ghcn$TYPE <- "observed"
met.ghcn$TAIR <- apply(met.ghcn[,c("TMAX", "TMIN")], 1, FUN=mean)
summary(met.ghcn)
# ----------------

# ----------------
# Short-Range Forecast: GEFS (16-days)
# ----------------
f.gefs <- dir(out.gefs, "_latest.csv")
met.gefs <- read.csv(file.path(out.gefs, f.gefs[length(f.gefs)]))
met.gefs[,c("tair", "tmax", "tmin")] <- met.gefs[,c("tair", "tmax", "tmin")]-273.15
met.gefs$Date <- as.Date(met.gefs$Date)
summary(met.gefs)
# ----------------


# ----------------
# Long-Range Forecast: CFS data (9 months; using last couple weeks/days for uncertainty)
# ----------------
f.cfs <- dir(out.cfs, "_latest.csv")
met.cfs <- read.csv(file.path(out.cfs, f.cfs[length(f.cfs)]))
met.cfs[,c("tair", "tmax", "tmin")] <- met.cfs[,c("tair", "tmax", "tmin")]-273.15
met.cfs$Date <- as.Date(met.cfs$Date)
summary(met.cfs)
# ----------------

# Plot the data for a check looking 30 days to the past and 30 days to the future
yr.now = lubridate::year(Sys.Date())
ggplot(data=met.ghcn[met.ghcn$YEAR==yr.now,]) +
  geom_line(data=met.cfs, aes(x=Date, y=tair, group=ens, color="CFS")) +
  geom_line(data=met.gefs, aes(x=Date, y=tair, group=ens, color="GEFS"), size=0.2) +
  geom_line(aes(x=DATE, y=TAIR, color="GHCN")) +
  scale_color_manual(values=c("GHCN"="black", "GEFS"="blue2", "CFS"="orange2"))+
  coord_cartesian(xlim=c(Sys.Date()-30, Sys.Date()+30))

# -------------------------------

# -------------------------------

# ----------------
# GEFS -- The short-range ensemble; 
#  NOTE: Tackle this first so we can propagate uncertainty to CFS
# ----------------
# Set up a bias-corrected data frame
bc.gefs <- data.frame(TYPE="forecast", ENS=met.gefs$ens, DATE=met.gefs$Date, YDAY=lubridate::yday(met.gefs$Date))
summary(bc.gefs)

# Loading the data we have to evaluate the forecast
gefs.overlap <- which(met.gefs$Date %in% met.ghcn$DATE)

# If we have some sort of overlap, do a bias correciton,
if(length(gefs.overlap)>0){ 
  gefs.comp <- met.gefs[gefs.overlap,]
  gefs.comp[,c("GHCN.tair", "GHCN.tmax", "GHCN.tmin", "GHCN.PRCP")] <- met.ghcn[met.ghcn$DATE %in% gefs.comp$Date, c("TAIR", "TMAX", "TMIN", "PRCP")]
  

  # THis is a stage that would be a natural fit for data assimilation
  for(ENS in unique(met.gefs$ens)){
    ens.row <- which(met.gefs$ens==ENS)
    comp.row <- which(gefs.comp$ens==ENS)
    
    # If there's only a tiny bit of data, just use the mean off-sets
    #if(length(comp.row)<=3){#This is the standard
    if(length(comp.row)<=14){  
      bc.gefs$TAIR[ens.row] <- met.gefs$tair[ens.row] + mean(gefs.comp$GHCN.tair[comp.row]-gefs.comp$tair[comp.row])
      bc.gefs$TMAX[ens.row] <- met.gefs$tmax[ens.row] + mean(gefs.comp$GHCN.tmax[comp.row]-gefs.comp$tmax[comp.row])
      bc.gefs$TMIN[ens.row] <- met.gefs$tmin[ens.row] + mean(gefs.comp$GHCN.tmin[comp.row]-gefs.comp$tmin[comp.row])
      
      if(all(gefs.comp[,c("prcp.day", "GHCN.PRCP")]==0)){
        bc.gefs$PRCP[ens.row] <- met.gefs$prcp.day[ens.row]
      } else {
        comp2 <- which(gefs.comp$ens==ENS & gefs.comp$prcp.day>0)
        bc.gefs$PRCP[ens.row] <- met.gefs$prcp.day[ens.row]*mean(gefs.comp$GHCN.PRCP[comp2]/gefs.comp$prcp.day[comp2])
      }
      
    } else {
      mod.tair <- lm(GHCN.tair ~ tair, data=gefs.comp[comp.row,])
      mod.tmax <- lm(GHCN.tmax ~ tmax, data=gefs.comp[comp.row,])
      mod.tmin <- lm(GHCN.tmin ~ tmin, data=gefs.comp[comp.row,])
      mod.prcp <- lm(GHCN.PRCP ~ prcp, data=gefs.comp[comp.row,])
      #Ok I added a comma to this bottom section to make it wokr but I hope I'm not changing functionality
      bc.gefs$TAIR[ens.row] <- predict(mod.tair, newdata = met.gefs[ens.row,])
      bc.gefs$TMAX[ens.row] <- predict(mod.tmax, newdata = met.gefs[ens.row,])
      bc.gefs$TMIN[ens.row] <- predict(mod.tmin, newdata = met.gefs[ens.row,])
      bc.gefs$PRCP[ens.row] <- predict(mod.prcp, newdata = met.gefs[ens.row,])
    }
  
  }
} else {
  # If no overlap, just put in the raw values
  bc.gefs$TAIR <- met.gefs$tair
  bc.gefs$TMAX <- met.gefs$tmax
  bc.gefs$TMIN <- met.gefs$tmin
  bc.gefs$PRCP <- met.gefs$prcp.day
}
summary(bc.gefs)

ghcn.ens.gefs <- merge(data.frame(ENS=unique(bc.gefs$ENS)), met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TAIR", "TMAX", "TMIN", "PRCP")], all=T)

dat.gefs <- rbind(ghcn.ens.gefs[,names(bc.gefs)], bc.gefs[bc.gefs$DATE>max(ghcn.ens.gefs$DATE),])
dat.gefs$MODEL <- "GEFS"
dat.gefs <- dat.gefs[order(dat.gefs$ENS, dat.gefs$DATE),]
head(dat.gefs)

# Calculate Growing degree-days etc?; Note: the input temp data carries over
gefs.indices <- data.frame()
for(ENS in unique(dat.gefs$ENS)){
  row.ens <- which(dat.gefs$ENS==ENS)
  gfs.tmp <- calc.indices(dat.gefs[row.ens,])

  gefs.indices <- rbind(gefs.indices, gfs.tmp)
}

summary(gefs.indices)
# 
ggplot(data=bc.gefs) +
  geom_line(aes(x=DATE, y=TAIR, color=TYPE, group=ENS))

write.csv(gefs.indices, file.path(out.gefs, paste0(site.name, "_GEFS_daily_FORECAST-READY_",Sys.Date(),".csv")))

# ----------------
# -------------------------------

# -------------------------------
# CFS -- Adjust based on GEFS so we can propagate uncertainty
#  UPDATE: We're now using a rolling window with CFS to get some long-range uncertianty in there. 
#          Looking at the exploratory figure up around line 403, I think we need to make sure to propagate the uncertainty from GEFS,
#          otherwise we might get our met uncertainty to collapse too much.
# ----------------
# Set up a bias-corrected data frame; 
# NOTE: starting simple and NOT adding additional uncertainty --> just doing the "best guess" adjustment from the correlation with GEFS
bc.cfs <- data.frame(TYPE="forecast", ENS.CFS=met.cfs$ens, DATE=met.cfs$Date, YDAY=lubridate::yday(met.cfs$Date))
summary(bc.cfs)

bc.cfs <- merge(bc.cfs, data.frame(ENS.GEFS=unique(bc.gefs$ENS)))
bc.cfs$ENS = paste(bc.cfs$ENS.CFS, bc.cfs$ENS.GEFS, sep=".")
summary(bc.cfs)

# Because we're working with propagating uncertainty, there shoudln't be any issues with overlap
# [NOT USING WITH ENSEMBLE FOR THE MOMENT] # NOTE: using the trailing 14-day window because adding in the initial part is just not enough uncertainty
dates.overlap <- unique(met.cfs$Date[which(met.cfs$Date %in% bc.gefs$DATE)])
# dates.overlap <- dates.overlap[length(dates.overlap):(length(dates.overlap)-14)]

cfs.overlap <- which(met.cfs$Date %in% dates.overlap)
cfs.comp <- met.cfs[cfs.overlap,]

gefs.sub <- bc.gefs[bc.gefs$DATE %in% dates.overlap,]
names(gefs.sub) <- car::recode(names(gefs.sub), "'DATE'='Date'; 'ENS'='ENS.GEFS'; 'TAIR'='GEFS.TAIR'; 'TMAX'='GEFS.TMAX'; 'TMIN'='GEFS.TMIN'; 'PRCP'='GEFS.PRCP'")
summary(gefs.sub)

cfs.comp <- merge(cfs.comp, gefs.sub)
dim(cfs.comp)
dim(gefs.sub)
summary(cfs.comp)

# -------------------------
# # Tried working with the correlations, but there generally IS NONE
# # So we're going to skip on down and use a mean adjustment instead
# -------------------------
for(ENS in unique(cfs.comp$ens)){
  dat.ens.c <- cfs.comp[cfs.comp$ens==ENS,]
  cfs.now <- met.cfs[met.cfs$ens==ENS,]
  
  for(ENSG in unique(dat.ens.c$ENS.GEFS)){
    dat.now <- dat.ens.c[dat.ens.c$ENS.GEFS==ENSG,]
    rows.out <- which(bc.cfs$ENS.CFS==ENS & bc.cfs$ENS.GEFS==ENSG)
    
    bc.cfs[rows.out, "TMAX"] <- cfs.now$tmax - mean(dat.now$tmax-dat.now$GEFS.TMAX)
    bc.cfs[rows.out, "TMIN"] <- cfs.now$tmin - mean(dat.now$tmin-dat.now$GEFS.TMIN)
    # bc.cfs[rows.out, "PRCP"] <- cfs.now$prcp.day - mean(dat.now$prcp.day-dat.now$GEFS.PRCP)
    
    # Precip is being a HUGE pain, so we'll just copy it in there
    bc.cfs[rows.out, "PRCP"] <- cfs.now$prcp.day
    # # Preciptiation needs to be adjusted differently so we don't get negative values! Do it by fraction
    # if(all(dat.now[,c("prcp.day")]<=0.1)){
    #   bc.cfs[rows.out, "PRCP"] <- cfs.now$prcp.day
    # } else {
    #   # bc.cfs[rows.out, "PRCP"] <- cfs.now$prcp.day * mean(dat.now$GEFS.PRCP[dat.now$prcp.day>0]/dat.now$prcp.day[dat.now$prcp.day>0])
    #   bc.cfs[rows.out, "PRCP"] <- cfs.now$prcp.day * mean(dat.now$GEFS.PRCP[dat.now$prcp.day>0.1])/mean(dat.now$prcp.day[dat.now$prcp.day>0.1])
    #   
    # }
    
  }
  
  
}
summary(bc.cfs)


ggplot(data=bc.cfs) +
  geom_line(aes(x=DATE, y=TMAX, color=TYPE, group=ENS)) +
  geom_line(data=met.cfs, aes(x=Date, y=tmax, group=ens), color="black", size=0.2) +
  geom_line(data=bc.gefs, aes(x=DATE, y=TMAX, group=ENS), color="blue", size=0.1)

# dat.gefs <- rbind(ghcn.ens[,names(bc.gefs)], bc.gefs[bc.gefs$DATE>max(ghcn.ens$DATE),])
# dat.gefs$MODEL <- "GEFS"
# dat.gefs <- dat.gefs[order(dat.gefs$ENS, dat.gefs$DATE),]
# head(dat.gefs)

ghcn.ens.cfs <- merge(data.frame(ENS=unique(bc.cfs$ENS)), met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TAIR", "TMAX", "TMIN", "PRCP")], all=T)

cols.use <- c("TYPE", "ENS", "DATE", "YDAY", "TMAX", "TMIN", "PRCP")
dat.cfs <- rbind(ghcn.ens.cfs[,cols.use], bc.cfs[bc.cfs$DATE>max(ghcn.ens.cfs$DATE),cols.use])
dat.cfs$MODEL <- "CFS"
dat.cfs <- dat.cfs[order(dat.cfs$ENS, dat.cfs$DATE),]
summary(dat.cfs)

cfs.indices <- data.frame()
for(ENS in unique(dat.cfs$ENS)){
  row.ens <- which(dat.cfs$ENS==ENS)
  cfs.tmp <- calc.indices(dat.cfs[row.ens,])
  
  cfs.indices <- rbind(cfs.indices, cfs.tmp)
}

write.csv(dat.cfs, file.path(out.cfs, paste0(site.name, "_CFS_daily_FORECAST-READY_",Sys.Date(),".csv")))
# ----------------

# Doing one more file that gets printed to the main weather forecast drivers
ghcn.ens.all <- merge(data.frame(ENS=unique(bc.cfs$ENS)), met.ghcn[met.ghcn$YEAR==lubridate::year(Sys.Date()), c("TYPE", "DATE", "YDAY", "TAIR", "TMAX", "TMIN", "PRCP")], all=T)
ghcn.ens.all$ENS.CFS <- unlist(lapply(strsplit(ghcn.ens.all$ENS, "[.]"), FUN=function(x) x[1]))
ghcn.ens.all$ENS.GEFS <- unlist(lapply(strsplit(ghcn.ens.all$ENS, "[.]"), FUN=function(x) x[2]))
head(ghcn.ens.all); tail(ghcn.ens.all)

names(bc.cfs)
cols.use <- c("TYPE", "ENS", "DATE", "YDAY", "TMAX", "TMIN", "PRCP", "ENS.GEFS", "ENS.CFS")
ghcn.cfs <- rbind(ghcn.ens.all[,cols.use], bc.cfs[bc.cfs$DATE>max(ghcn.ens.all$DATE),cols.use])
head(ghcn.cfs)

bc.gefs2 <- bc.gefs
names(bc.gefs2) <- car::recode(names(bc.gefs2), "'ENS'='ENS.GEFS'")
bc.gefs2 <- merge(data.frame(ENS.CFS=unique(ghcn.cfs$ENS.CFS)), bc.gefs2)
bc.gefs2$ENS <- paste(bc.gefs2$ENS.CFS, bc.gefs2$ENS.GEFS, sep=".")
dim(bc.gefs); dim(bc.gefs2)

forecast.final <- rbind(ghcn.cfs[!ghcn.cfs$DATE %in% bc.gefs2$DATE,], bc.gefs2[,cols.use])
forecast.final <- forecast.final[order(forecast.final$ENS, forecast.final$DATE),]

ggplot(data=forecast.final) +
  geom_line(aes(x=DATE, y=TMAX, color=TYPE, group=ENS), size=0.1) 

final.indices <- data.frame()
for(ENS in unique(forecast.final$ENS)){
  row.ens <- which(forecast.final$ENS==ENS)
  final.tmp <- calc.indices(forecast.final[row.ens,])
  
  final.indices <- rbind(final.indices, final.tmp)
}
# length(unique(final.indices$ENS))
write.csv(final.indices, file.path(dir.met, paste0(site.name, "_daily_FORECAST-READY-LONGRANGE_", Sys.Date(),".csv")), row.names = F)


print("Met Driver Workflow Complete!")
# -------------------------------

