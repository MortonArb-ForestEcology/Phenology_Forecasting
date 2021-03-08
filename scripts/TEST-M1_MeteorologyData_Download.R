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
library(ggplot2)

path.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting"

dir.met <- "../data_raw/meteorology"
if(!dir.exists(dir.met)) dir.create(dir.met, recursive=T, showWarnings = F)
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
path.ghcn=c("../data_raw/meteorology/GHCN_extracted/")
dir.raw="../data_raw/meteorology/GHCN_raw/"

source("met_download_GHCN.R"); source("met_gapfill.R")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")
# -------------------------------------

# ---------------------------------
# 1.2. Forecast Data 
# ---------------------------------
# ----------------
# A bit of setup to get the data
# ----------------
# Load GHCN data to get the window we need
met.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
met.ghcn$DATE <- as.Date(met.ghcn$DATE)
summary(met.ghcn)

# Set up some parameters for downloading the data
site.name="MortonArb"
lat.in=41.812739
lon.in=-88.072749
ghcn.overlap = max(met.ghcn$DATE)-7
forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")


# ----------------

# ----------------
# 1.2.1. CFS data (1 ensemble member, but long-range) 
# Note: CFS is what Shawn Taylor used in his Ecol App Pub
# -- he downloaded the 5 most recent forecasts to get uncertainty
# ----------------
source("met_download_CFS.R")
out.cfs="../data_raw/meteorology/CFS_Forecast"
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
  download.cfs(vars.in=vars.in, lat.in=lat.in, lon.in=lon.in, forecast.start=as.Date(cfs.start), forecast.end=forecast.end, path.save=file.path(out.cfs, site.name, FCST))
  
}
# ----------------

# ----------------
# 1.2.2. GEFS data (31 ensemble members, but shorter-range)
# Note: GEFS is what the EFI forecasting challenge is using
#   - Using Quinn's temporal downscaling to get better Tmin/Tmax estimates
# ----------------
# For GEFS, Quinn has it set up to download a window prior to today w/ each in a separate window; we want to do that only if needed to
out.gefs <- "../data_raw/meteorology/GEFS"

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
# 2. Bias-correct raw datasets
# -------------------------------------------------
# -------------------------------------------------
