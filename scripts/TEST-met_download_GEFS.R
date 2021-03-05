# Trying out Quinn's noaaGEFSpoint package to get forecast ensembles
# https://github.com/rqthomas/noaaGEFSpoint/
# devtools::install_github("rqthomas/noaaGEFSpoint")
 
outdir <- "../data_raw/meteorology/GEFS"
if(!dir.exists(outdir)) dir.create(outdir)

# -----------------------
# Try with Morton Arb
# -----------------------
site.name="MortonArb"
lat.in=41.812739
lon.in=-88.072749

# Try syntax from eco4cast challenge: https://github.com/eco4cast/neon4cast-noaa-download/blob/master/launch_download_downscale.R
# This works.  It's a little slow & only goes out 16 days, but it works.
noaaGEFSpoint::noaa_gefs_download_downscale(site_list = paste0(site.name, 3),
                                            lat_list = lat.in,
                                            lon_list= lon.in,
                                            output_directory = outdir,
                                            forecast_time = "00",
                                            forecast_date = Sys.Date(),
                                            downscale = TRUE,
                                            run_parallel = FALSE,
                                            num_cores = 1,
                                            method = "point",
                                            overwrite = FALSE)

# Loading all of the different ensembles
ftoday <- dir(file.path(outdir, "NOAAGEFS_1hr", paste0(site.name, 3), Sys.Date(), "00"))

ncT <- ncdf4::nc_open(file.path(outdir, "NOAAGEFS_1hr", paste0(site.name, 2), Sys.Date(), "00", ftoday[1]))
summary(ncT$var)
ncT$dim$time
nval <- length(ncT$dim$time$vals)
ncdf4::nc_close(ncT)

# Note: When I'm testing this, it gets wonky because of daylight savings. :face_palm:
df.1hr <- data.frame(Timestamp=seq.POSIXt(as.POSIXct(paste(Sys.Date(), "00:00")), by="1 hour", length.out=nval), ens=rep(1:length(ftoday), each=nval), tair=NA, prcp=NA)
df.1hr$Date <- as.Date(substr(df.1hr$Timestamp, 1, 10))
df.1hr[1:12,]

for(i in 1:length(ftoday)){
  row.ind <- which(df.6hr$ens==i)
  
  ncT <- ncdf4::nc_open(file.path(outdir, "NOAAGEFS_6hr", paste0(site.name, 2), Sys.Date(), "00", ftoday[i]))
  df.6hr$tair[row.ind] <- ncdf4::ncvar_get(ncT, "air_temperature")
  df.6hr$prcp[row.ind] <- ncdf4::ncvar_get(ncT, "precipitation_flux")
  
  ncdf4::nc_close(ncT)
}
# the first value for all precip is NA, so let just make it 0 for our snaity
df.6hr$prcp[is.na(df.6hr$prcp)] <- 0
summary(df.6hr)

met.day <- aggregate(cbind(tair, prcp) ~ Date + ens, data=df.1hr, FUN=mean)
met.day$tmax <- aggregate(tair ~ Date + ens, data=df.1hr, FUN=max)$tair
met.day$tmin <- aggregate(tair ~ Date + ens, data=df.1hr, FUN=min)$tair
met.day$prcp.day <- met.day$prcp*60*60*24/4 # should convert to mm/day
summary(met.day)

ggplot(data=met.day) +
  geom_line(aes(x=Date, y=(tair-273.15)*9/5+32, group=ens))
# -----------------------


# # This is SUPER slow because it's downloading ALL the data first BUT it would give us forecast out to 35 days in the future
# noaaGEFSpoint::noaa_gefs_download_downscale(site_list = paste0(site.name, "grid"),
#                                             lat_list = lat.in,
#                                             lon_list= lon.in,
#                                             output_directory = outdir,
#                                             forecast_time = "00",
#                                             forecast_date = Sys.Date(),
#                                             downscale = FALSE,
#                                             run_parallel = FALSE,
#                                             num_cores = 1,
#                                             method = "grid",
#                                             overwrite = FALSE)
