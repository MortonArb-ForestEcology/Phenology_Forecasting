# A function to download NOAA CFS operational forecasts: 
# https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/climate-forecast-system-version2-cfsv2#CFSv2%20Operational%20Forecasts
# Full documentation to come (ask Christy in the meanwhile)

download.cfs <- function(vars.in, lat.in, lon.in, path.save, forecast.start = Sys.Date(), forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")){
  if(!dir.exists(path.save)) dir.create(path.save, recursive=T, showWarnings = F)

  # The variable tranlsation table
  var.table <- data.frame(var=c("tmax", "tmin", "prate"),
                          cfs=c("Maximum_temperature_height_above_ground", "Minimum_temperature_height_above_ground", "Precipitation_rate_surface"))
  
  if(lon.in<0) lon.in = 180+lon.in

  # Setting up the various file paths
  cfs.base <- "https://www.ncdc.noaa.gov/thredds/ncss/cfs_v2_for_ts"
  cat.base <- "https://www.ncdc.noaa.gov/thredds/catalog/cfs_v2_for_ts"
  
  # 1. Getting a list of the years we can get dara from
  fname <- RCurl::getURL(file.path(cat.base, "catalog.html"), dirlistonly = TRUE)
  fname <- strsplit(fname, "\r*\n")[[1]]
  
  ind.parent <- grep("Time Series", fname)
  # ind.yr <- grep(lubridate::year(Sys.Date()), fname2)
  f.latest <- fname[ind.parent+6]
  yr.latest <- strsplit(f.latest, "<*tt*>")[[1]][2]
  yr.latest <- substr(yr.latest, 1, 4)
  
  # 2. Getting the available months for the latest year
  fname2 <- RCurl::getURL(file.path(cat.base, yr.latest, "catalog.html"), dirlistonly = TRUE)
  fname2 <- strsplit(fname2, "\r*\n")[[1]]
  str.mo <- fname2[grep(yr.latest, fname2)[4]] # Pass thorugh all the headers to get the most recent month
  mo.latest <- strsplit(str.mo, "<*tt*>")[[1]][2]
  mo.latest <- substr(mo.latest, 1, 6)
  
  # 3. getting the latest day
  fname3 <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, "catalog.html"), dirlistonly = TRUE)
  fname3 <- strsplit(fname3, "\r*\n")[[1]]
  str.day <- fname3[grep(mo.latest, fname3)[4]] # Pass thorugh all the headers to get the most recent day
  
  day.latest <- strsplit(str.day, "<*tt*>")[[1]][2]
  day.latest <- substr(day.latest, 1, 8)
  
  # 4. getting the latest hour
  fname4 <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, day.latest, "catalog.html"), dirlistonly = TRUE)
  fname4 <- strsplit(fname4, "\r*\n")[[1]]
  str.hr <- fname4[grep(day.latest, fname4)[4]] # Pass thorugh all the headers to get the most recent day
  
  hr.latest <- strsplit(str.hr, "<*tt*>")[[1]][2]
  hr.latest <- substr(hr.latest, 1, 10)
  
  # f.avail <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, day.latest, hr.latest, "catalog.html"), dirlistonly = TRUE)
  # f.avail <- strsplit(f.avail, "\r*\n")[[1]]
  # f.grb <- f.avail[grep(".grb2</tt>", f.avail)]
  
  day.strng <- as.Date(day.latest, format="%Y%m%d")
  
  # WHen trying to string together the path, use ncss = netcdf sub set
  
  # Loop through our variables to get what we want & save them
  for(VAR in vars.in){
    cfs.var <- var.table$cfs[var.table$var==VAR]
    
    path.latest <- file.path(cfs.base, yr.latest, mo.latest, day.latest, hr.latest, paste0(VAR, ".01.", hr.latest, ".daily.grb2?var=", cfs.var, "&latitude=", lat.in, "&longitude=", lon.in, "&time_start=", start.str, "T00%3A00%3A00Z&time_end=", end.str, "T00%3A00%3A00Z&vertCoord=1&accept=csv"))
    
    download.file(path.latest, destfile=file.path(path.save, paste0(VAR, "_cfs_latest.csv")))
  }
}