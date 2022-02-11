# A function to download NOAA CFS operational forecasts: 
# https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/climate-forecast-system-version2-cfsv2#CFSv2%20Operational%20Forecasts
# Full documentation to come (ask Christy in the meanwhile)
## NOTE: ADDED NEW FUNCTIONALITY TO GET HISTORICAL FORECASTS!  You can pass in "latest" if you want whatever's newest, but you can also pass in specific dates.  Note: Dates > "lastest" may cause issues
library(httr)

download.cfs <- function(vars.in, lat.in, lon.in, path.save, forecast.start = "latest", forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")){
  if(!dir.exists(path.save)) dir.create(path.save, recursive=T, showWarnings = F)

  # The variable tranlsation table
  var.table <- data.frame(var=c("tmax", "tmin", "prate"),
                          cfs=c("Maximum_temperature_height_above_ground", "Minimum_temperature_height_above_ground", "Precipitation_rate_surface"))
  
  if(lon.in<0) lon.in = 180+lon.in
  
  # Setting up the various file paths
  cfs.base <- "https://www.ncei.noaa.gov/thredds/ncss/model-cfs_v2_for_ts"
  cat.base <- "https://www.ncei.noaa.gov/thredds/catalog/model-cfs_v2_for_ts"
  # https://www.ncei.noaa.gov/thredds/catalog/model-cfs_v2_for_ts/catalog.html
  
  if(class(forecast.start)=="Date") {
    day.strng <- forecast.start
    
    yr.latest <- substr(day.strng, 1, 4)
    mo.latest <- paste0(yr.latest, substr(day.strng, 6, 7))
    day.latest <- paste0(mo.latest, substr(day.strng, 9, 10))
    
    connec <- httr::GET(file.path(cat.base, yr.latest, mo.latest, day.latest, "catalog.html"))
    fname4 <- strsplit(content(connec, "text"), "href='")[[1]]
    
    # Stop if this date isn't in the forecast yet:
    if(grepl("HTTP Status 404 - Not Found", fname4[1])){
      warning(paste0("Forecast Date Not Found: ", day.strng))
      return()
    }

    str.hr <- fname4[grep(day.latest, fname4)[4]] # Pass thorugh all the headers to get the most recent day
    hr.latest <- strsplit(str.hr, "<*tt*>")[[1]][2]
    hr.latest <- substr(hr.latest, 1, 10)
    rm(str.hr) # Remove this so tryCatch will work above
  } else if(forecast.start=="latest"){
    # 1. Getting a list of the years we can get dara from
    #fname <- RCurl::getURL(file.path(cat.base, "catalog.html"), dirlistonly = TRUE)
    connec <- httr::GET(file.path(cat.base, "catalog.html"))
    yr.latest <- strsplit(strsplit(content(connec, "text"), "href='")[[1]][4], "/")[[1]][1]
    
    # 2. Getting the available months for the latest year
    #fname2 <- RCurl::getURL(file.path(cat.base, yr.latest, "catalog.html"))
    connec <- httr::GET(file.path(cat.base, yr.fcst, "catalog.html"))
    fname2 <- strsplit(content(connec, "text"), "href='")[[1]]
    
    str.mo <- fname2[grep(yr.latest, fname2)[4]] # Pass thorugh all the headers to get the most recent month
    mo.latest <- strsplit(str.mo, "<*tt*>")[[1]][2]
    mo.latest <- substr(mo.latest, 1, 6)
    
    # 3. getting the latest day
    
    #fname3 <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, "catalog.html"), dirlistonly = TRUE)
    connec <- httr::GET(file.path(cat.base, yr.latest, mo.latest, "catalog.html"))
    fname3 <- strsplit(content(connec, "text"), "href='")[[1]]
    str.day <- fname3[grep(mo.latest, fname3)[4]] # Pass thorugh all the headers to get the most recent day
    
    day.latest <- strsplit(str.day, "<*tt*>")[[1]][2]
    day.latest <- substr(day.latest, 1, 8)
    
    # 4. getting the latest hour
    #fname4 <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, day.latest, "catalog.html"), dirlistonly = TRUE)
    connec <- httr::GET(file.path(cat.base, yr.latest, mo.latest, day.latest, "catalog.html"))
    fname4 <- strsplit(content(connec, "text"), "href='")[[1]]
    str.hr <- fname4[grep(day.latest, fname4)[4]] # Pass thorugh all the headers to get the most recent day
    
    hr.latest <- strsplit(str.hr, "<*tt*>")[[1]][2]
    hr.latest <- substr(hr.latest, 1, 10)
    
    # f.avail <- RCurl::getURL(file.path(cat.base, yr.latest, mo.latest, day.latest, hr.latest, "catalog.html"), dirlistonly = TRUE)
    # f.avail <- strsplit(f.avail, "\r*\n")[[1]]
    # f.grb <- f.avail[grep(".grb2</tt>", f.avail)]
    
    day.strng <- as.Date(day.latest, format="%Y%m%d")
    
  } else {
    stop("forecast.start needs to either a date or 'latest'")
  }
  
  
  # Loop through our variables to get what we want & save them
  for(VAR in vars.in){
    cfs.var <- var.table$cfs[var.table$var==VAR]
    
    path.latest <- file.path(cfs.base, yr.latest, mo.latest, day.latest, hr.latest, paste0(VAR, ".01.", hr.latest, ".daily.grb2?var=", cfs.var, "&latitude=", lat.in, "&longitude=", lon.in, "&time_start=", forecast.start, "T00%3A00%3A00Z&time_end=", forecast.end, "T00%3A00%3A00Z&vertCoord=1&accept=csv"))
    
    tryCatch(download.file(path.latest, destfile=file.path(path.save, paste0(VAR, "_cfs_", day.strng, "_latest.csv"))),
             error=function(e){ print(paste0("File not found: ", VAR, " ", day.strng))})
  }
}