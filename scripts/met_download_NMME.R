# https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/north-american-multi-model-ensemble
download.nmme <- function(mod.use="CCSM4", vars.in, lat.in, lon.in, path.save, forecast.start = Sys.Date(), forecast.end = paste0(lubridate::year(Sys.Date()), "-12-31")){
  if(!dir.exists(path.save)) dir.create(path.save, recursive=T, showWarnings = F)
  if(lon.in<0) lon.in = 180+lon.in
  
  # if(!mod.use %in% c("CCSM")){
  #   warning("This model is not in NMME.")
  #   return()
  # }
  if(mod.use %in% c("CanCM3")) {
    warning("CanCM3 is out of date.  Do not use")
    return()
  }
  if(grepl("cfs", tolower(mod.use))){
    warning("CFSV2-2011 not implemented yet.  Use alternate script")
    return()
  }
  
  if(mod.use %in% c("GEOS-5")) {
    warning("GEOS-5 has no forecast data.  Stopping.")
    return()
  }
  
  nmme.vars <- data.frame(shorthand=c("precip", "tmax", "tmin", "stress.zonal", "stress.meridional", "press", "wind.zonal", "wind.meridonal", "lw.net", "solar.net", "gph", "sh"), 
                          variable=c("precipitation rate", "daily maximum surface air temperature", "daily minimum surface air temperature", "zonal surface stress", "meridional surface stress", "sea level pressure", "zonal wind", "meridional wind", "net longwave flux at top of model", "net solar flux at surface", "geopotential height", "specific humidity"),
                          var2 = c("Total Precipitation", "Maximum Temperature", "Minimum Temperature", "Wind Stress Eastward", "Wind Stress Northward", "Pressure at Sea Level", "Wind Eastward", "Wind Northward", NA, NA, "Geopotential", "Specific Humidity"),
                          CCSM4=c("pr", "TREFMXAV", "TREFMNAV", "STX", "STY", "PSL", "ua", "va", NA, NA, "g", "HUS"),
                          CESM1=c("precip", "Tasmax", "Tasmin", "Stx", "Sty", "Psl", "ua", "va", "Rlt", "Rss", "G", "hus"),
                          CFSV2=c("pr", "tasmax", "tasmin", "stx", "sty", "psl", "ua", "va", NA, NA, "g", "hus"),
                          CanCM3=c("pr", "tasmax", "tasmin", "stx", "sty", "psl", "ua", "va", "rlt", "rss", "g", "hus"),
                          CanCM4=c("pr", "tasmax", "tasmin", "stx", "sty", "psl", "ua", "va", "rlt", "rss", "g", "hus"),
                          FLORB=c("pr", "tasmax", "tasmin", NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          GEOS5=c("prlr", "tasmax", "tasmin", "stx", "sty", "psl", "ua", "va", "rlt", NA, "g", "hus"))
  
  dat.base <- "https://www.ncei.noaa.gov/thredds/ncss"
  ncei.base <- "https://www.ncei.noaa.gov/"
  cat.base <- "https://www.ncei.noaa.gov/thredds/model/north-american-multi-model-ensemble-nmme"
  
  if(mod.use %in% c("CCSM4", "GEOS-5")){
    fname <- RCurl::getURL(file.path(cat.base, paste0(tolower(mod.use), "_catalog.html")), dirlistonly = TRUE)
    fname <- strsplit(fname, "\r*\n")[[1]]
  }
  
  if(mod.use %in% c("CanCM4", "CanCM3")){
    fname <- RCurl::getURL(file.path(cat.base, paste0(tolower(mod.use), "_catalog.html")), dirlistonly = TRUE)
    fname <- strsplit(fname, "\r*\n")[[1]]
    fmod <- fname[grep(paste("NMME", mod.use, "File Access"), fname)]
    # fname[f.ind]
    fmod <- strsplit(fmod, "'")[[1]][6]
    
    fyr <- RCurl::getURL(file.path(ncei.base, fmod), dirlistonly = TRUE)
    fyr <- strsplit(fyr, "\r*\n")[[1]]
    
    flink <- grep("thredds/folder.gif", fyr)
    fyr <- fyr[flink[length(flink)]]  # These are ordered oldest to newest, so we want X from the bottom
    fyr <- strsplit(fyr, "'")[[1]][6]
    
    mod.base <- strsplit(fmod, "/")[[1]]
    mod.base <- paste(mod.base[1:(length(mod.base)-1)], collapse="/")
    # Next step
    fname <- RCurl::getURL(file.path(ncei.base, mod.base, fyr), dirlistonly = TRUE)
    fname <- strsplit(fname, "\r*\n")[[1]]
  }
  
  for(VAR in vars.in){
    # VAR="tmax"
    var.cat <- nmme.vars$var2[nmme.vars$shorthand==VAR]
    var.mod <- nmme.vars[nmme.vars$shorthand==VAR, gsub("-", "", mod.use)]
    
    # Note: Most models will have multiple ensemble members
    if(mod.use %in% c("CCSM4", "GEOS-5")){
      fvar <- grep(var.cat, fname)
    } else {
      # For the CanCM3 models, it has multiple output oldest to newest; need to get the most recent
      fvar <- grep(var.mod, fname)
      # fname <- fname[fvar]
      
      date.start <- vector(length=length(fvar))
      for(i in 1:length(fvar)){
        date.start[i] <- as.numeric(substr(strsplit(fname[fvar[i]], "_")[[1]][11],1,8))
      }
      fvar <- fvar[date.start==max(date.start)]
    }
    
    # CCSM4 example: https://www.ncei.noaa.gov/thredds/catalog/model-nmme_ccsm4_tasmax_day_r01_agg/catalog.html
    for(i in 1:length(fvar)){
      ens.base <- paste(mod.use, "ens", stringr::str_pad(i, 2, 0, side="left"), sep="_")
      path.ens <- file.path(path.save, ens.base)
      if(!dir.exists(path.ens)) dir.create(path.ens, recursive=T, showWarnings = F)
      
      
      fnow <- fname[fvar[i]]
      if(mod.use %in% c("CCSM4", "GEOS-5")){
        fnow <- strsplit(fnow, "'")[[1]][6]  
        fnext <- RCurl::getURL(file.path(ncei.base, fnow), dirlistonly = TRUE)
        fnext <- strsplit(fnext, "\r*\n")[[1]]
        fnext <- fnext[grep("Best Time Series", fnext)]
        
      } else {
        fnext <- fnow
      }
      url.use <- strsplit(fnext, "'")[[1]][2]
      url.use <- strsplit(url.use, "=")[[1]][2]
      if(mod.use %in% c("CCSM4", "GEOS-5")){
        url.use <- file.path(dat.base, url.use)
      } else {
        url.use <- file.path(dat.base, paste0("model-", url.use))
      }
      
      
      
      query.use <- paste0("?var=", var.mod, "&latitude=", lat.in, 2, "&longitude=", lon.in, 2, "&time_start=", forecast.start, "T12%3A00%3A00Z&time_end=", forecast.end, "T12%3A00%3A00Z&timeStride=1&addLatLon=true&accept=csv")
      # paste0(url.use, query.use)
      
      download.file(paste0(url.use, query.use), destfile=file.path(path.ens, paste0(VAR, "_",ens.base, "_latest.csv")))
        
  
    }
    
  }
}


