download.ghcn <- function(ID="USC00115097", vars.in= c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), path.save, dir.raw, gapfill=T, method="https"){
  vars.in <- toupper(vars.in)
  if(!dir.exists(path.save)) dir.create(path.save)
  # if(any(!vars.in %in% c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")))
  
  if(!method %in% c("https", "ftp")){
    warning("invalid method.  Only https and ftp allowed.  Defaulting to https")
    method="https"
  } 
  # if(any(!vars.in %in% c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")))
  if(method=="ftp"){
    dat.raw <- FedData::get_ghcn_daily_station(ID=ID, raw.dir=dir.raw, force.redo = T)
    
    # Finding an issue where not all variables may line up (weird; but we can deal)
    dat.ghcn <- data.frame(STATION=ID)
    summary(dat.ghcn)
    
    for(MET in vars.want){
      dat.var <- data.frame(YEAR=dat.raw[[MET]]$YEAR, 
                            MONTH=dat.raw[[MET]]$MONTH,
                            DAY=rep(1:31, each=nrow(dat.raw[[MET]])))
      dat.var[,MET] <- stack(dat.raw[[MET]][grep("D", names(dat.raw[[MET]]))])[,1]
      
      dat.ghcn <- merge(dat.ghcn, dat.var, all=T)
    }
    
    # Format Date
    dat.ghcn$DATE <- as.Date(paste(dat.ghcn$YEAR, dat.ghcn$MONTH, dat.ghcn$DAY, sep="-"))
    dat.ghcn <- dat.ghcn[!is.na(dat.ghcn$DATE),]
    dat.ghcn$YDAY <- lubridate::yday(dat.ghcn$DATE)
    
  }
  if(method=="https"){
    ghcn.https <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/"
    dat.raw <- read.csv(file.path(ghcn.https, paste0(ID, ".csv")))
    dat.raw$DATE <- as.Date(dat.raw$DATE)
    
    # Adding in missing dates
    df.date <- data.frame(DATE=seq.Date(min(dat.raw$DATE), max(dat.raw$DATE), by=1))
    
    dat.raw <- merge(dat.raw, df.date, all=T)
    dat.raw$YEAR <- lubridate::year(dat.raw$DATE)
    dat.raw$MONTH <- lubridate::month(dat.raw$DATE)
    dat.raw$DAY <- lubridate::day(dat.raw$DATE)
    dat.raw$YDAY <- lubridate::yday(dat.raw$DATE)
    
    dat.ghcn <- dat.raw[,c("YEAR", "MONTH", "DAY", "STATION", "TMAX", "TMIN", "PRCP", "SNOW", "SNWD", "DATE", "YDAY")]
  }
  
  dat.ghcn[,c("TMAX", "TMIN", "PRCP")] <- dat.ghcn[,c("TMAX", "TMIN", "PRCP")]*0.1 # Unit correction

  dat.ghcn <- dat.ghcn[dat.ghcn$DATE < Sys.Date(),] # Get rid of anything in the future
  dat.ghcn <- dat.ghcn[order(dat.ghcn$DATE),] # Ordering just to make life easier
  # dat.ghcn[,]
  summary(dat.ghcn)
  tail(dat.ghcn)
  
  # If we have no data for days at the end, just trim those days
  dat.none <- which(is.na(dat.ghcn$TMAX) | is.na(dat.ghcn$TMIN))
  while(dat.none[length(dat.none)]==nrow(dat.ghcn)){
    dat.ghcn <- dat.ghcn[1:(nrow(dat.ghcn)-1),]
    dat.none <- which(is.na(dat.ghcn$TMAX) | is.na(dat.ghcn$TMIN))
  }
  tail(dat.ghcn)
  
  # -------------------------------------
  # Missing days are going to be a pain in the butt, so lets do dumb gap-filling for now. 
  #  - For temperature, linearly interpolate
  #  - For precipitation, just assume no rain
  #  ** Make sure to add column about being gapfilled
  # -------------------------------------
  if(gapfill){
    # if(!"met.gapfill" %in% ls()) source("met_gapfill.R")
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
    
    # summary(dat.ghcn)
  }
  write.csv(dat.ghcn, file.path(path.save, paste0(ID, "_latest.csv")), row.names=F)
  
}