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
dat.ghcn$GDD0 <- ifelse(dat.ghcn$TMEAN>0, dat.ghcn$TMEAN-0, 0)
dat.ghcn$GDD5 <- ifelse(dat.ghcn$TMEAN>5, dat.ghcn$TMEAN-5, 0)

# For chilling days, only start after the solstice (June 20th)
dat.ghcn$CDD0 <- ifelse(dat.ghcn$YDAY>172 & dat.ghcn$TMEAN<0, 0-dat.ghcn$TMEAN, 0)
dat.ghcn$CDD2 <- ifelse(dat.ghcn$YDAY>172 & dat.ghcn$TMEAN< -2, -2-dat.ghcn$TMEAN, 0)
dat.ghcn$DaysNoRain <- NA
summary(dat.ghcn)
# -------------------------------------



# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
yr.min <- 2008
yr.max <- lubridate::year(Sys.Date())
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]

  dat.ghcn[rows.yr, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum")] <- cumsum(dat.ghcn[rows.yr, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP")])
  
  dat.ghcn[rows.yr, c("NORAIN.cum")] <- cumsum(ifelse(dat.ghcn[rows.yr,"PRCP"]>0, 1, 0))
  
  # Calculating days since rain just in case
  dat.ghcn$DaysNoRain[rows.yr[1]] <- ifelse(dat.ghcn$PRCP[rows.yr[1]]>0, 0, 1)
  for(i in 2:length(rows.yr)){
    dat.ghcn$DaysNoRain[rows.yr[i]] <- ifelse(dat.ghcn$PRCP[rows.yr[i]]>0, dat.ghcn$DaysNoRain[rows.yr[i-1]]+1, 0)
  }
}
summary(dat.ghcn)

write.csv(dat.ghcn, file.path(path.out, "data", "weather_ArbCOOP_latest.csv"), row.names=F)
# -------------------------------------

# -------------------------------------
# Doing some quick graphing
# -------------------------------------
day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
 dat.ghcn$threshA <- dat.ghcn$GDD0.cum 
 dat.ghcn$threshB <- dat.ghcn$GDD5.cum 
} else {
  dat.ghcn$threshA <- dat.ghcn$CDD0.cum 
  dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
}

plot.threshA <- ggplot(data=dat.ghcn, aes(x=YDAY, y=threshA)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.threshB <- ggplot(data=dat.ghcn, aes(x=YDAY, y=threshB)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))) {
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. GrowDD, base 0 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY<=180], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. GrowDD, base 5 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY<=180], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
} else {
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. ChillDD, base 0 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY>=155], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. ChillDD, base -2 C" ,expand=c(0,0), limits=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY>=155], na.rm=T))) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) 
}  

plot.prcp <- ggplot(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=2) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)]) +
  scale_y_continuous(name="Cum. Precip (mm)" ,expand=c(0,0)) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.tmean <- ggplot(data=dat.ghcn, aes(x=YDAY, y=TMEAN)) +
  stat_summary(fun.y=mean, color="black", geom="line", size=1) +
  geom_line(aes(group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(color=as.factor(lubridate::year(Sys.Date()))), size=1) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)]) +
  scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
  scale_color_manual(name="Year", values = "blue2") +
  theme_bw() +
  guides(color=F) +
  theme(legend.position = c(0.1, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.dat <- cowplot::plot_grid(plot.tmean, plot.prcp, plot.threshA, plot.threshB)

library(cowplot)
plot.title <- ggdraw() + 
  draw_label(paste("The Morton Arboretum weather, last updated:", max(dat.ghcn$DATE)),
    fontface = 'bold', x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 1)
  )


png(file.path(path.out, "figures", "Weather_latest.png"), height=6, width=6, units="in", res=220)
cowplot::plot_grid(plot.title, plot.dat,ncol=1, rel_heights = c(0.1, 1))
dev.off()
# -------------------------------------



# ---------------------------------

# ---------------------------------
# 2. Forecast Data 
# ---------------------------------
# ---------------------------------
