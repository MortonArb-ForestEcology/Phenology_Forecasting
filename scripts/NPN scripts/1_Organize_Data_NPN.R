#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the model input for NPN data
# Inputs: Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         NPN download currently manually gotten from the npn website
# Outputs:dat.comb dataframe that can be used in the NPN models in  this repository
# Notes: All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
#        The majority of the rest is currently a modification of that same script by Christy

#-----------------------------------------------------------------------------------------------------------------------------------#
#dplyr for the summarise function
library(dplyr)
library(rnpn)

path.hub <- "C:/Users/lucie/Documents/GitHub/"
#path.hub <- "../.."

path.daymet <- "../data_raw/DAYMET"
if(!dir.exists(path.daymet)) dir.create(path.daymet)


library(data.table)

#Retrieving npn data
#The Species ID's are currently for imbricaria falcata, and stellata respectively. Phenophase is breakign leaf buds
#rnpn packages has tools to show the corresponding id's for these queries. Request source is your name/affiliation
dat.npn <- npn_download_status_data(request_source='Morton Arboretum', years=c(2010:2019), 
                                    species_id=c(1190, 1484, 1755), phenophase_ids =c(371))

dat.npn$year <- lubridate::year(dat.npn$observation_date)

dat.npn <- aggregate(dat.npn[dat.npn$phenophase_status==1, "day_of_year"], 
                     by=dat.npn[dat.npn$phenophase_status==1, c("latitude", "longitude", "individual_id", "year", "genus", "species" )], 
                     FUN=min)

dat.npn$species <- paste(dat.npn$genus, dat.npn$species, sep= " ")


colnames(dat.npn) <- c("Latitude", "Longitude", "PlantNumber", "Year","Genus", "Species", "Yday")
dat.npn$PlantNumber <- as.factor(dat.npn$PlantNumber)

for(YR in dat.npn$Year){
  start <- paste(as.character(dat.npn$Year), "-01-01", sep="")
  dat.npn$Date <- as.Date((dat.npn$Yday-1), origin = start)
}

# Note: We will probably want this to go by species, rather than site, but for now this works
dat.npn$Site <- 'NPN'


# Creating a point list and time range that matches your MODIS dataset
# Note: This will probably change down the road
NPN.pts <- aggregate(Year~Site+Latitude+Longitude, data=dat.npn, 
                       FUN=min)
names(NPN.pts)[4] <- "yr.start"
NPN.pts$yr.end <- aggregate(Year~Site+Latitude+Longitude, data=dat.npn, 
                              FUN=max)[,4]
NPN.pts


#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(NPN.pts, file.path(path.daymet, "NPN_points.csv"), row.names=FALSE)

# if(!dir.exist(path.daymet)) dir.create(path.daymet)
#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.daymet, "NPN_points.csv"),
                                           start = min(NPN.pts$yr.start),
                                           end = max(NPN.pts$yr.end),
                                           internal = T)


#removing failed downloads 
lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]

#Reading in our function for calculating weather statistics of interest
source(file.path(path.hub, "Phenology_Forecasting/scripts/NPN scripts/Daymetr_weather_calc.R"))

#Running our function to calculate weather statistics. Default year range is 1975-2019. Growing seaosn is yday 1 to 120
lat.calc<- Daymetr_weather_calc(lat.list)


write.csv(lat.calc, "../data_processed/Daymet_clean_data.csv", row.names=F)


df.loc <- read.csv( "../data_processed/Daymet_clean_data.csv")


#Making sure we only go through relevant years we are calculating gdd5 for
dat.npn <- dat.npn[dat.npn$Year >= ystart, ]


#Our occurence points are rounded becasue daymet rounds them to 6 digits. 
#Unfortunately we need to round them to 4 becasue otherwise they don't macth as a few points will round differently
dat.npn$Latitude <- signif(dat.npn$Latitude, digits = 7) 
dat.npn$Longitude <- signif(dat.npn$Longitude, digits = 7) 

#This can be removed if down the line I figure out a way to match the locations back without rounding
df.loc$latitude <- signif(df.loc$latitude, digits = 7) 
df.loc$longitude <- signif(df.loc$longitude, digits = 7)


#This section is a gross solution so I can leave it in a working state
#For some reason if you start the rounding at 6 digits, they round differently and won't match
#If you round them to 7 and then to 6 they will all match
#Since this rounding solution needs to be fully removed this is my current bad solution
dat.npn$Latitude <- signif(dat.npn$Latitude, digits = 6) 
dat.npn$Longitude <- signif(dat.npn$Longitude, digits = 6) 


df.loc$latitude <- signif(df.loc$latitude, digits = 6) 
df.loc$longitude <- signif(df.loc$longitude, digits = 6)


dat.npn$Location <- paste(dat.npn$Latitude, dat.npn$Longitude, sep= " ")
df.loc$location <- paste(df.loc$latitude, df.loc$longitude, sep= " ")

dat.comb <- data.frame()
pcount <- 1
for(LOC in unique(as.character(dat.npn$Location))){
  npn.tmp <- dat.npn[dat.npn$Location == LOC,]
  npn.tmp$GDD5.cum <- NA
  npn.tmp$NCD <- NA
  npn.tmp$GTmean <- NA
  for(DAT in paste(npn.tmp$Date)){
    if(length(df.loc[df.loc$Date==as.Date(DAT), "GDD5.cum"]) > 0){
      npn.tmp[npn.tmp$Date==as.Date(DAT),"GDD5.cum"] <- df.loc[df.loc$Date==as.Date(DAT) & df.loc$location == LOC, "GDD5.cum"]
    }
    if(length(df.loc[df.loc$Date==as.Date(DAT), "NCD"]) > 0){ 
      npn.tmp[npn.tmp$Date==as.Date(DAT),"NCD"] <- df.loc[df.loc$Date==as.Date(DAT) & df.loc$location == LOC, "NCD"]
    }
    if(length(df.loc[df.loc$Date==as.Date(DAT), "GTmean"]) > 0){ 
      npn.tmp[npn.tmp$Date==as.Date(DAT),"GTmean"] <- df.loc[df.loc$Date==as.Date(DAT)& df.loc$location == LOC, "GTmean"]
    }
  }
  dat.comb <- rbind(dat.comb, npn.tmp)
}

dat.comb[dat.comb$Yday>=171, c("Yday", "GDD5.cum", "NCD")] <- NA
summary(dat.comb)

# Save dat.comb 
write.csv(dat.comb, "../data_processed/Phenology_NPN_combined.csv", row.names=F)

