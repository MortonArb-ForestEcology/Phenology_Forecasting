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
library(data.table)

path.hub <- "C:/Users/lucie/Documents/GitHub/"
#path.hub <- "../.."

path.daymet <- "../data_raw/DAYMET"
if(!dir.exists(path.daymet)) dir.create(path.daymet)


#Retrieving npn data
#The Genus ID is currently for Quercus respectively. Phenophase is breaking leaf buds
#rnpn packages has tools to show the corresponding id's for these queries. Request source is your name/affiliation
dat.npn <- npn_download_status_data(request_source='Morton Arboretum', years=c(2010:2019), genus_ids = c(946),phenophase_ids =c(371))

dat.npn$year <- lubridate::year(dat.npn$observation_date)

dat.npn <- aggregate(dat.npn[dat.npn$phenophase_status==1, "day_of_year"], 
                     by=dat.npn[dat.npn$phenophase_status==1, c("latitude", "longitude", "individual_id", "year", "genus", "species", "site_id" )], 
                     FUN=min)

dat.npn$species <- paste(dat.npn$genus, dat.npn$species, sep= " ")


colnames(dat.npn) <- c("Latitude", "Longitude", "PlantNumber", "Year","Genus", "Species", "Site", "Yday")
dat.npn$PlantNumber <- as.factor(dat.npn$PlantNumber)

for(YR in dat.npn$Year){
  start <- paste(as.character(dat.npn$Year), "-01-01", sep="")
  dat.npn$Date <- as.Date((dat.npn$Yday-1), origin = start)
}

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


# This gives us a list with one layer per site (I think)
length(lat.list)
names(lat.list) <- NPN.pts$site # Giving the different layers of the list the site names they correspond to

#Lets look at the structure of what we are given
summary(lat.list)

#As you might notice we have a "List of 1" containing a nested "List of 7"
#Within that "List of 7" there is a data frame called data that has the data we want
#Well we know where it is now but how do we access it?

# Creating a new simplified list that won't make Christy cranky
list.met <- list()
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=NPN.pts$Site[i], latitude=NPN.pts$Latitude[i], longitude=NPN.pts$Longitude[i], lat.list[[i]]$data)
}
names(list.met) <-  NPN.pts$site

rm(lat.list) # Removing lat.list to save memory


#Reading in our function for calculating weather statistics of interest
source(file.path(path.hub, "Phenology_Forecasting/scripts/weather_calc.R"))

#Running our function to calculate weather statistics. Default year range is 1975-2019. Growing seaosn is yday 1 to 120
list.met<- lapply(list.met, weather_calc)


write.csv(list.met, "../data_processed/Daymet_clean_data.csv", row.names=F)

#IF YOU READ IN THE CV YOU DON"T NEED THE rbindlist LINE AS IT IS READ IN AS A DATa FRAME
#list.met <- read.csv( "../data_processed/Daymet_clean_data.csv")

lat.calc <- rbindlist(list.met)

rm(list.met)

dat.comb <- data.frame()
pcount <- 1
for(LOC in unique(as.numeric(dat.npn$Site))){
  npn.tmp <- dat.npn[dat.npn$Site == LOC,]
  npn.tmp$GDD5.cum <- NA
  npn.tmp$GDD0.cum <- NA
  npn.tmp$NCD <- NA
  npn.tmp$GTmean <- NA
  for(DAT in paste(npn.tmp$Date)){
    if(length(lat.calc[lat.calc$Date==as.Date(DAT) & lat.calc$site == LOC, "GDD5.cum"]) > 0){
      npn.tmp[npn.tmp$Date==as.Date(DAT),"GDD5.cum"] <- lat.calc[lat.calc$Date==as.Date(DAT) & lat.calc$site == LOC, "GDD5.cum"]
    }
    if(length(lat.calc[lat.calc$Date==as.Date(DAT)& lat.calc$site == LOC, "GDD0.cum"]) > 0){
      npn.tmp[npn.tmp$Date==as.Date(DAT),"GDD0.cum"] <- lat.calc[lat.calc$Date==as.Date(DAT) & lat.calc$site == LOC, "GDD0.cum"]
    }
    if(length(lat.calc[lat.calc$Date==as.Date(DAT)& lat.calc$site == LOC, "NCD"]) > 0){ 
      npn.tmp[npn.tmp$Date==as.Date(DAT),"NCD"] <- lat.calc[lat.calc$Date==as.Date(DAT) & lat.calc$site == LOC, "NCD"]
    }
    if(length(lat.calc[lat.calc$Date==as.Date(DAT)& lat.calc$site == LOC, "GTmean"]) > 0){ 
      npn.tmp[npn.tmp$Date==as.Date(DAT),"GTmean"] <- lat.calc[lat.calc$Date==as.Date(DAT)& lat.calc$site == LOC, "GTmean"]
    }
  }
  dat.comb <- rbind(dat.comb, npn.tmp)
}

summary(dat.comb)

# Save dat.comb 
write.csv(dat.comb, "../data_processed/Phenology_NPN_combined.csv", row.names=F)

