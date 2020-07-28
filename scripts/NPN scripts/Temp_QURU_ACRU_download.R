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


dat.npn <- npn_download_individual_phenometrics(phenophase_ids =c(371),  years=2000:2019, species_ids = c(3, 102), request_source="The Morton Arboretum")
dat.npn[dat.npn==-9999] <- NA

dat.npn$species <- as.factor(dat.npn$species)
dat.npn$species_id <- as.factor(dat.npn$species_id)
dat.npn$individual_id <- as.factor(dat.npn$individual_id)
dat.npn$phenophase_id <- as.factor(dat.npn$phenophase_id)
dat.npn$phenophase_description <- as.factor(dat.npn$phenophase_description)

#Pulling in names for use
#IGNORE THE WARNING
site_names <- npn_stations()

dat.npn$site_name <- site_names$station_name[match(dat.npn$site_id, site_names$station_id)]

#Makigns ure different locations with the same name are given unique names by adding site_id
for(Name in unique(dat.npn$site_name)){
  dat.tmp <- dat.npn[dat.npn$site_name == Name,]
  if(length(unique(dat.tmp$site_id)) >1){
    dat.tmp$site_name <- paste(dat.tmp$site_name, dat.tmp$site_id, sep="_")
  }
  dat.npn[dat.npn$site_name==Name, "site_name"] <- dat.tmp$site_name
}

dat.quru <- dat.npn[dat.npn$species == "rubra", ]
dat.acru <- dat.npn[dat.npn$species == "rubrum", ]

#--------------------------------------------#
#This section that pulls out matching sites and matches their names is very rough I apologize. It does work though

#Pulling out unique sites
quru.sites <- as.data.frame(table(dat.quru$site_name))
colnames(quru.sites) <- c("Site", "Freq")

acru.sites <- as.data.frame(table(dat.acru$site_name))
colnames(acru.sites) <- c("Site", "Freq")

#Making sure they only use matching sites
dat.quru <- dat.quru[dat.quru$site_name %in% acru.sites$Site,]
dat.acru <- dat.acru[dat.acru$site_name %in% quru.sites$Site,]

dat.bind <- rbind(dat.quru, dat.acru) 

# ------------------------------------------
# Deciding what data is "good" or "bad"
# ------------------------------------------
# Using a 10-day thresholds since prior/next no as an indicator of questionable data  or if there is no value for that (first obs of the year)
# What this code says data[ROWS, COLUMNS]
# if the days since last no is greather than 10 OR (| mean OR) there is no "no" observation before a yes
dat.bind[dat.bind$numdays_since_prior_no>10 | is.na(dat.bind$numdays_since_prior_no), c("first_yes_doy", "first_yes_julian_date")] <- NA

dat.bind[dat.bind$numdays_until_next_no>10 | is.na(dat.bind$numdays_until_next_no), c("last_yes_doy", "last_yes_julian_date")] <- NA


# Getting rid of bud burst after July 1 (~182) because we just want SPRING budburst
dat.bind[dat.bind$first_yes_doy>182 & !is.na(dat.bind$first_yes_doy), c("first_yes_doy", "first_yes_julian_date")] <- NA
dat.bind[dat.bind$last_yes_doy>182 & !is.na(dat.bind$last_yes_doy), c("last_yes_doy", "last_yes_julian_date")] <- NA

# Aggregateing using a formula; in R, y=mx+b is y ~ m*x + b 
dat.budburst <- data.frame(individual_id=rep(unique(dat.bind$individual_id), each=length(unique(dat.bind$first_yes_year))),
                           year=unique(dat.bind$first_yes_year))
summary(dat.budburst)                           

for(IND in unique(dat.budburst$individual_id)){
  # adding some individual metadata -- this only needs to be done for each tree; we dont' care about which year it is
  dat.budburst[dat.budburst$individual_id==IND, c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")] <- unique(dat.bind[dat.bind$individual_id==IND,c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")])
  
  for(YR in unique(dat.budburst$year[dat.budburst$individual_id==IND])){
    # creating a handy index for what row we're working with
    row.now <- which(dat.budburst$individual_id==IND & dat.budburst$year==YR)
    
    # Just narrowing the data frame down to just the part we want to work with
    dat.tmp <- dat.bind[dat.bind$individual_id==IND & dat.bind$first_yes_year==YR,]
    
    if(nrow(dat.tmp)==0) next # skips through if there's no data
    
    if(nrow(dat.tmp)==1){
      dat.budburst[row.now, c("first.mean", "first.min", "first.max")] <- dat.tmp$first_yes_doy
      dat.budburst[row.now, c("last.mean", "last.min", "last.max")] <- dat.tmp$last_yes_doy
    } else {
      dat.budburst[row.now, "first.mean"] <- mean(dat.tmp$first_yes_doy, na.rm=T)
      dat.budburst[row.now, "first.min" ] <- min(dat.tmp$first_yes_doy, na.rm=T)
      dat.budburst[row.now, "first.max" ] <- max(dat.tmp$first_yes_doy, na.rm=T)
      
      dat.budburst[row.now, "last.mean"] <- mean(dat.tmp$last_yes_doy, na.rm=T)
      dat.budburst[row.now, "last.min" ] <- min(dat.tmp$last_yes_doy, na.rm=T)
      dat.budburst[row.now, "last.max" ] <- max(dat.tmp$last_yes_doy, na.rm=T)
    }
  }
}

dat.budburst$Yday <- dat.budburst$first.min

#This loop freezes at the end and needs to be manuall stopped but also fully works?
for(YR in unique(dat.budburst$year)){
  start <- paste(as.character(dat.budburst$year), "-01-01", sep="")
  dat.budburst$Date <- as.Date((dat.budburst$Yday-1), origin = start)
}


dim(dat.budburst)

write.csv(dat.budburst, "../data_raw/QURU_ACRU_NPN_combined.csv", row.names=F)

# Creating a point list and time range that matches your MODIS dataset
# Note: This will probably change down the road
NPN.pts <- aggregate(year~site_id+latitude+longitude, data=dat.budburst, 
                     FUN=min)
names(NPN.pts)[4] <- "yr.start"
NPN.pts$yr.end <- aggregate(year~site_id+latitude+longitude, data=dat.budburst, 
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

list.met <- list()
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=NPN.pts$site_id[i], latitude=NPN.pts$latitude[i], longitude=NPN.pts$longitude[i], lat.list[[i]]$data)
}
names(list.met) <-  NPN.pts$site

rm(lat.list) # Removing lat.list to save memory


#Reading in our function for calculating weather statistics of interest
source(file.path(path.hub, "Phenology_Forecasting/scripts/weather_calc.R"))

#Running our function to calculate weather statistics. Default year range is 1975-2019. Growing seaosn is yday 1 to 120

list.met<- lapply(list.met, weather_calc)

lat.calc <- dplyr::bind_rows(list.met)

write.csv(lat.calc, "../data_processed/QURU_ACRU_Daymet_clean_data.csv", row.names=F)

lat.calc <- read.csv("../data_processed/QURU_ACRU_Daymet_clean_data.csv")

dat.budburst$Date <- as.Date(dat.budburst$Date)
lat.calc$Date <- as.Date(lat.calc$Date)

dat.comb <- data.frame()
for(LOC in unique(as.numeric(dat.budburst$site_id))){
  npn.tmp <- dat.budburst[dat.budburst$site_id == LOC,]
  lat.tmp <- lat.calc[lat.calc$site == LOC,]
  npn.tmp$GDD5.cum <- lat.tmp$GDD5.cum[match(npn.tmp$Date, lat.tmp$Date)]
  npn.tmp$GDD0.cum <- lat.tmp$GDD0.cum[match(npn.tmp$Date, lat.tmp$Date)]
  npn.tmp$NCD <- lat.tmp$NCD[match(npn.tmp$Date, lat.tmp$Date)]
  npn.tmp$GTmean <- lat.tmp$GTmean[match(npn.tmp$Date, lat.tmp$Date)]
  dat.comb <- rbind(dat.comb, npn.tmp)
}


dat.comb <- dat.comb[!is.na(dat.comb$Yday),] 
dat.comb <- dat.comb[is.finite(dat.comb$Yday),]
summary(dat.comb)


# Save dat.comb 
write.csv(dat.comb, "../data_processed/QURU_ACRU_NPN_combined.csv", row.names=F)

