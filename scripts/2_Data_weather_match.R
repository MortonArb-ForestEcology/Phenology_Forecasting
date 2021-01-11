
#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Spring phenology forecasting
# Purpose: This script serves as the download of weather data and the calculation of relevant weather statistics
# Inputs: The weather_calc.r script which defines the weather_calc function.
# Outputs: lat.calc dataframe that can be used to match weather to phenological events
# Notes:
#-----------------------------------------------------------------------------------------------------------------------------------#
path.g <- "G:/My Drive"
# path.g <- "/Volumes/GoogleDrive/My Drive"
path.hub <- ".."
path.data <- "../data_processed/"


path.daymet <- "../data_raw/DAYMET"
if(!dir.exists(path.daymet)) dir.create(path.daymet, recursive = T)

dat.burst <- read.csv(paste0(path.data, "/Oak_collection_budburst_raw.csv"))
dat.leaf <- read.csv(paste0(path.data, "/Oak_collection_leaf_raw.csv"))
dat.comb <- rbind(dat.burst, dat.leaf)


# Note: This will probably change down the road
NPN.pts <- aggregate(Year~Site+Latitude+Longitude, data=dat.burst, 
                     FUN=min)
names(NPN.pts)[4] <- "yr.start"
NPN.pts$yr.end <- aggregate(Year~Site+Latitude+Longitude, data=dat.burst, 
                            FUN=max)[,4]
NPN.pts


#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(NPN.pts, file.path(path.daymet, "NPN_points.csv"), row.names=FALSE)

# if(!dir.exist(path.daymet)) dir.create(path.daymet)
#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.daymet, "NPN_points.csv"),
                                           start = 2000,
                                           end = 2019,
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
  list.met[[i]] <- data.frame(site=NPN.pts$Site[i], latitude=NPN.pts$Latitude[i], longitude=NPN.pts$Longitude[i], lat.list[[i]]$data)
}
names(list.met) <-  NPN.pts$site

rm(lat.list) # Removing lat.list to save memory


#Reading in our function for calculating weather statistics of interest
source(file.path(path.hub, "scripts/Weather_calc.R"))

#Running our function to calculate weather statistics. Default year range is 1975-2019. Growing seaosn is yday 1 to 120

list.met<- lapply(list.met, weather_calc)

lat.calc <- dplyr::bind_rows(list.met)

write.csv(lat.calc, "../data_processed/Arb_Daymet_clean_data.csv", row.names=F)


met.all <- read.csv("../data_processed/Arb_Daymet_clean_data.csv")


dat.leaf <- dat.leaf[dat.leaf$Yday < 200,]

# dir.create("../data_processed/", recursive = T, showWarnings = F)

#Creating a new column in our phenology data frame that takes the date of earliest burst and gives us the cumulative gdd of that date from the met data
dat.burst$GDD5.cum <- met.all$GDD5.cum[match(dat.burst$Date, met.all$Date)]
dat.burst$GDD0.cum <- met.all$GDD0.cum[match(dat.burst$Date, met.all$Date)]
dat.burst$NCD <- met.all$NCD[match(dat.burst$Date, met.all$Date)]
dat.burst$GTmean <- met.all$GTmean[match(dat.burst$Date, met.all$Date)]
dat.burst$PTTGDD.cum <- met.all$PTTGDD.cum[match(dat.burst$Date, met.all$Date)]


dat.leaf$GDD5.cum <- met.all$GDD5.cum[match(dat.leaf$Date, met.all$Date)]
dat.leaf$GDD0.cum <- met.all$GDD0.cum[match(dat.leaf$Date, met.all$Date)]
dat.leaf$NCD <- met.all$NCD[match(dat.leaf$Date, met.all$Date)]
dat.leaf$GTmean <- met.all$GTmean[match(dat.leaf$Date, met.all$Date)]
dat.leaf$PTTGDD.cum <- met.all$PTTGDD.cum[match(dat.leaf$Date, met.all$Date)]


#Loop removing any
for(SP in unique(dat.burst$Species)){
  dat.sp <- dat.burst[dat.burst$Species == SP,]
  for(YR in unique(dat.sp$Year)){
    dat.yr <- dat.sp[dat.sp$Year == YR,]
    dat.sp[dat.sp$Year == YR, "sp_ind_per_year"] <- length(unique(dat.yr$PlantNumber))
    #setting the Threshold. Current need 2 individuals budburst or leaf out per year
    if(length(unique(dat.yr$PlantNumber))<2){
      #0 notating it doesn't meet the threshold
      dat.sp[dat.sp$Year == YR, "obs_check"] <- 0
    }
    else{
      dat.sp[dat.sp$Year == YR, "obs_check"] <- 1
    }
  }
  
  dat.burst[dat.burst$Species == SP, "sp_ind_per_year"] <- dat.sp$sp_ind_per_year
  dat.burst[dat.burst$Species == SP, "nIndividuals"] <- max(dat.sp$sp_ind_per_year)
  #Making sure the whole species is removed even if only one year is bad
  dat.burst[dat.burst$Species == SP, "obs_check"] <- min(dat.sp$obs_check)
}

dat.burst <- dat.burst[dat.burst$obs_check > 0,]


for(SP in unique(dat.leaf$Species)){
  dat.sp <- dat.leaf[dat.leaf$Species == SP,]
  for(YR in unique(dat.sp$Year)){
    dat.yr <- dat.sp[dat.sp$Year == YR,]
    dat.sp[dat.sp$Year == YR, "sp_ind_per_year"] <- length(unique(dat.yr$PlantNumber))
    #setting the Threshold. Current need 2 individuals budburst or leaf out per year
    if(length(unique(dat.yr$PlantNumber))<2){
      #0 notating it doesn't meet the threshold
      dat.sp[dat.sp$Year == YR, "obs_check"] <- 0
    }
    else{
      dat.sp[dat.sp$Year == YR, "obs_check"] <- 1
    }
  }
  
  dat.leaf[dat.leaf$Species == SP, "sp_ind_per_year"] <- dat.sp$sp_ind_per_year
  dat.leaf[dat.leaf$Species == SP, "nIndividuals"] <- max(dat.sp$sp_ind_per_year)
  #Making sure the whole species is removed even if only one year is bad
  dat.leaf[dat.leaf$Species == SP, "obs_check"] <- min(dat.sp$obs_check)
}

dat.leaf <- dat.leaf[dat.leaf$obs_check > 0,]


# Save modified data

write.csv(dat.burst, "../data_processed/Oak_collection_budburst.csv", row.names=F)
write.csv(dat.leaf, "../data_processed/Oak_collection_leaf.csv", row.names=F)
