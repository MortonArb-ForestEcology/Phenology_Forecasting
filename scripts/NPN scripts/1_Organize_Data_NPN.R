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


#Setting the points to download the daymet data from
path.doc <- "C:/Users/lucie/Documents/NPN_data/"
species <- "Chosen_Oaks"
ystart <- min(dat.npn$Year)

#make sure the yend of the data matches what you enter. Sometimes daymet truncates and this varibale will become wrong later in the script
yend <- max(dat.npn$Year)

pointsfile <- paste(species, "_npn_points.csv", sep="")

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- dat.npn[,(c=1:2)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]
q.lat <- unique(q.lat)

#Writing the csv file of lat and longs because batch function needs to read a file instead of a dataframe
write.csv(q.lat, file.path(path.doc, file = pointsfile), row.names=FALSE)

setwd(path.doc)
#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = pointsfile,
                                           start = ystart,
                                           end = yend,
                                           internal = T)

#removing failed downloads 
lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]


#----------------------------------------------------#
#This is not strictly for npn data but how the loop for the model must change when it is included.
#----------------------------------------------------#

#Start of loop to pull out the GDD5.cum of the bud burst date fore every tree and location
#Progress bar

yrlength <- ((yend-ystart)+1)
pb <- txtProgressBar(min=0, max=length(lat.list)*yrlength, style=3)
pb.ind=0

#Creating a dataframe to hold weather summary statistics
df.loc <- data.frame(latitude=rep(lat.list[[1]]$latitude, 365* yrlength) ,
                     longitude=rep(lat.list[[1]]$longitude, 365 * yrlength))

#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 1
i <- 1
YR <- ystart
gcount <- 1
for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  df.tmp$GDD5.cum <- NA
  df.tmp$NCD <- NA
  df.tmp$GTmean <- NA
  
  #Setting the parameters of the growing season length for the growing season temp mean
  g_start <- 1
  g_end <- 120
  met.gtmean <- df.tmp[(df.tmp$yday>=g_start & df.tmp$yday<=g_end), ]
  
  #Calculating the mean temp of the growing season for each years
  for(YR in unique(met.gtmean$year)){
    dat.tmp <- met.gtmean[met.gtmean$year==YR, ]
    dat.tmp$GTmean <- mean(dat.tmp$TMEAN, na.rm = TRUE)
    df.loc[gcount:(gcount+364), "GTmean"] <- rep(dat.tmp[1, "GTmean"], 365)
    gcount <- gcount + 365
  }
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
    
    df.yr <- df.tmp[df.tmp$year==YR,]
    start <- paste(as.character(df.tmp$year), "-01-01", sep="")
    df.tmp$Date <- as.Date((df.yr$yday-1), origin = start)
    
    g_end = 120
    gdd.cum=0
    d.miss = 0
    ncd = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[j]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      if(!is.na(df.yr$TMEAN[j]) & df.yr$TMEAN[j] < 0){
        ncd <- ncd + 1
      }
      df.yr[j, "NCD"] <- ncd
      df.yr[j,"GDD5.cum"] <- gdd.cum
    }
    df.tmp[df.tmp$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
    df.tmp[df.tmp$year==YR, "NCD"] <- df.yr$NCD
  }
  
  df.loc[count:((count-1) + (365 * yrlength)), "latitude"] <- lat.list[[i]]$latitude
  df.loc[count:((count-1) + (365 * yrlength)), "longitude"] <- lat.list[[i]]$longitude
  df.loc[count:((count-1) + (365 * yrlength)), "year"] <- df.tmp$year
  df.loc[count:((count-1) + (365 * yrlength)), "TMAX"] <- df.tmp$tmax..deg.c.
  df.loc[count:((count-1) + (365 * yrlength)), "TMIN"] <- df.tmp$tmin..deg.c.
  df.loc[count:((count-1) + (365 * yrlength)), "TMEAN"] <- df.tmp$TMEAN
  df.loc[count:((count-1) + (365 * yrlength)), "PRCP"] <- df.tmp$prcp..mm.day.
  df.loc[count:((count-1) + (365 * yrlength)), "SNOW"] <- df.tmp$srad..W.m.2.
  df.loc[count:((count-1) + (365 * yrlength)), "YDAY"] <- df.tmp$yday
  df.loc[count:((count-1) + (365 * yrlength)), "Date"] <- df.tmp$Date
  df.loc[count:((count-1) + (365 * yrlength)), "GDD5"] <- df.tmp$GDD5
  df.loc[count:((count-1) + (365 * yrlength)), "GDD5.cum"] <- df.tmp$GDD5.cum
  df.loc[count:((count-1) + (365 * yrlength)), "NCD"] <- df.tmp$NCD
  
  count <- count + (365 * yrlength)
  
}
setwd("../")
write.csv(df.loc, "../data_processed/Daymet_clean_data.csv", row.names=F)
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

