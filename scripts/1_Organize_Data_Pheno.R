#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the frequentist model
#          Part way through the script it marks where it one can transition to the Bayesian_GDD5-burst.R script
# Inputs: Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
#         The clean_google_form.r script which defines the clean.google function. Found in the Github repository "Phenology_ LivingCollections"
# Outputs: Figure of Frequentist model prediction for bud burst timing of Quercus Macrocarpa for each year
#          dat.comb dataframe that can be used in the Bayesian_GDD5-burst.r script in this repository
# Notes: All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
#        The majority of the rest is currently a modification of that same script by Christy

#-----------------------------------------------------------------------------------------------------------------------------------#

#loading ggplot for visualization 
library(ggplot2)

dir.create("../data_processed/", recursive = T, showWarnings = F)

#-------------------------------------------------#
#This section is for downloaded the met data, pulling out data of interest, and calculating growing degree days
#-------------------------------------------------#

#Setting a shared file path for where the data are
path.met <- "G:/My Drive/Arboretum Met Data/GHCN-Daily"

# Read in the older dataset. This is because the GHCND at the arboretum changed in 2007 and we need to pull from both
met.old <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00119221_1895-2007.csv"))
met.old$DATE <- as.Date(met.old$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.old)

# Read in the newer dataset
met.new <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00115097_2007-04-01_2019-12-31.csv"))
met.new$DATE <- as.Date(met.new$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.new)

# Check to make sure we're in metric (earlier I had a mixture of units and that was bad)
range(met.old$TMAX, na.rm=T)
range(met.new$TMAX, na.rm=T)

# Combine the old and the new datasets into a new data frame.  We don't want all columns, so just take the ones we care about
met.all <- rbind(met.old[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")],
                 met.new[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")])
met.all$YEAR <- lubridate::year(met.all$DATE)
met.all$MONTH <- lubridate::month(met.all$DATE)
met.all$DAY <- lubridate::day(met.all$DATE)
met.all$YDAY <- lubridate::yday(met.all$DATE)
met.all <- met.all[met.all$YEAR>1895 & met.all$YEAR<2020,]
met.all$TMEAN <- (met.all$TMAX + met.all$TMIN)/2
summary(met.all)

# Adding in growing degree-days with base temp of 5
met.all$GDD5 <- ifelse(met.all$TMEAN>5, met.all$TMEAN-5, 0)
met.all$GDD5.cum <- NA
summary(met.all)

# Calculate the cumulative growing degree days for each day/year
for(YR in unique(met.all$YEAR)){
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  
  if(min(dat.tmp$DATE)>as.Date(paste0(YR, "-01-01"))) next
  
  gdd.cum=0
  d.miss = 0
  for(i in 1:nrow(dat.tmp)){
    if(is.na(dat.tmp$GDD5[i]) & d.miss<=7){ #YOU CHANGED THIS TO 7 FOR NOW BUT CHANGE BACK
      d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
      gdd.cum <- gdd.cum+0
    } else {
      d.miss = 0 # reset to 0
      gdd.cum <- gdd.cum+dat.tmp$GDD5[i] 
    }
    
    dat.tmp[i,"GDD5.cum"] <- gdd.cum
  }
  met.all[met.all$YEAR==YR, "GDD5.cum"] <- dat.tmp$GDD5.cum
}
summary(met.all)

# -----------------------------
# This section is to read in Phenology Monitoring data from our years of interest. THIS SECTION REQUIRES THE clean.google function found in the Phenology_LivingCollections repository
# -----------------------------
source("../../Phenology_LivingCollections/scripts/clean_google_form.R")

quercus.18 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2018)
quercus.18$Collection <- as.factor("Quercus")
quercus.18$Year <- lubridate::year(quercus.18$Date.Observed)

quercus.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2019)
quercus.19$Collection <- as.factor("Quercus")
quercus.19$Year <- lubridate::year(quercus.19$Date.Observed)


#Year 2018 has different column names not converted by the clean.google function so this sets them back to equal
colnames(quercus.18) <- as.character(colnames(quercus.19))

#Creating one data frame from them both
dat.pheno <- rbind(quercus.18, quercus.19)

#Enter chosen species here. Genus must be capitalized, one space between genus and species, and species is lower case
chosen <- "Quercus macrocarpa"

#Seperating out our chosen species
dat.oak <- dat.pheno[(dat.pheno$Species == chosen),]
dat.oak$Date.Observed <- as.Date(dat.oak$Date.Observed)
dat.oak$Bud <- as.factor(dat.oak$leaf.buds.observed)
dat.oak <- dat.oak[!is.na(dat.oak$Date.Observed),]

#pulling out bud burst information from out phenology data
dat.oak <- subset(dat.oak, select = c("Date.Observed", "Year", "Species", "Bud", "PlantNumber"))


#Creating final frame containing the first burst for each year.
dat.burst <- aggregate(dat.oak[dat.oak$Bud=="Yes", "Date.Observed"], 
                       by=dat.oak[dat.oak$Bud=="Yes", c("Species", "PlantNumber", "Year")], 
                       FUN=min)

#Adding in arboretum Lat and Long
dat.burst$Latitude <- 41.8164
dat.burst$Longitude <- -88.0549

#Making easier names for the data frameYday",
colnames(dat.burst) <- c("Species", "PlantNumber", "Year", "Date", "Latitude", "Longitude")
dat.burst$Yday <- lubridate::yday((dat.burst$Date))


#Relic from when this combined this data with npn. keeping since it will return.
dat.comb <- dat.burst

dat.comb$Location <- paste(dat.comb$Latitude, dat.comb$Longitude, sep= " ")

#Creating a new column in our phenology data frame that takes the date of earliest burst and gives us the cumulative gdd of that date from the met data
dat.comb$GDD5.cum <- NA
for(DAT in paste(dat.comb$Date)){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.comb[dat.comb$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

#Removing some outliers for now so sd doesn't go negative. REMEMBER TO COME BACK AND CHANGE THIS
dat.comb[dat.comb$Yday>=240, c("Yday", "GDD5.cum")] <- NA

# Save dat.comb 
write.csv(dat.comb, "../data_processed/Phenology_Met_combined.csv", row.names=F)
