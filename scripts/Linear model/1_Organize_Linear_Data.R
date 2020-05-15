#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the initial data download, crosswalking, and orgnaizaiton needed for and the linear model input
# Inputs: Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
#         The clean_google_form.r script which defines the clean.google function. Found in the Github repository "Phenology_ LivingCollections"
# Outputs:dat.comb dataframe that can be used in the linear regression models in this repository
# Notes: All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
#-----------------------------------------------------------------------------------------------------------------------------------#


#loading ggplot for visualization 
library(ggplot2)

path.g <- "G:/My Drive"
path.g <- "/Volumes/GoogleDrive/My Drive"
dir.create("../data_processed/", recursive = T, showWarnings = F)

#-------------------------------------------------#
#This section is for downloaded the met data, pulling out data of interest, and calculating growing degree days
#-------------------------------------------------#

#Setting a shared file path for where the data are
path.met <- file.path(path.g, "Arboretum Met Data/GHCN-Daily")

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


# Calculating the Tmean for the growing season of that year

g_start <- 1
g_end <- 120

met.all <- met.all[(met.all$YDAY>=g_start & met.all$YDAY<=g_end), ]

for(YR in unique(met.all$YEAR)){
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  dat.tmp$GTmean <- mean(dat.tmp$TMEAN, na.rm = TRUE)
  met.all[met.all$YEAR==YR, "GTmean"] <- dat.tmp$GTmean
}

summary(met.all)
write.csv(met.all, "../data_processed/GHCN_met_all_linear.csv", row.names=F)
# -----------------------------
# This section is to read in Phenology Monitoring data from our years of interest. THIS SECTION REQUIRES THE clean.google function
# This function below takes in a vector of the genus of interest and a start and end year for the forms you want
# -----------------------------
path.hub <- "C:/Users/lucie/Documents/GitHub/"
path.hub <- "../.."

#Calling in the clean.google function
source(file.path(path.hub, "Phenology_LivingCollections/scripts/clean_google_form.R"))

#Calling in the group.google function. clean.google is needed for this.
source(file.path(path.hub, "Phenology_Forecasting/scripts/Group_google.R"))

#Enter the genus of interest as a vector and the final year of observations you want as a variable
#The function will crash if you have a genus included without end years we have a form for i.e("Ulmus" with range 2018:2019)
#This only matters for end years. Start years are adjusted to match the first year the genus has a form.
Genus <- c("Quercus", "Acer")
StartYear <- 2018
EndYear <- 2019

dat.pheno <- group.google(Genus, StartYear, EndYear)
#--------------------------------------------------------#
# Here is where you pick out your species of interest
#--------------------------------------------------------#
#Enter chosen species here. Genus must be capitalized, one space between genus and species, and species is lower case
# species <- c("Quercus macrocarpa")
# species <- c("Quercus macrocarpa", "Quercus alba", "Acer rubrum", "Acer saccharum")

#Seperating out our chosen species
# dat.oak <- dat.pheno[dat.pheno$Species %in% species, ]
dat.oak <- dat.pheno
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

dat.comb$GTmean <- NA
for(DAT in paste(dat.comb$Date)){
    YR <- lubridate::year(DAT)
    dat.comb[dat.comb$Date==as.Date(DAT),"GTmean"] <- mean(met.all[met.all$YEAR == YR, "GTmean"])
  }

#Removing some outliers for now so sd doesn't go negative. REMEMBER TO COME BACK AND CHANGE THIS
dat.comb[dat.comb$Yday>=171, c("Yday", "GTmean")] <- NA
summary(dat.comb)

# Save dat.comb 
write.csv(dat.comb, "../data_processed/Phenology_Met_combined_linear.csv", row.names=F)
