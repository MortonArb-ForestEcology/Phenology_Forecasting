#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick & Christy Rollinson
# Project: Spring Phenoogy Forecasting
# Purpose: This script serves as the initial data download and organization of the arb data
# Inputs: Organized & Cleaned phenology data from the Oak Collection; labeled 
#         "LivingCollectionPhenology_Observation_Data_[Collection]_[Year]_Final.csv
#         This data lives in Google Drive in LivingCollections_Phenology/Data_Observations
# Outputs: dat.burst dataframe that can be used for the model
#          dat.leaf dataframe that can be used for the model
# Notes:  Since the original data is kept by the authors, most users will not need to run this script. 
#         The data frames created by this script are saved in this repository and can be used for the proceeding scripts.
#-----------------------------------------------------------------------------------------------------------------------------------#

#loading ggplot for visualization 
library(ggplot2)

path.hub <- "../"
#path.hub <- "/Users/jocelyngarcia/Documents/GitHub/Phenology_Forecasting"
pathDatOut <- "../data_processed/"
# path <- "/Users/jocelyngarcia/Documents/GitHub/Phenology_Forecasting/data_processed"
dir.create(pathDatOut, recursive = T, showWarnings = F)

pathRaw <- "~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/"

# -----------------------------
# Pulling from our cleaned, finalized data 
# -----------------------------

Genus <- c("Quercus")
StartYear <- 2018
EndYear <- 2024

# Getting a list of the data we have available to model
fGen <- dir(pathRaw, paste("ObservationData", Genus, sep="_"))
fGen <- fGen[grep("FINAL", fGen)]

dat.oak <- data.frame()
for(i in 1:length(fGen)){
  fNow <- read.csv(file.path(pathRaw, fGen[i]))
  dat.oak <- rbind(dat.oak, fNow)
}
summary(dat.oak)
length(unique(dat.oak$Species))
summary(as.factor(dat.oak$Species))

# Cleaning up some species name stuff
dat.oak$Species <- gsub("  ", " ", dat.oak$Species)
dat.oak$PlantNumber <- gsub(" ", "", dat.oak$PlantNumber)
dat.oak$PlantNumber <- gsub("2390-26 *1", "2390-26*1", dat.oak$PlantNumber)

summary(as.factor(dat.oak$Species[dat.oak$Year==2023]))
summary(as.factor(dat.oak$Species[dat.oak$Year==2024]))

# Check names against BRAHMS records
quercusList2023 <- read.csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Quercus/ObservingList_Quercus_2023.csv")
quercusList2023$PlantNumber <- quercusList2023$PlantID
summary(quercusList2023)
dat.oak <- merge(dat.oak, quercusList2023[,c("PlantNumber", "Taxon")], all.x=T, all.y=F)
dim(dat.oak)
summary(dat.oak)

dat.oak$TaxonMatch <- dat.oak$Species==dat.oak$Taxon
dat.oak$Species[!dat.oak$TaxonMatch & !is.na(dat.oak$TaxonMatch)] <- dat.oak$Taxon[!dat.oak$TaxonMatch & !is.na(dat.oak$TaxonMatch)]
summary(dat.oak)

# summary(as.factor(dat.oakTest$Species[!dat.oakTest$TaxonMatch & !(is.na(dat.oakTest$TaxonMatch))]))
# summary(as.factor(dat.oakTest$Species[is.na(dat.oakTest$TaxonMatch)]))
# summary(as.factor(dat.oakTest$PlantNumber[is.na(dat.oakTest$TaxonMatch)]))
# dat.oakTest[!is.na(dat.oakTest$PlantNumber) & dat.oakTest$PlantNumber=="304-92-2", ]

# dat.oakTest[is.na(dat.oakTest$PlantNumber), ]
#  unique(dat.oakTest$PlantNumber)
# 
oak.spp <- unique(dat.oak$Species)
oak.spp[order(oak.spp)]
  


#--------------------------------------------------------#
# Here is where you  pull out phenometrics of interest (bud burst and leaf out)
#--------------------------------------------------------#
dat.oak$Date.Observed <- as.Date(dat.oak$Date.Observed)
dat.oak$Bud <- as.factor(dat.oak$leaf.breaking.buds.observed)
dat.oak$Leaf <- as.factor(dat.oak$leaf.present.observed)
dat.oak <- dat.oak[!is.na(dat.oak$Date.Observed),]

#pulling out bud burst information from out phenology data
dat.oak <- subset(dat.oak, select = c("Date.Observed", "Year", "Species", "Bud", "Leaf", "PlantNumber"))
dat.oak <- dat.oak[dat.oak$Year != "2020",] # Exclude 2020 because COVID disrupted spring obs
summary(dat.oak)

#Creating final frame containing the first burst for each year.
dat.burst <- aggregate(dat.oak[dat.oak$Bud=="Yes", "Date.Observed"], 
                       by=dat.oak[dat.oak$Bud=="Yes", c("Species", "PlantNumber", "Year")], 
                       FUN=min)

#Creating final frame containing the first burst for each year.
dat.leaf <- aggregate(dat.oak[dat.oak$Leaf=="Yes", "Date.Observed"], 
                      by=dat.oak[dat.oak$Leaf=="Yes", c("Species", "PlantNumber", "Year")], 
                      FUN=min)

#Adding in arboretum Lat and Long
dat.burst$Latitude <- 41.8164
dat.burst$Longitude <- -88.0549
dat.burst$Site <- "Morton"

#Adding in arboretum Lat and Long
dat.leaf$Latitude <- 41.8164
dat.leaf$Longitude <- -88.0549
dat.leaf$Site <- "Morton"

#Making easier names for the data frameYday",
colnames(dat.burst) <- c("Species", "PlantNumber", "Year", "Date", "Latitude", "Longitude", "Site")
dat.burst$Yday <- lubridate::yday((dat.burst$Date))

#Making easier names for the data frameYday",
colnames(dat.leaf) <- c("Species", "PlantNumber", "Year", "Date", "Latitude", "Longitude", "Site")
dat.leaf$Yday <- lubridate::yday((dat.leaf$Date))

#Creating raw data output
write.csv(dat.burst, file.path(pathDatOut, "Oak_collection_budburst_raw.csv"), row.names=F)
write.csv(dat.leaf, file.path(pathDatOut, "Oak_collection_leaf_raw.csv"), row.names=F)

