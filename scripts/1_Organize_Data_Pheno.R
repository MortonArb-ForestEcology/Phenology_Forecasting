#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Spring Phenoogy Forecasting
# Purpose: This script serves as the initial data download and organization of the arb data
# Inputs: Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
#         The clean_google_form.r script which defines the clean.google function.
#         The group_google_form.r script which defines the group.google function
# Outputs: dat.burst dataframe that can be used for the model
#          dat.leaf dataframe that can be used for the model
# Notes:  Since the original data is kept by the authors, most users will not need to run this script. 
#         The data frames created by this script are saved in this repository and can be used for the proceeding scripts.
#-----------------------------------------------------------------------------------------------------------------------------------#

#loading ggplot for visualization 
library(ggplot2)

path.hub <- "../"

path <- "../data_processed/"
dir.create("../data_processed/", recursive = T, showWarnings = F)

# -----------------------------
# This section is to read in Phenology Monitoring data from our years of interest. THIS SECTION REQUIRES THE clean.google function
# This function below takes in a vector of the genus of interest and a start and end year for the forms you want
# -----------------------------

#Calling in the clean.google function
source(file.path(path.hub, "scripts/Clean_google_form.R"))

#Calling in the group.google function. clean.google is needed for this.
source(file.path(path.hub, "scripts/Group_google.R"))

#Enter the genus of interest as a vector and the final year of observations you want as a variable
#The function will crash if you have a genus included without end years we have a form for i.e("Ulmus" with range 2018:2019)
#This only matters for end years. Start years are adjusted to match the first year the genus has a form.
Genus <- c("Quercus")
StartYear <- 2018
EndYear <- 2021

#Will produce message that look like errors that say 
#"New names: Please choose the correct accession number for the tree you are observing.` -> `Please choose the correct accession number for the tree you are observing....5`"
#But this is a normal sign that the function is working
dat.oak <- group.google(Genus, StartYear, EndYear)

#--------------------------------------------------------#
# Here is where you  pull out phenometrics of interest (bud burst and leaf out)
#--------------------------------------------------------#
dat.oak$Date.Observed <- as.Date(dat.oak$Date.Observed)
dat.oak$Bud <- as.factor(dat.oak$leaf.breaking.buds.observed)
dat.oak$Leaf <- as.factor(dat.oak$leaf.present.observed)
dat.oak <- dat.oak[!is.na(dat.oak$Date.Observed),]

#pulling out bud burst information from out phenology data
dat.oak <- subset(dat.oak, select = c("Date.Observed", "Year", "Species", "Bud", "Leaf", "PlantNumber"))
dat.oak <- dat.oak[dat.oak$Year != "2020",]

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
write.csv(dat.burst, paste0(path, "/Oak_collection_budburst_raw.csv"), row.names=F)
write.csv(dat.leaf, paste0(path, "/Oak_collection_leaf_raw.csv"), row.names=F)


