#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To create a table of values of our species of interest for visualizations. May become a script for visualizations in general
# Inputs: Quercus 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
#         The clean_google_form.r script which defines the clean.google function. Found in the Github repository "Phenology_ LivingCollections"
#         Old metstation data from 1895-2007 found in the "Arboretum Met Data/GHCN-Daily" google drive folder
#         New metstation data from 2007-present found in the "Arboretum Met Data/GHCN-Daily" google drive folder
# Outputs: A table of the mean + SD (or 95% CI) for GDD threshold & day of year for chosen species. MAybe more in the future
# Notes: This script currently requires manual changes depending on which species and phenophases you are interested in. Since it calculates
#        the summary statistics for the different phases in seperate data frames that are later combined if you wanted to remove or add a
#        phenophase it would require many changes. Now that the out put is working I'll try and make it more future proof.


#loading ggplot for visualization 
library(ggplot2)

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
# This section is to read in Phenology Monitoring data from our years of interest. THIS SECTION REQUIRES THE clean.google function
# -----------------------------

quercus.18 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2018)
quercus.18$Collection <- as.factor("Quercus")
quercus.18$Year <- lubridate::year(quercus.18$Date.Observed)

quercus.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2019)
quercus.19$Collection <- as.factor("Quercus")
quercus.19$Year <- lubridate::year(quercus.19$Date.Observed)

acer.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Acer", dat.yr=2019)
acer.19$Collection <- as.factor("Acer")
acer.19$Year <- lubridate::year(acer.19$Date.Observed)


#Year 2018 has different column names not converted by the clean.google function so this sets them back to equal
colnames(quercus.18) <- as.character(colnames(quercus.19))

#Creating one data frame from them both
dat.pheno <- rbind(quercus.18, quercus.19, acer.19)

#Choosing the species of interest
species <- c("Acer saccharum", "Acer saccharinum", "Acer rubrum", "Quercus macrocarpa", "Quercus alba", "Quercus rubra")

dat.chosen <- dat.pheno[dat.pheno$Species %in% species, ]
dat.chosen <- dat.chosen[!is.na(dat.chosen$Date.Observed),]
dat.chosen$Bud <- as.factor(dat.chosen$leaf.buds.observed)
dat.chosen$Flower <- as.factor(dat.chosen$flower.open.observed)

#pulling out bud burst information from out phenology data
dat.chosen <- subset(dat.chosen, select = c("Date.Observed", "Year", "Species", "Bud", "Flower", "PlantNumber"))
dat.chosen$Species <- as.character(dat.chosen$Species)

#----------------------------------------------------------------#
#If you are interested in bud burst
#----------------------------------------------------------------#

#Creating final frame containing the first burst for each year.
dat.burst <- aggregate(dat.chosen[dat.chosen$Bud=="Yes", "Date.Observed"], 
                       by=dat.chosen[dat.chosen$Bud=="Yes", c("Species", "PlantNumber", "Year")], 
                       FUN=min)

#Making easier names for the data frame
colnames(dat.burst) <- c("Species", "PlantNumber", "Year", "Date")
dat.burst$burst.yday <- lubridate::yday((dat.burst$Date))


#Creating a new column in our phenology data frame that takes the date of earliest burst and gives us the cumulative gdd of that date from the met data
dat.burst$burst.GDD5.cum <- NA
for(DAT in paste(dat.burst$Date)){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.burst[dat.burst$Date==as.Date(DAT),"burst.GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

#Final frame for the summary statistics relating to bud burst
#This needs to do.call because otherwise it gives a dataframe containing matrices that are hard to work with
dat.burst <- do.call(data.frame, aggregate(cbind(burst.yday, burst.GDD5.cum) ~ Species, data = dat.burst, FUN = function(x) c(mean = mean(x), sd = sd(x) ) ))


#------------------------------------------------------------------#
#If you are interested in flower timing
#------------------------------------------------------------------#

#Working with flower
dat.flower <- aggregate(dat.chosen[dat.chosen$Flower=="Yes", "Date.Observed"], 
                        by=dat.chosen[dat.chosen$Flower=="Yes", c("Species", "PlantNumber", "Year")], 
                        FUN=min)

#Making easier names for the data frameYday",
colnames(dat.flower) <- c("Species", "PlantNumber", "Year", "Date")
dat.flower$flower.yday <- lubridate::yday((dat.flower$Date))


#Creating a new column in our phenology data frame that takes the date of earliest flower and gives us the cumulative gdd of that date from the met data
dat.flower$flower.GDD5.cum <- NA
for(DAT in paste(dat.flower$Date)){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.flower[dat.flower$Date==as.Date(DAT),"flower.GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

#This needs to do.call because otherwise it gives a dataframe containing matrices that are hard to work with
dat.flower <- do.call(data.frame, aggregate(cbind(flower.yday, flower.GDD5.cum) ~ Species, data = dat.flower, FUN = function(x) c(mean = mean(x), sd = sd(x))))

#Creating the final data frame that is the output of this script
#Each species has a mean and sd calculated for the day of year and gdd5 threshold for both bud burst and flower open
sum.df <- merge(dat.burst, dat.flower, by = "Species")




