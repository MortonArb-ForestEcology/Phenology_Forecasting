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
# This function below takes in a vector of the genus of interest and a start and end year for the forms you want
# -----------------------------
path.hub <- "C:/Users/lucie/Documents/GitHub/"

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

#-----------------------------------------------------------------#
#HERE you make choices about the species and phenophses
#-----------------------------------------------------------------#
#Choosing the species of interest here.
species <- c("Acer saccharum", "Acer saccharinum", "Acer rubrum", "Quercus macrocarpa", "Quercus alba", "Quercus rubra")

#Choosing the Phenophase of interest here. Name is stored as a variable then placed in a list for a loop later on. Make sure it matches the column name
Bud <- "Bud"
Flower <- "Flower"
#Pollen <- "Pollen"
#Fruit <- "Fruit"
#Drop <- "Drop"
Pheno <- c(Bud, Flower)

#Renaming and picking out your phenophases of interest. REMEMBER to matchw hat you seleted in the last step
dat.pheno[[Bud]] <- as.factor(dat.pheno$leaf.buds.observed)
dat.pheno[[Flower]] <- as.factor(dat.pheno$flower.open.observed)
#dat.pheno[[Pollen]] <- as.factor(dat.pheno$flower.pollen.observed)
#dat.pheno[[Fruit]] <- as.factor(dat.pheno$fruit.present.observed)
#dat.pheno[[Drop]] <- as.factor(dat.pheno$fruit.drop.observed)

#Pulling our species of interest
dat.chosen <- dat.pheno[dat.pheno$Species %in% species, ]
dat.chosen <- dat.chosen[!is.na(dat.chosen$Date.Observed),]


#pulling out bud burst information from out phenology data. MAKE SURE this includes the phenophases you picked out earlier
dat.chosen <- subset(dat.chosen, select = c("Date.Observed", "Year", "Species", "PlantNumber", "Bud", "Flower"))
dat.chosen$Species <- as.character(dat.chosen$Species)

#----------------------------------------------------------------#
#If you are interested in bud burst
#----------------------------------------------------------------#
#Creating final frame containing the first burst for each year.
sum.df <- data.frame()
for(i in Pheno){
  dat.burst <- aggregate(dat.chosen[dat.chosen[[i]]=="Yes", "Date.Observed"], 
                        by=dat.chosen[dat.chosen[[i]]=="Yes", c("Species", "PlantNumber", "Year")], 
                        FUN=min)

  #Making easier names for the data frame
  colnames(dat.burst) <- c("Species", "PlantNumber", "Year", "Date")
  dat.burst$yday <- lubridate::yday((dat.burst$Date))


  #Creating a new column in our phenology data frame that takes the date of earliest phenophase and gives us the cumulative gdd of that date from the met data
  dat.burst$GDD5.cum <- NA
  for(DAT in paste(dat.burst$Date)){
    if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
    dat.burst[dat.burst$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
  }

  #Final frame for the summary statistics relating to phenophase
  #This needs to do.call because otherwise it gives a dataframe containing matrices that are hard to work with
  dat.burst <- do.call(data.frame, aggregate(cbind(yday, GDD5.cum) ~ Species, 
                                             data = dat.burst, FUN = function(x) c(mean = mean(x), sd = sd(x))))
  dat.burst$Pheno <- i
  sum.df <- rbind(sum.df, dat.burst)
}

#This dataframe contains the yday and gdd5.cum mean and sd for every species.
#Currently is split by the Pheno type. Slightly different than original. Should be easy to tweak to desires
sum.df

