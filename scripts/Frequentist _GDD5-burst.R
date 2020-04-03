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
    if(is.na(dat.tmp$GDD5[i]) & d.miss<=3){
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

#Creating a new column in our phenology data folder that takes the date of earliest burst and gives us the cumulative gdd of that date from the met data
dat.comb$GDD5.cum <- NA
for(DAT in paste(dat.comb$Date)){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.comb[dat.comb$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

#Removing some outliers for now so sd doesn't go negative. REMEMBER TO COME BACK AND CHANGE THIS
dat.comb[dat.comb$Yday>=240, c("Yday", "GDD5.cum")] <- NA

#---------------------------------------------------------------------#
#If only running the Bayesian model than you can stop here. These may become fully seperate scripts down the line
#---------------------------------------------------------------------#

# Testing whether GDD5 is a good predictor of day 
dat.gdd5.lm <- lm(Yday ~ GDD5.cum, data=dat.comb)
summary(dat.gdd5.lm)
plot(Yday ~ GDD5.cum, data=dat.comb)
abline(dat.gdd5.lm, col="red")


#Box plot of dat.burst to see the distribution. Macrocarpa has an outlier but seems plausible 
ggplot(data=dat.comb) +
  geom_boxplot(aes(x=Species, y=GDD5.cum, fill=Species)) +
  scale_y_continuous(name="Accumulated Warming") +
  scale_fill_manual(name="Species", values=c("deeppink2", "mediumpurple1")) +
  guides(fill=F) +
  theme(panel.background=element_rect(fill=NA, color="black"),
        legend.text = element_text(face="italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face="bold.italic", color="black", size=rel(3)),
        axis.title.y=element_text(face="bold", color="black", size=rel(3.5)),
        axis.text.y=element_text(color="black", size=rel(2)))


#Creating a dataframe with the mean values of these metrics for every year
dat.yr <- aggregate(met.all[,c("TMAX", "TMIN", "TMEAN", "PRCP", "SNOW")],
                    by=met.all[,c("STATION", "YEAR")],
                    FUN=mean, na.rm=T)

#removing years too before reliable measurments
dat.yr <- dat.yr[dat.yr$YEAR>=1922,]

#Creating a matrix of the right size to be filled with the distribution of predictions for that year
mat.yr <- array(dim=c(nrow(dat.yr), 1000))
dimnames(mat.yr)[[1]] <- dat.yr$YEAR

#---------------------------------------------------------#
#This section is for calculating mean and sd of our observed data to base our model distribtuion off of
#---------------------------------------------------------#
#loading in nlme and lme4 for their linear mixed effect equations
library(nlme); library(lme4)

#creating a mean and sd for gdd
dat.gdd5.mean <- mean(dat.comb[,"GDD5.cum"], na.rm=T); 
dat.gdd5.sd <- sd(dat.comb[,"GDD5.cum"], na.rm=T)

#Creating a hierarchial mean to compare
mac.cue <- lme(GDD5.cum ~ 1, random=list(Year=~1, PlantNumber=~1), data=dat.comb, na.action=na.omit)
mac.summ <- summary(mac.cue)
MuMIn::r.squaredGLMM(mac.cue)
mod.cue.est <- mac.summ$tTable[,"Value"] # Hierarchical mean
mod.cue.se <- mac.summ$tTable[,"Std.Error"]
mod.cue.sd <- mac.summ$tTable[,"Std.Error"]*sqrt(mac.summ$tTable[,"DF"]+1)

# Compare to non-hierarchical
mod.cue.est; mod.cue.sd
dat.gdd5.mean; dat.gdd5.sd 

#Creating the distribtuion of gdd5 values to be ran through the calc.bud function
dat.gdd5.vec <- rnorm(1000, dat.gdd5.mean, dat.gdd5.sd)

#Function used to calculate bud burst day using gdd5
calc.bud <- function(x){min(dat.tmp[which(dat.tmp$GDD5.cum >= x),"YDAY"])}

#Filling a matrix with yday values calculating by running calc.bud on a distribution of gdd5.cum values at bud burst (dat.gdd5.vec)
i <- 1
for(i in 1:nrow(dat.yr)){
  YR=dat.yr$YEAR[i]
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  # summary(dat.tmp)
  if(nrow(dat.tmp)==0) next
  
  # Bloom time -- simple
  bud.pred <- calc.bud(dat.gdd5.mean)
  if(bud.pred != Inf) dat.yr[i,"bud.oak"] <- bud.pred
  bud.vec <- unlist(lapply(dat.gdd5.vec, calc.bud))
  bud.vec[bud.vec==Inf] <- NA
  
  mat.yr[i,] <- bud.vec
}


#---------------------------------------------------------#
#This section is for summarizing the model data and observed data so they can be visualized and compared
#---------------------------------------------------------#
#Calculating summary statistics of the matrix of values from each year
dat.yr$bud.mean <- apply(mat.yr, 1, mean, na.rm=T)
dat.yr$bud.sd   <- apply(mat.yr, 1, sd, na.rm=T)
dat.yr$bud.lb   <- apply(mat.yr, 1, quantile, 0.025, na.rm=T)
dat.yr$bud.ub   <- apply(mat.yr, 1, quantile, 0.975, na.rm=T)
summary(dat.yr) 


#Aggregating all indivudla measurements into one mean for the every year
oak.bud <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                        by=list(dat.comb$Year),
                        FUN=mean, na.rm=F)


#Calculating sd values to put on our visualizations
oak.bud[,c("Yday.sd", "GDD5.cum.sd")]  <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                                                       by=list(dat.comb$Year),
                                                       FUN=sd, na.rm=F)[,c("Yday", "GDD5.cum")]


#Graphing the output of the model vs. the observed data
ggplot(data=dat.yr[,]) +
  geom_ribbon(data=dat.yr[,], aes(x=YEAR, ymin=bud.lb, ymax=bud.ub, fill="Modeled"), alpha=0.5) +
  geom_point(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) +
  geom_line(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) + 
  geom_pointrange(data=oak.bud, aes(x=Group.1, y=Yday, ymin=Yday-Yday.sd, ymax=Yday+Yday.sd,color="Observed"))+
  ggtitle("Modeled and Observed day of year of budburst for Quercus Macrocarpa")+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_text(size=rel(2.5), color="black"),
        axis.title=element_text(size=rel(2.5), face="bold"),)

