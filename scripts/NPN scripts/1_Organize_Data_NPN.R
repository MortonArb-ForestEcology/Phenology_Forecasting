#Script for the integration of historical npn data into our model

#dplyr for the summarise function
library(dplyr)

#Retrieving npn data
dat.npn <- read.csv("C:/Users/lucie/Documents/NPN_data/NPN_Quercus.csv")
chosen <- c("Quercus imbricaria", "Quercus falcata", "Quercus stellata")

dat.npn <- aggregate(dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", "First_Yes_DOY"], 
                     by=dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", c("Latitude", "Longitude", "Individual_ID", "First_Yes_Year", "Genus", "Species")], 
                     FUN=min)

dat.npn$Species <- paste(dat.npn$Genus, dat.npn$Species, sep= " ")
colnames(dat.npn) <- c("Latitude", "Longitude", "PlantNumber", "Year","Genus", "Species", "Yday")
dat.npn$PlantNumber <- as.factor(dat.npn$PlantNumber)


for(YR in dat.npn$Year){
  start <- paste(as.character(dat.npn$Year), "-01-01", sep="")
  dat.npn$Date <- as.Date((dat.npn$Yday-1), origin = start)
}


#Setting the points to download the daymet data from
path.doc <- "C:/Users/lucie/Documents/NPN_data/"
species <- "Chosen_Oaks"
ystart <- 2018

#make sure the yend of the data matches what you enter. Sometimes daymet truncates and this varibale will become wrong later in the script
yend <- 2019

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
#This is not its own script but a saved loop to be reinserted into Frequentist_GDD5-burst.R later on
#----------------------------------------------------#

#Start of loop to pull out the GDD5.cum of the bud burst date fore every tree and location
#Progress bar
pb <- txtProgressBar(min=0, max=length(lat.list)*((yend-ystart)+1), style=3)
pb.ind=0

#Creating a dataframe to hold weather summary statistics
df.loc <- data.frame(latitude=rep(lat.list[[1]]$latitude, ((yend-ystart)+1)) ,
                     longitude=rep(lat.list[[1]]$longitude, ((yend-ystart)+1)))

#Making sure we only go through relevant years we are calculating gdd5 for
dat.npn <- dat.npn[dat.npn$Year >= ystart, ]

#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 1
i <- 1
YR <- ystart
for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  df.tmp$GDD5.cum <- NA
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
    
    df.yr <- df.tmp[df.tmp$year==YR,]
    start <- paste(as.character(df.tmp$year), "-01-01", sep="")
    df.tmp$Date <- as.Date((df.yr$yday-1), origin = start)
    
    gdd.cum=0
    d.miss = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[i]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      
      df.yr[j,"GDD5.cum"] <- gdd.cum
    }
    df.tmp[df.tmp$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
    
    loc.sum <- df.yr %>% summarise(TMAX = max(tmax..deg.c.),
                                   TMIN = min(tmin..deg.c.),
                                   TMEAN = mean(TMEAN),
                                   PRCP = mean(prcp..mm.day.),
                                   SNOW = mean(srad..W.m.2.))
    
    
    df.loc[count, "latitude"] <- lat.list[[i]]$latitude
    df.loc[count, "longitude"] <- lat.list[[i]]$longitude
    df.loc[count, "year"] <- YR
    df.loc[count, "TMAX"] <- loc.sum$TMAX
    df.loc[count, "TMIN"] <- loc.sum$TMIN
    df.loc[count, "TMEAN"] <- loc.sum$TMEAN
    df.loc[count, "PRCP"] <- loc.sum$PRCP
    df.loc[count, "SNOW"] <- loc.sum$SNOW
    
    count <- count + 1
    
  }
  dat.npn$GDD5.cum <- NA
  for(DAT in paste(dat.npn$Date)){
    if(length(df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"])==0) next
    dat.npn[dat.npn$Date==as.Date(DAT),"GDD5.cum"] <- df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"]
  }
  
}

dat.comb <- dat.npn

dat.comb$Location <- paste(dat.comb$Latitude, dat.comb$Longitude, sep= " ")

dat.comb[dat.comb$Yday>=240, c("Yday", "GDD5.cum", "GDD0.cum")] <- NA
summary(dat.comb)

setwd("../")
# Save dat.comb 
write.csv(dat.comb, "../data_processed/Phenology_NPN_combined.csv", row.names=F)
