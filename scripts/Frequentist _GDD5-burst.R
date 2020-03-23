# Combining our Weather Station Data with Phenology reports
# All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
# The majority of the rest is currently a modification of that same script by Christy
library(ggplot2)

# -----------------------------
# Read in the meteorology data from the NOAA Global Historical Climatology Network Stations
# Note: There are two stations at/near the Arb with different time periods
# -----------------------------
path.doc <- "C:/Users/lucie/Documents/"

dat.npn <- read.csv("C:/Users/lucie/Documents/NPN_macrocarpa (2).csv")

dat.npn <- aggregate(dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", "First_Yes_DOY"], 
                     by=dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", c("Latitude", "Longitude", "Individual_ID", "First_Yes_Year")], 
                     FUN=min)

colnames(dat.npn) <- c("Latitude", "Longitude", "Individual_ID", "Year", "Yday")

for(YR in dat.npn$Year){
start <- paste(as.character(dat.npn$Year), "-01-01", sep="")
dat.npn$Date <- as.Date((dat.npn$Yday-1), origin = start)
}

species <- "Q_macrocarpa"
ystart <- 2017

#make sure the yend of the data matches what you enter. Sometimes daymet truncates and this varibale will become wrong later in the script
yend <- 2019

npnfile <- paste(species, "_npn_points.csv", sep="")

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- dat.npn[,(c=1:2)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]
q.lat <- unique(q.lat)

#Writing the csv file of lat and longs because batch function needs to read a file instead of a dataframe
write.csv(q.lat, file.path(path.doc, file = npnfile), row.names=FALSE)


#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = npnfile,
                                           start = ystart,
                                           end = yend,
                                           internal = T)

#removing failed downloads 
lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]

#Progress bar
pb <- txtProgressBar(min=0, max=length(lat.list)*((yend-ystart)+1), style=3)
pb.ind=0

#Looping to pull out the GDD%.cum of the bud burst date at every location
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
    
  }
  dat.npn$GDD5.cum <- NA
  for(DAT in paste(dat.npn$Date)){
    if(length(df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"])==0) next
    dat.npn[dat.npn$Date==as.Date(DAT),"GDD5.cum"] <- df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"]
  }

}



# Setting a shared file path for where the data are
path.met <- "G:/My Drive/Arboretum Met Data/GHCN-Daily"

# Read in the older dataset
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

# Calculate the running growing degree days for each day/year
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

#First place its differnet from Christy's
# -----------------------------
# Read in Phenology Monitoring data
# -----------------------------

quercus.18 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2018)
quercus.18$Collection <- as.factor("Quercus")
quercus.18$Year <- lubridate::year(quercus.18$Date.Observed)
summary(quercus.18)

quercus.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2019)
quercus.19$Collection <- as.factor("Quercus")
quercus.19$Year <- lubridate::year(quercus.19$Date.Observed)
summary(quercus.19)

#Unneeded since only one species right now
#acer.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Acer", dat.yr=2019)
#acer.19$Collection <- as.factor("Acer")
#acer.19$Year <- lubridate::year(acer.19$Date.Observed)
#summary(acer.19)

#Year 2018 has different column names not converted by the clean.google function so this sets themm back to equal
colnames(quercus.18) <- as.character(colnames(quercus.19))

dat.pheno <- rbind(quercus.18, quercus.19)
summary(dat.pheno)

#----------------------------------#
#What i used while the clean.google function was down
#pheno.df <- googlesheets4::sheets_find("Phenology_Observations_GoogleForm")
#dat.pheno <- data.frame(googlesheets4::sheets_read(pheno.df))

#cols.id <- grep("accession", names(dat.pheno))
#dat.pheno$PlantNumber <- as.factor(apply(dat.pheno[,cols.id], 1, FUN=function(x) {x[which(!is.na(x))][1]}))
#----------------------------------#

#Enter chosen species here. Genus must be capitalized, one space between genus and species, and species is lower case

chosen <- "Quercus macrocarpa"

dat.oak <- dat.pheno[(dat.pheno$Species == chosen),]

dat.oak$Date.Observed <- as.Date(dat.oak$Date.Observed)
dat.oak$Species <- as.factor(dat.oak$Species)
dat.oak$Bud <- as.factor(dat.oak$leaf.buds.observed)
dat.oak <- dat.oak[!is.na(dat.oak$Date.Observed),]
summary(dat.oak)

dat.bud <- subset(dat.oak, select = c("Date.Observed", "Year", "Species", "Bud", "PlantNumber"))

length(unique(dat.bud$Species))

#Creating final frame of the first burst for each year.
dat.burst <- aggregate(dat.bud[dat.bud$Bud=="Yes", "Date.Observed"], 
                       by=dat.bud[dat.bud$Bud=="Yes", c("Species", "PlantNumber", "Year")], 
                       FUN=min)
names(dat.burst)[which(names(dat.burst)=="x")] <- "Date.First.Bud.Burst"
dat.burst$yday <- lubridate::yday((dat.burst$Date.First.Bud.Burst))

# Merge in cumulative growing degree-days from the met data
dat.burst$GDD5.cum <- NA
for(DAT in unique(paste(dat.burst$Date.First.Bud.Burst))){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.burst[dat.burst$Date.First.Bud.Burst==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

summary(dat.burst)

# Testing whether GDD5 is a good predictor of day 
dat.gdd5.lm <- lm(yday ~ GDD5.cum, data=dat.burst)
summary(dat.gdd5.lm)
plot(yday ~ GDD5.cum, data=dat.burst)
abline(dat.gdd5.lm, col="red")


#Box plot of dat.burst to see the distribution. Macrocarpa has an outlier but seems plausible 
ggplot(data=dat.burst) +
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


library(nlme); library(lme4)


dat.gdd5.mean <- mean(dat.burst[,"GDD5.cum"], na.rm=T); 
dat.gdd5.sd <- sd(dat.burst[,"GDD5.cum"], na.rm=T)


mac.cue <- lme(GDD5.cum ~ 1, random=list(Year=~1, PlantNumber=~1), data=dat.burst, na.action=na.omit)
mac.summ <- summary(mac.cue)
MuMIn::r.squaredGLMM(mac.cue)
mod.cue.est <- mac.summ$tTable[,"Value"] # Hierarchical mean
mod.cue.se <- mac.summ$tTable[,"Std.Error"]
mod.cue.sd <- mac.summ$tTable[,"Std.Error"]*sqrt(mac.summ$tTable[,"DF"]+1)

# Compare to non-hierarchical
mod.cue.est; mod.cue.sd
dat.gdd5.mean; dat.gdd5.sd 


# prior <- runif(1000, min=0, max=1000)
# GDD5 ~ dnorm(mu, sigma)

lme(GDD5.cum ~ Date.First.Bud.Burst, data=dat.burst, random=list(Year=~1,  PlantNumber=~1))









data <- list(x = dat.burst$yday, y = dat.burst$GDD5.cum, n = length(dat.burst$GDD5.cum))

model {
  ## model goes here ##
}
    
plot(dat.burst$yday, dat.burst$GDD5.cum)


