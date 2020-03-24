# Combining our Weather Station Data with Phenology reports
# All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
# The majority of the rest is currently a modification of that same script by Christy
library(ggplot2)
library(dplyr)

# -----------------------------
# Read in Phenology Monitoring data
# -----------------------------

quercus.18 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2018)
quercus.18$Collection <- as.factor("Quercus")
quercus.18$Year <- lubridate::year(quercus.18$Date.Observed)

quercus.19 <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection="Quercus", dat.yr=2019)
quercus.19$Collection <- as.factor("Quercus")
quercus.19$Year <- lubridate::year(quercus.19$Date.Observed)


#Year 2018 has different column names not converted by the clean.google function so this sets themm back to equal
colnames(quercus.18) <- as.character(colnames(quercus.19))

dat.pheno <- rbind(quercus.18, quercus.19)

#Enter chosen species here. Genus must be capitalized, one space between genus and species, and species is lower case

chosen <- "Quercus macrocarpa"

dat.oak <- dat.pheno[(dat.pheno$Species == chosen),]

dat.oak$Date.Observed <- as.Date(dat.oak$Date.Observed)
dat.oak$Bud <- as.factor(dat.oak$leaf.buds.observed)
dat.oak <- dat.oak[!is.na(dat.oak$Date.Observed),]

dat.oak <- subset(dat.oak, select = c("Date.Observed", "Year", "Species", "Bud", "PlantNumber"))


#Creating final frame of the first burst for each year.
dat.burst <- aggregate(dat.oak[dat.oak$Bud=="Yes", "Date.Observed"], 
                       by=dat.oak[dat.oak$Bud=="Yes", c("Species", "PlantNumber", "Year")], 
                       FUN=min)
names(dat.burst)[which(names(dat.burst)=="x")] <- "Date"
dat.burst$Yday <- lubridate::yday((dat.burst$Date))
dat.burst$Latitude <- 41.8164
dat.burst$Longitude <- -88.0549

#Retrieving npn data
path.doc <- "C:/Users/lucie/Documents/"

dat.npn <- read.csv("C:/Users/lucie/Documents/NPN_macrocarpa (2).csv")

dat.npn <- aggregate(dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", "First_Yes_DOY"], 
                     by=dat.npn[dat.npn$Phenophase_Description=="Breaking leaf buds", c("Latitude", "Longitude", "Individual_ID", "First_Yes_Year")], 
                     FUN=min)

colnames(dat.npn) <- c("Latitude", "Longitude", "PlantNumber", "Year", "Yday")
dat.npn$PlantNumber <- as.factor(dat.npn$PlantNumber)
dat.npn$Species <- chosen

for(YR in dat.npn$Year){
  start <- paste(as.character(dat.npn$Year), "-01-01", sep="")
  dat.npn$Date <- as.Date((dat.npn$Yday-1), origin = start)
}

dat.burst <- dat.burst[,c("Latitude", "Longitude", "PlantNumber", "Year", "Yday", "Species", "Date")]

dat.comb <- rbind(dat.burst, dat.npn)


#Setting the points to download the daymet data from
species <- "Q_macrocarpa"
ystart <- 2017

#make sure the yend of the data matches what you enter. Sometimes daymet truncates and this varibale will become wrong later in the script
yend <- 2019

pointsfile <- paste(species, "_npn_points.csv", sep="")

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- dat.comb[,(c=1:2)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]
q.lat <- unique(q.lat)

#Writing the csv file of lat and longs because batch function needs to read a file instead of a dataframe
write.csv(q.lat, file.path(path.doc, file = pointsfile), row.names=FALSE)


#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = pointsfile,
                                           start = ystart,
                                           end = yend,
                                           internal = T)

#removing failed downloads 
lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]


#Start of loop to pull out the GDD5.cum of the bud burst date fore every tree and location
#Progress bar
pb <- txtProgressBar(min=0, max=length(lat.list)*((yend-ystart)+1), style=3)
pb.ind=0

#Creating a dataframe to hold weather summary statistics
df.loc <- data.frame(latitude=rep(lat.list[[i]]$latitude, ((yend-ystart)+1)) ,
                        longitude=rep(lat.list[[i]]$longitude, ((yend-ystart)+1)))


#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 0
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
  dat.comb$GDD5.cum <- NA
  for(DAT in paste(dat.comb$Date)){
    if(length(df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"])==0) next
    dat.comb[dat.comb$Date==as.Date(DAT),"GDD5.cum"] <- df.tmp[df.tmp$Date==as.Date(DAT), "GDD5.cum"]
  }

}

mat.yr <- array(dim=c(nrow(df.loc), 1000))
dimnames(mat.yr)[[1]] <- df.loc$YEAR


dat.comb$Location <- paste(dat.comb$Latitude, dat.comb$Longitude, sep= " ")

#Removing some outliers for now so sd doesn't go negative. REMEMBER TO COME BACK AND CHANGE THIS
dat.comb[dat.comb$Yday>=250, c("Yday", "GDD5.cum")] <- NA

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


library(nlme); library(lme4)

dat.gdd5.mean <- mean(dat.comb[,"GDD5.cum"], na.rm=T); 
dat.gdd5.sd <- sd(dat.comb[,"GDD5.cum"], na.rm=T)

mac.cue <- lme(GDD5.cum ~ 1, random=list(Location=~1, PlantNumber=~1), data=dat.comb, na.action=na.omit)
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

dat.gdd5.vec <- rnorm(1000, dat.gdd5.mean, dat.gdd5.sd)

dat.yr$bud.oak <- NA

calc.bud <- function(x){min(df.yr[which(df.yr$GDD5.cum >= x),"yday"])}
k <- 0

for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    df.yr <- df.tmp[df.tmp$year==YR,]
    gdd.cum=0
    d.miss = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[j]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      
      df.yr[j,"GDD5.cum"] <- gdd.cum
    }
    # summary(dat.tmp)
    if(nrow(df.yr)==0) next
    
    # Bloom time -- simple
    bud.pred <- calc.bud(dat.gdd5.mean)
    if(bud.pred != Inf) df.loc[k,"bud.oak"] <- bud.pred
    bud.vec <- unlist(lapply(dat.gdd5.vec, calc.bud))
    bud.vec[bud.vec==Inf] <- NA
    
    # par(mfrow=c(2,1))
    # hist(ceca.gdd5.vec); hist(bloom.vec)
    # par(mfrow=c(1,1))
    
    mat.yr[k,] <- bud.vec
    k <- k +1
  }
  
  df.loc$bud.mean <- apply(mat.yr, 1, mean, na.rm=T)
  df.loc$bud.sd   <- apply(mat.yr, 1, sd, na.rm=T)
  df.loc$bud.lb   <- apply(mat.yr, 1, quantile, 0.025, na.rm=T)
  df.loc$bud.ub   <- apply(mat.yr, 1, quantile, 0.975, na.rm=T)
}


lm.ceca.bloom <- lm(bud.oak ~ year, data=df.loc)
summary(lm.ceca.bloom)

yr.df <- stack(data.frame(mat.yr))
yr.df$year <- as.numeric(row.names(mat.yr))
bloom.trend2 <- lm(values ~ year, data=yr.df)
summary(bloom.trend2)

dat.comb <- na.omit(dat.comb)

oak.bud <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                        by=dat.comb[,c("Location", "Year")],
                        FUN=mean, na.rm=F)

oak.bud[,c("Yday.sd", "GDD5.cum.sd")]  <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                                                       by=dat.comb[,c("Location", "Year")],
                                                       FUN=sd, na.rm=F)[,c("Yday", "GDD5.cum")]


ggplot(data=df.loc[,]) +
  facet_wrap(~Location)+
  geom_ribbon(data=df.loc[,], aes(x=year, ymin=bud.lb, ymax=bud.ub, fill="Modeled"), alpha=0.5) +
  geom_point(data=df.loc[,], aes(x=year, y=bud.mean, color="Modeled"), alpha=0.8) +
  geom_line(data=df.loc[,], aes(x=year, y=bud.mean, color="Modeled"), alpha=0.8) + 
  geom_point(data=oak.bud, aes(x=Year, y=Yday, color="Observed"), size=1)



