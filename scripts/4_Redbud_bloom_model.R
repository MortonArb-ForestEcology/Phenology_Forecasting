# -----------------------------
# Read in the meteorology data from the NOAA Global Historical Climatology Network Stations
# Note: There are two stations at/near the Arb with different time periods
# -----------------------------
library(ggplot2)
# Setting a shared file path for where the data are
path.goog <- "G:/.shortcut-targets-by-id/1SGuXMBpBVsqzUFeK_YsR7BD7ZqqmhJeG/ClimateChange_MortonArb/datasets"

# Read in the older dataset
met.old <- read.csv(file.path(path.goog, "MortonArb_GHCND-USC00119221_1895-2007.csv"))
met.old$DATE <- as.Date(met.old$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.old)

# Read in the newer dataset
met.new <- read.csv(file.path(path.goog, "MortonArb_GHCND-USC00115097_2007-04-01_2019-05-15.csv"))
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
met.all <- met.all[met.all$YEAR>1895 & met.all$YEAR<2019,]
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

ggplot(data=met.all) +
  geom_line(aes(x=YDAY, y=GDD5.cum, group=YEAR), size=0.5, alpha=0.5) +
  theme_bw()
# -----------------------------
# Read in Ed's Bloom Data & subset just _Cercis canadensis_ because it has the longest record & most observations
# -----------------------------
dat.bloom <- as.data.frame(readxl::read_excel(file.path(path.goog, "BRAHMS-FULL-BLOOM-EXTRACT_2019-04-15.xlsx"), sheet="Sheet1"))
dat.bloom$Date <- as.Date(dat.bloom$Date)
dat.bloom$year <- lubridate::year(dat.bloom$Date)
dat.bloom$yday <- lubridate::yday(dat.bloom$Date)
dat.bloom$PlantID <- as.character(dat.bloom$PlantID)
dat.bloom$FullName <- as.character(dat.bloom$FullName)
dat.bloom$Collection <- as.factor(dat.bloom$Collection)
dat.bloom$FlowerStage <- as.factor(dat.bloom$Collection)
dat.bloom <- dat.bloom[!is.na(dat.bloom$Date),]
summary(dat.bloom)

dim(dat.bloom)
hist(dat.bloom$year)
length(unique(dat.bloom$PlantID))
length(unique(dat.bloom$FullName))

# Merge in cumulative growing degree-days from the met data
dat.bloom$GDD5.cum <- NA
for(DAT in unique(paste(dat.bloom$Date))){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.bloom[dat.bloom$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}
summary(dat.bloom)

dat.b <- dat.bloom[dat.bloom$FullName == "Cercis canadensis L." ,]

write.csv(dat.b, file.path("../data_processed/Redbud_bloom_obs.csv"))

library(rjags)
library(coda)
library(dplyr)
library(tidyr)

dir.create("../data_processed/model_output", recursive = T, showWarnings = F)

# Read in output of previous script
dat.b$Accession <- unlist(lapply(strsplit(paste(dat.b$PlantID), "-"), function(x){x[1]}))
dat.b$Date <- as.Date(dat.b$Date)

#Creating a data frame to check the model output
Check <- data.frame()
l <- 1
#We are using bloom to determine the species becasue it is missing two species: fusiformis and laurifolia

  
#Creating the row for this species information
Check[l, "species"] <- "Cercis canadensis L."
Check[l, "bloom.nObs"] <- nrow(dat.b)
  

SP.burst <- as.data.frame(table(dat.b[, "PlantID"]))
colnames(SP.burst) <- c("Species", "Freq")
  
#Checking the mean
Check[l, "mean.bloom"] <- mean(dat.bloom$GDD5.cum, na.rm = T)
  
#Creating indexes so the hierarchy can properly function
bloom.acc <- aggregate(FullName~Accession, data=dat.b,
                         FUN=min)
  
hierarchical_regression <- "
model{
  for(k in 1:n){
      mu[k] <- acc[pln[k]]  
      y[k] ~ dnorm(mu[k], sPrec)
  }
  
  for(j in 1:nSp){
    THRESH <-  a
    a ~ dnorm(Tprior, aPrec)
    aPrec ~ dgamma(0.2, 0.1)
    Tprior ~ dunif(1, 600)
  }
    
  for(i in 1:nAcc){
      acc[i] <-  THRESH[sp[i]] + c[i]
      c[i] ~ dnorm(0, cPrec)
  }
  sPrec ~ dgamma(0.1, 0.1)
  cPrec ~ dgamma(0.1, 0.1)

}
"
  
bloom.list <- list(y = dat.b$GDD5.cum, n = length(dat.b$GDD5.cum),
                   pln = as.numeric(factor(dat.b$Accession)), nAcc = length(unique(dat.b$Accession)),
                   sp = as.numeric(factor(bloom.acc$FullName)), nSp = length(unique(dat.b$FullName)))


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
rngs <- c(737, 874, 869)
for(i in 1:nchain){
  inits[[i]] <- list(.RNG.name = "base::Super-Duper",
                     .RNG.seed = as.integer(rngs[i])
  )
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
bloom.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = bloom.list,
                             inits = inits,
                             n.chains = 3)
  
  
#Converting the ooutput into a workable format
bloom.out   <- coda.samples (model = bloom.model,
                             variable.names = c("THRESH", "aPrec"),
                             n.iter = 700000)
  
  
#Checking that convergence happened
bud.con <- gelman.diag(bloom.out)
Check[l, "bloom.converge"] <- bud.con[[1]][1]
  
#Removing burnin before convergence occurred
burnin = 690000                                ## determine convergence from GBR output
bloom.burn <- window(bloom.out,start=burnin)  ## remove burn-in
summary(bloom.burn)
  
#converting them into a dataframe 
bloom.df <- as.data.frame(as.matrix(bloom.burn))
  
#calculating sd from the precison
bloom.df$sd <- 1/sqrt(bloom.df[,"aPrec"])
summary(bloom.df)
  
  
bloom.df$Species <- unique(dat.b$FullName)
bud.ci <- apply(as.matrix(bloom.df$THRESH),2,quantile,c(0.025,0.5,0.975))
  
Check[l, "bloom 2.5%"] <- bud.ci[1]
Check[l, "bloom 50%"] <- bud.ci[2]
Check[l, "bloom 97.5%"] <- bud.ci[3]

  
write.csv(bloom.df, file.path("../data_processed/model_output", paste0("Cercis_canadensis_TT_model_bloom.csv")), row.names=F)
  

#Checking convergence and confidence interval of all species
write.csv(Check, file.path("../data_processed/model_output", paste0("Cercis_canadensis_convergence.csv")), row.names=F)
