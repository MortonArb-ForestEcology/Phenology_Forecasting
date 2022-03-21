#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script is for the creation of the sql database that is pulled for the app
# Inputs: Thermal Time model output for Oak species
#         Quercuscollection googlesheet
#         Oak_collection_budburst.csv
#         Weather_ArbCOOP_historical_latest.csv created by M1_Meterology_download.R
#         Weather_Arb_forecast_ensemble_latest.csv created by M1_Meterology_download.R
# Outputs: Arb_Pheno.db created
# Notes: #Currently this establishes the database on a local device and then loads it with our information of interest
#-----------------------------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(gridExtra)
library(cowplot)
library(ggplot2)
path.weath <- "data_raw/meteorology/"
path.ghcn=c("data_raw/meteorology/GHCN_extracted/")
dir.met <- "data_raw/meteorology"

path.temp <- "MortonArb_BloomForecast/data/"
if(!dir.exists(path.temp)) dir.create(path.temp, recursive=T, showWarnings = F)

path.met <- "MortonArb_BloomForecast/data/meteorology"
if(!dir.exists(path.met)) dir.create(path.met, recursive=T, showWarnings = F)

path.bloom <- file.path("MortonArb_BloomForecast/data/bloom")
if(!dir.exists(path.bloom)) dir.create(path.bloom, recursive=T, showWarnings = F)

#Reading in Bloom model
path.mod <- "../../data_processed/model_output"

Bloom_Model <- read.csv(file.path(path.mod, "Cercis_canadensis_TT_model_bloom.csv"))

Bloom_Model <- Bloom_Model[,c("THRESH", "aPrec", "sd", "Species")]

convg.df <- read.csv(file.path(path.mod, paste0("Cercis_canadensis_convergence.csv")))
good.sp <- convg.df[convg.df$bloom.converge < 1.05, "species"]

Bloom_Model <- Bloom_Model[Bloom_Model$Species %in% good.sp,]

set.seed(903)
#Taking a  random sample of 1000 pulls
b.model <- do.call(rbind, 
        lapply(split(Bloom_Model, Bloom_Model$Species), 
               function(x) x[sample(nrow(x), 1000), ]))

rownames(b.model) <- NULL

#Reading in the oak observations
dat.b <- read.csv(file.path(path.mod, "../Redbud_bloom_obs.csv"))

#Reading in the historical weather
dat.ghcn <- read.csv(file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"))

dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Reading in our latest forecast
dat.forecast <- read.csv(file.path(dir.met, paste0("Mortonarb_daily_FORECAST-READY-LONGRANGE_", Sys.Date(),".csv")))
vars.agg <- c("TMEAN", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
ens.forecast <- list()

#Populating our uncertainty
for(VAR in vars.agg){
  ens.forecast[[VAR]] <- aggregate(dat.forecast[,VAR],
                                   by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                   FUN=mean, na.rm=T)
  names(ens.forecast[[VAR]])[names(ens.forecast[[VAR]])=="x"] <- "mean"
  ens.forecast[[VAR]]$min <- aggregate(dat.forecast[,VAR],
                                       by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                       FUN=min, na.rm=T)$x
  ens.forecast[[VAR]]$max <- aggregate(dat.forecast[,VAR],
                                       by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                       FUN=max, na.rm=T)$x
}

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
  dat.ghcn$threshB <- dat.ghcn$GDD5.cum
  ens.forecast$threshB <- ens.forecast$GDD5.cum
} else {
  dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
  ens.forecast$threshB <- ens.forecast$CDD0.cum
}

fc.sp <- as.data.frame(do.call(rbind, ens.forecast))
fc.sp$VAR <- gsub("\\..*","",(row.names(fc.sp)))
rownames(fc.sp) <- NULL
colnames(fc.sp) <- c("PRED.DATE", "YDAY", "TYPE", "mean", "min", "max", "VAR")
 
write.csv(fc.sp, file.path(path.temp, "meteorology", paste0("Forecast_data_", Sys.Date(),".csv")), row.names = F)

calc.bud <- function(dat, VAR, THRESH){
  min(dat[which(dat[,VAR] >= THRESH),"YDAY"])
}

#Need to optomize this later
#pred.sp <- data.frame(matrix(ncol = 2, nrow = length(unique(b.model$species))*100))
pred.sp <- data.frame()

lim.comb <- data.frame(matrix(ncol = 6, nrow = length(unique(b.model$species))))
colnames(lim.comb) <- c("Species", "lb", "ub", "mean", "min", "max")


count <- 1
for(SP in unique(b.model$Species)){
  #Pulling out the gdd5.cum vlaues
  
  thresh <- b.model[b.model$Species == SP, "THRESH"]
  
  pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
  #We want to pull from the Budburst_Model table for out GDD predictions
  ens <- unique(dat.forecast$ENS)
  for(i in 1:length(ens)){
    pred.array[,i] <- unlist(lapply(dat=dat.forecast[dat.forecast$ENS==ens[i],], FUN=calc.bud, VAR="GDD5.cum", thresh))
  }
  #pred.df <- data.frame(yday=as.vector(pred.array))
  pred.df <- data.frame(yday=pred.array)
  pred.long <- tidyr::gather(pred.df)
  colnames(pred.long) <- c("ens.ref", "yday")
  pred.long$Species <- SP
  
  for(i in 1:length(unique(pred.long$ens.ref))){
    ref <- paste0("yday.",i)
    pred.long[pred.long$ens.ref == ref, "ens.num"] <- ens[as.numeric(i)]
  }
  
  prop.df <- aggregate(Species~yday+ens.num, data=pred.long, FUN = length)
  colnames(prop.df) <- c("yday", "ens", "count")
  prop.df$ens.group <- as.numeric(gsub("\\..*","", prop.df$ens))
  prop.df$Proportion <- prop.df$count/nrow(pred.df)
  prop.df$Species <- SP
  
  pred.sp <- rbind(pred.sp, prop.df)
  
  quant <- quantile(pred.array, c(0.125, 0.875))
  # Create some useful indices and labels
  dat.lim <- data.frame(SP, quant[1], quant[2],
                        mean(pred.array), min(pred.array), max(pred.array))
  
  colnames(dat.lim) <- c("Species", "lb", "ub", "mean", "min", "max")
  row.names(dat.lim) <- NULL
  
  lim.comb[count, ] <- dat.lim
  count <- count +1
}

write.csv(pred.sp, file.path(path.bloom, paste0("Prop_Bloom_Prediction_", Sys.Date() ,".csv")), row.names = F)
write.csv(lim.comb, file.path(path.bloom, paste0("Bloom_Prediciton_Summary_", Sys.Date() ,".csv")), row.names = F)

#Checking the dates we have done a forecast for so we can pick using the slider
past.fc <- list.files(path = file.path(path.met), full.names = F)
past.dates <- data.frame((gsub("\\..*", "", gsub(".*_", "", past.fc))), past.fc)
colnames(past.dates) <- c("Date", "File")

write.csv(past.dates, file.path(path.temp, "Old_Forecast_List.csv"), row.names = F)
write.csv(dat.ghcn, file.path(path.temp, "Historical_Weather.csv"), row.names = F)
write.csv(dat.b, file.path(path.temp, "Redbud_bloom_obs.csv"), row.names = F)


print("Data Organizaiton & Prediction Workflow Complete!")
