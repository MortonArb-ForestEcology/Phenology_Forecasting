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

path.temp <- "shiny_app/data/"
if(!dir.exists(path.temp)) dir.create(path.temp, recursive=T, showWarnings = F)

path.met <- "shiny_app/data/meteorology"
if(!dir.exists(path.met)) dir.create(path.met, recursive=T, showWarnings = F)

path.burst <- file.path("shiny_app/data/budburst")
if(!dir.exists(path.burst)) dir.create(path.burst, recursive=T, showWarnings = F)

#Reading in budburst model
path.mod <- "../../data_processed/model_output"
bud.files <- list.files(path = path.mod, pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

convg.df <- read.csv(file.path(path.mod, paste0("Budburst_convergence.csv")))
good.sp <- convg.df[convg.df$burst.converge < 1.05, "species"]

Budburst_Model <- Budburst_Model[Budburst_Model$species %in% good.sp,]

set.seed(903)
#Taking a  random sample of 1000 pulls
b.model <- do.call(rbind, 
        lapply(split(Budburst_Model, Budburst_Model$species), 
               function(x) x[sample(nrow(x), 1000), ]))

rownames(b.model) <- NULL

#Reading in the oak observations
dat.b <- read.csv(file.path(path.mod, "../Oak_collection_budburst.csv"))
dat.b <- dat.b[dat.b$Species %in% good.sp,]

#Reading in the historical weather
dat.ghcn <- read.csv(file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"))

dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Reading in our latest forecast
dat.forecast <- read.csv(file.path(dir.met, paste0("MortonArb_daily_FORECAST-READY-LONGRANGE_", Sys.Date(),".csv")))
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
for(SP in unique(b.model$species)){
#Pulling out the gdd5.cum vlaues

  thresh <- b.model[b.model$species == SP, "THRESH"]

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

write.csv(pred.sp, file.path(path.burst, paste0("Prop_Oak_Budburst_Prediction_", Sys.Date(),".csv")), row.names = F)
write.csv(lim.comb, file.path(path.burst, paste0("Oak_Prediciton_Summary_", Sys.Date(),".csv")), row.names = F)

#Creating the name indexes used for the name picker (This is for having both common and scientific names)
## This is a file Lucien created based on something in our Google Drive and will need to be manually updated if necessary; This is is a small, static file, so now pushed to GitHub so everybody can have the same version
oaks <- read.csv(file.path("../../data_static//Quercus_Collection.csv"))

sp.catalogue <- as.data.frame(unique(dat.b$Species))
colnames(sp.catalogue) <- c("Scientific")
sp.catalogue$Common <- oaks$Common.Name[match(sp.catalogue$Scientific, oaks$Species)]

sci <- as.data.frame(unique(dat.b$Species))
colnames(sci) <- "Name"
sci$Type <- "Scientific"

com <- as.data.frame(sp.catalogue$Common)
colnames(com) <- "Name"
com$Type <- "Common"

sp.index <- rbind(sci, com)

#Checking the dates we have done a forecast for so we can pick using the slider
past.fc <- list.files(path = file.path(path.met), full.names = F)
past.dates <- data.frame((gsub("\\..*", "", gsub(".*_", "", past.fc))), past.fc)
colnames(past.dates) <- c("Date", "File")


#Creating the different csv's used for options
write.csv(sp.index , file.path(path.temp, "Species_Index.csv"), row.names = F)
write.csv(sp.catalogue, file.path(path.temp, "Species_Catalogue.csv"), row.names = F)
write.csv(past.dates, file.path(path.temp, "Old_Forecast_List.csv"), row.names = F)
write.csv(dat.ghcn, file.path(path.temp, "Historical_Weather.csv"), row.names = F)
write.csv(dat.b, file.path(path.temp, "Oak_collection_budburst.csv"), row.names = F)


print("Data Organizaiton & Prediction Workflow Complete!")
