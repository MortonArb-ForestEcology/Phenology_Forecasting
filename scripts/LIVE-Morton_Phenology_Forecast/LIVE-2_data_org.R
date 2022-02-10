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
path.weath <- "data_raw/meteorology/"
path.ghcn=c("data_raw/meteorology/GHCN_extracted/")
dir.met <- "data_raw/meteorology"

path.temp <- "MortonArb_PhenoForecast/data_raw/meteorology/"
if(!dir.exists(path.temp)) dir.create(path.temp, recursive=T, showWarnings = F)

#Reading in budburst model
bud.files <- list.files(path = "preloaded_data/model_output/", pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

convg.df <- read.csv(file.path("preloaded_data/model_output", paste0("Budburst_convergence.csv")))
good.sp <- convg.df[convg.df$burst.converge < 1.05, "species"]

Budburst_Model <- Budburst_Model[Budburst_Model$species %in% good.sp,]

set.seed(903)
#Taking a  random sample of 1000 pulls
b.model <- do.call(rbind, 
        lapply(split(Budburst_Model, Budburst_Model$species), 
               function(x) x[sample(nrow(x), 1000), ]))

rownames(b.model) <- NULL

#Reading in the oak observations
dat.b <- read.csv("preloaded_data/Oak_collection_budburst.csv")
dat.b <- dat.b[dat.b$Species %in% good.sp,]

#Reading in the historical weather
dat.ghcn <- read.csv(file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"))

dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Creating the name indexes used for the name picker (This is for having both common and scientific names)
oaks <- read.csv(file.path("preloaded_data/Quercus_Collection.csv"))

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

#Reading in budburst model
past.fc <- list.files(path = file.path(path.temp), pattern = "Previous-Forecast", full.names = F)
past.dates <- data.frame((gsub("\\..*", "", gsub(".*_", "", past.fc))), past.fc)
colnames(past.dates) <- c("Date", "File")

#Creating the different csv's
#The Forecasts have already been put there by the 1_M1 script
write.csv(b.model, file.path(path.temp, "Budburst_Model.csv"), row.names = F)
write.csv(dat.b , file.path(path.temp,"Oak_collection_budburst.csv"), row.names = F)
write.csv(dat.ghcn, file.path(path.temp, "Historical_Weather.csv"), row.names = F)
write.csv(sp.index , file.path(path.temp, "Species_Index.csv"), row.names = F)
write.csv(sp.catalogue, file.path(path.temp, "Species_Catalogue.csv"), row.names = F)
write.csv(past.dates, file.path(path.temp, "Old_Forecast_List.csv"), row.names = F)