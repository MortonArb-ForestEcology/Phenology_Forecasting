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
library(RSQLite)
library(dplyr)
path.temp <- "shiny_app/data_raw/meteorology/"
path.weath <- "data_raw/meteorology/"
path.ghcn=c("data_raw/meteorology/GHCN_extracted/")
dir.met <- "data_raw/meteorology"

#Reading in budburst model
bud.files <- list.files(path = "../../data_processed/model_output/", pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

set.seed(901)
#Taking a  random sample of 1000 pulls
b.model <- do.call(rbind, 
        lapply(split(Budburst_Model, Budburst_Model$species), 
               function(x) x[sample(nrow(x), 1000), ]))

rownames(b.model) <- NULL

#Reading in the oak observations
dat.b <- read.csv("../../data_processed/Oak_collection_budburst.csv")


#Reading in the historical weather
dat.ghcn <- read.csv(file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"))

dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Reading in the forecast weather
dat.forecast <- read.csv(file.path(paste0(path.weath,"GEFS/","MortonArb_GEFS_daily_FORECAST-READY-LONGRANGE.csv")))
dat.forecast$DATE <- as.Date(dat.forecast$DATE)

#Creating the name indexes used for the name picker (This is for having both common and scientific names)
oaks <- googlesheets4::read_sheet("14rJUVhJ2pDSskyEzMM7KX5p3LTvpxiSnDoOw2Ngu2PY", sheet="QuercusCollection")
oaks <- data.frame(oaks)

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

write.csv(b.model, file.path(path.temp, "Budburst_Model.csv"), row.names = F)
write.csv(dat.ghcn, file.path(path.temp, "Historical_Weather.csv"), row.names = F)
write.csv(dat.forecast , file.path(path.temp, "Forecast_Weather.csv"), row.names = F)
write.csv(sp.index , file.path(path.temp, "Species_Index.csv"), row.names = F)
write.csv(sp.catalogue, file.path(path.temp, "Species_Catalogue.csv"), row.names = F)



#This creates the database and "conn" becomes the variable noting the connection path
conn <- dbConnect(RSQLite::SQLite(), "shiny_app/Arb_Pheno.db")
dbWriteTable(conn, "Budburst_Model", b.model, overwrite = T)
dbWriteTable(conn, "Budburst_Obs", dat.b, overwrite = T)
dbWriteTable(conn, "Historical_Weather", dat.ghcn, overwrite = T)
dbWriteTable(conn, "Forecast_Weather", dat.forecast, overwrite = T)
dbWriteTable(conn, "Species_Index", sp.index, overwrite = T)
dbWriteTable(conn, "Species_Catalogue", sp.catalogue, overwrite = T)


#See list of tables to confirm
dbListTables(conn)
