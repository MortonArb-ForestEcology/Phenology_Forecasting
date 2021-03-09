#This script is for the creation of the sql database
#Currently this only establishes the database on a local device and then loads it with our budburst model prediction data
#This is a first pass test of concept but will soon integrate all aspects of our phenology data
library(RSQLite)

bud.files <- list.files(path = "Morton_Phenology_Forecast/data_processed/model_output/", pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

#This creates the database and "conn" becomes the variable noting the connection path
conn <- dbConnect(RSQLite::SQLite(), "Morton_Phenology_Forecast/Arb_Pheno.db")
dbWriteTable(conn, "Budburst_Model", Budburst_Model)

#See list of tables to confirm
dbListTables(conn)