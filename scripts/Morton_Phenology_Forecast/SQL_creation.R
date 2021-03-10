#This script is for the creation of the sql database
#Currently this only establishes the database on a local device and then loads it with our budburst model prediction data
#This is a first pass test of concept but will soon integrate all aspects of our phenology data
library(RSQLite)

#Reading in budburst model
bud.files <- list.files(path = "../../data_processed/model_output/", pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

#Reading in the oak observations
dat.b <- read.csv("../../data_processed/Oak_collection_budburst.csv")


#This creates the database and "conn" becomes the variable noting the connection path
conn <- dbConnect(RSQLite::SQLite(), "shiny_app/Arb_Pheno.db")
dbWriteTable(conn, "Budburst_Model", Budburst_Model)
dbWriteTable(conn, "Budburst_Obs", dat.b)


#See list of tables to confirm
dbListTables(conn)
