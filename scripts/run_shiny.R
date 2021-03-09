library(shiny)
library(dplyr)
library(shiny)

#setwd("../")
#runApp("Morton_Phenology_Forecast")


setwd("Morton_Phenology_Forecast")

rsconnect::deployApp(forceUpdate = T, launch.browser = F)

stopApp(returnValue = invisible())


#Run this section if you need to update the phenology data
#may be unneccessary as we use the sql
oaks <- googlesheets4::read_sheet("14rJUVhJ2pDSskyEzMM7KX5p3LTvpxiSnDoOw2Ngu2PY", sheet="QuercusCollection")
oaks <- data.frame(oaks)

dat.b <- read.csv("../data_processed/Oak_collection_budburst.csv")

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

write.csv(sp.catalogue, "shiny_app/Species_Name_Catalogue.csv", row.names = F)
write.csv(sp.index, "shiny_app/Species_Name_Index.csv", row.names = F)


