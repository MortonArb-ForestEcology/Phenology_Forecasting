#This script is used to create the csv's used for indexing the names of species so you can use either scientific or common names
#This may go away after sql updating and further progress

oaks <- googlesheets4::read_sheet("14rJUVhJ2pDSskyEzMM7KX5p3LTvpxiSnDoOw2Ngu2PY", sheet="QuercusCollection")
oaks <- data.frame(oaks)

#SQL SHOULD REMOVE THIS
dat.b <- read.csv("../../data_processed/Oak_collection_budburst.csv")

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
