#This script is for mapping the locations that our npn data comes from so we have a general sense of where the data is coming from
#Inputs: THis script requires the script 1_Organize_NPN_Data.R to be run so you have the local file of npn data


library(rgdal)
library(raster)
library(dplyr)
library(sf)

#Reading in our organized dataframe
dat.all <- read.csv("../data_processed/Phenology_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)


#These two lines aren't really neccessary but helps me keep track of what I'm working with
species <- c("Quercus imbricaria", "Quercus falcata", "Quercus stellata")
dat.comb <- dat.all[dat.all$Species %in% species, ]


#Creating a shapefile using the longitude and latitiude in the NPN data
npn.sf <- st_as_sf(dat.comb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")

#Reading in a nice US state map
state_boundary_us <- readOGR("C:/Users/lucie/Documents/R/data/usa-boundary-layers",
                             "US-State-Boundaries-Census-2014")


#Converting that sf shapefile int a sp spatial object
npn <- as(npn.sf, "Spatial")

#Checking that the extent of the data fits within the extent of the US
extent(npn)
extent(state_boundary_us)


#Setting up different colors for different species
species_palette <- c("blue", "red", "green")
all_species_colors <- (species_palette)[npn$Species]

#Plotting out results
plot(state_boundary_us,
     main = "Locations of our NPN Oak data",
     border = "gray40")

plot(npn,
     col = all_species_colors,
     lwd = 2,
     add = TRUE)

legend("bottomleft",   # location of legend
       legend = levels(npn$Species), # categories or elements to render in
       # the legend
       fill = species_palette)

