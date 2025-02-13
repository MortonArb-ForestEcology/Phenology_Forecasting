library(ncdf4) 
library(raster) 
library(ggplot2)
library(dplyr)
library(lubridate)

#Working with data extractions

#Gridmet Data (2015-2020)
#https://www.climatologylab.org/wget-gridmet.html

# Function to read specific lon/lat
read_nc_point <- function(file, var_name, target_lon, target_lat) {
  nc <- nc_open(file)
  
  # Get lon/lat values from the file
  lon_vals <- ncvar_get(nc, "lon")
  lat_vals <- ncvar_get(nc, "lat")
  
  # Find the closest indices to your target coordinates
  lon_index <- which.min(abs(lon_vals - target_lon))
  lat_index <- which.min(abs(lat_vals - target_lat))
  
  # Read only the selected lon/lat across all time steps
  data_subset <- ncvar_get(nc, var_name, start = c(lon_index, lat_index, 1), count = c(1, 1, -1))

  scale_factor <- ncatt_get(nc, var_name, "scale_factor")$value
  add_offset <- ncatt_get(nc, var_name, "add_offset")$value
  
  # Close file
  nc_close(nc)
  
  adjusted_data <- data_subset * scale_factor + add_offset
  
  return(adjusted_data)
}

# Read time variable
read_nc_time <- function(file) {
  nc <- nc_open(file)
  time_vals <- ncvar_get(nc, "day")
  nc_close(nc)
  return(time_vals)
}

folder_path <- "/Users/jocelyngarcia/Documents/GitHub/Phenology_Forecasting/Getting Gridmet:Daymet Datasets/Gridmet Data"
  
# List of NetCDF files for different years 
tmmx_files <- list.files(path = folder_path, pattern = "tmmx_.*\\.nc$", full.names = TRUE) 
tmmn_files <- list.files(path = folder_path, pattern = "tmmn_.*\\.nc$", full.names = TRUE) 

# Extract temperature data for the specific lon/lat from each file
tmmx_data_list <- lapply(tmmx_files, read_nc_point, var_name = "air_temperature", target_lon = -88.072749, target_lat = 41.812739)
tmmn_data_list <- lapply(tmmn_files, read_nc_point, var_name = "air_temperature", target_lon = -88.072749, target_lat = 41.812739)

# Combine into a two data frame one for max temps and one for min temps
tmmx_all_years <- do.call(rbind, lapply(seq_along(tmmx_files), function(i) {
  date_values <- as.Date("1900-01-01") + read_nc_time(tmmx_files[i])
  data.frame(Day = yday(date_values),  # Extract day of the year
             Max_Temperature = tmmx_data_list[[i]], 
             Year = as.integer(substr(tmmx_files[i], regexpr("20[0-9]{2}", tmmx_files[i]), regexpr("20[0-9]{2}", tmmx_files[i]) + 3)))
}))

tmmn_all_years <- do.call(rbind, lapply(seq_along(tmmn_files), function(i) {
  date_values <- as.Date("1900-01-01") + read_nc_time(tmmn_files[i])
  data.frame(Day = yday(date_values),  # Extract day of the year
             Min_Temperature = tmmn_data_list[[i]], 
             Year = as.integer(substr(tmmn_files[i], regexpr("20[0-9]{2}", tmmn_files[i]), regexpr("20[0-9]{2}", tmmn_files[i]) + 3)))
}))


# Checking everything worked
head(tmmx_all_years)
head(tmmn_all_years)


#Combine everything into 1 df 
full_gridmet_data <- merge(tmmx_all_years, tmmn_all_years , by.tmmx_all_years = "Date", by.tmmn_all_years = "Date")

#Adding temperature average column
full_gridmet_data$Temp_avg <- (full_gridmet_data$Max_Temperature + full_gridmet_data$Min_Temperature)/2

# Sorting data by Year first, then by Day
full_gridmet_data <- full_gridmet_data %>% arrange(as.integer(Year), as.integer(Day))

full_gridmet_data <- full_gridmet_data %>% filter(full_gridmet_data$Year > 2016)

head(full_gridmet_data)

write.csv(full_gridmet_data, 
          file.path("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/My Drive", 
                    "full_gridmet_data.csv"), 
          row.names = FALSE)

#######################################################################################################################################

#Daymet Data (1/1/2013 - 12/31/2023)
#https://daymet.ornl.gov/single-pixel/ used this tool to get a wget link, then pasted the url in terminal (after changing directory to desktop)
#Downloads csv file to desktop

full_daymet_data <- read.csv("daymet_data.csv")

#Changed to appropiate titles
names(full_daymet_data)[1]<-paste("Year")
names(full_daymet_data)[2]<-paste("Day")
names(full_daymet_data)[3]<-paste("Max_Temperature")
names(full_daymet_data)[4]<-paste("Min_Temperature")

#Adding Average Temp column
full_daymet_data$Temp_avg <- (full_daymet_data$Max_Temperature + full_daymet_data$Min_Temperature)/2

#Only want data 2017 and beyond
full_daymet_data <- full_daymet_data %>% filter(full_daymet_data$Year > 2016)

# Reorganize the columns to switch Year and yday

full_daymet_data <- full_daymet_data[c("Day", "Year", "Max_Temperature", "Min_Temperature", "Temp_avg")]


head(full_daymet_data)

write.csv(full_daymet_data,
          file.path("/Users/jocelyngarcia/Library/CloudStorage/GoogleDrive-jgarcia@mortonarb.org/My Drive", "full_daymet_data.csv"),
          row.names = FALSE)
