#----------------------------------------------------------------------------------------------------------------------------------#
# Function by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: A function that will download all of the google forms of interest
# Inputs: 2018 to present phenology monitoring data from the googlesheet "Phenology_Observations_GoogleForm" in the "LivingCollections-Phenology/Data_Observations/" folder
#         The clean_google_form.r script which defines the clean.google function. Found in the Github repository "Phenology_ LivingCollections"
# Outputs:A dataframe containing the information from all desired google forms.

#Enter the genus of interest as a vector and the final year of observations you want as a variable
#The function will crash if you have a genus included without end years we have a form for i.e("Ulmus" with range 2018:2019)
#This only matters for end years. Start years are adjusted to match the first year the genus has a form.

#Tends to tke about 30 seconds per form
group.google <- function(x, ystart, yend){
  dat.pheno <- data.frame()
  #Checking which genus we are working with an setting correct parameters
  for(p in x){
    
    if (p == "Quercus") {
      yrange <- c(yend:ystart)  #THIS MUST BE REVERSED AS A WORKAROUND TO AN ISSUE WITH 2018 QUERCUS. SEEMS ONLY PC ISSUE
    } else if (p == "Acer") {
      if(ystart < 2019){
        ystart <- 2019
      }
      yrange <- c(ystart:yend)
    } else if (p == "Ulmus") {
      if(ystart < 2020){
        ystart <- 2020
      }
      yrange <- c(ystart:yend)
    }
    
    collection <- p
    #Downloading the googleform from every year in the requested range
    for(yr in yrange){
      temp <- clean.google(google.key = "1eEsiJ9FdDiNj_2QwjT5-Muv-t0e-b1UGu0AFBRyITSg", collection=collection, dat.yr=yr)
      temp$Year <- yr
      temp$Collection <- as.factor(collection)
      # names(temp) <- tolower(names(temp))
      #Work around for clean.google not changing 2018 names. THIS ALSO MEANS RANGE MUST GO REVERSE FOR QUERCUS
      if(yr == "2018"){
      colnames(temp) <- as.character(colnames(dat.pheno))
      }
      dat.pheno <- rbind(dat.pheno, temp)
    }
  }
  return(dat.pheno) 
}