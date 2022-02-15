#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script serves to launch the app locally or online
# Inputs: 
# Outputs: Runs the app
# Notes: You can run the app locally or online. Which working directory you need to be in depends on which you are doing
# You must be in the "shiny_app" folder to run online. You must be in this scripts wd to run it locally
#-----------------------------------------------------------------------------------------------------------------------------------#
library(shiny)
library(shinyWidgets)


#This is to run the app LOCALLY
#If you open this script and are in it's directory, all you have to do is runApp.
#setwd("../")  #This is kept here for when I am bouncing between online and local runs
#runApp("MortonArb_PhenoForecast")


#This section is to run the app ONLINE
#Must change the directory to the app itself to run it online
setwd("MortonArb_PhenoForecast/")

rsconnect::deployApp(forceUpdate = T, launch.browser = F)

setwd("../")

print("Phenology Forecast Updated!")

#This is how you manually stop the ONLINE app
#stopApp(returnValue = invisible())
