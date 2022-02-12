#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script serves to run throught our daily workflow
# Inputs: 
# Outputs: Downloads the new weather, updates the forecasts, and launches the app online
# Notes: This just sources the other scripts in this directory
#-----------------------------------------------------------------------------------------------------------------------------------#
# library(shiny)
# library(ggplot2)
# library(plotly)
# library(stringr)
# library(shinyWidgets)
# library(dplyr)
# library(gridExtra)

#Downloading the new weather data
source("LIVE-1_M1_Meterology_download.R")

#Uploading the new data to the shiny app's folder
tictoc::tic()
source("LIVE-2_data_org.R")
tictoc::toc()

#Launching the app to the internet
source("LIVE-3_run_shiny.R")
