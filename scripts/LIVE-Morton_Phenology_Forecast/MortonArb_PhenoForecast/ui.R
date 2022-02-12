#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script is the ui side of the shiny app
# Inputs: Weather_ArbCOOP_historical_latest.csv created by M1_Meterology_download.R
#         Weather_Arb_forecast_ensemble_latest.csv created by M1_Meterology_download.R
#         Species_Name_Catalogue.csv created by SP_Names.R
# Outputs: 
# Notes: The first half of this script is taken from M2_Meteorology_Graphing_Bayes.R
#-----------------------------------------------------------------------------------------------------------------------------------#
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(shinyWidgets)
# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.in <- "data/"

#Reading in the list of species names
sp.catalogue<- read.csv(file.path( path.in, "Species_Catalogue.csv"))
sp.list <- sort(unique(sp.catalogue$Scientific))
name.type <- c("Scientific", "Common")

#Reading in the list of available past forecasts: This system will only work for one year this time. I should split it by year into different folders down the line
fc.df <- read.csv(file.path(path.in, "Old_Forecast_List.csv"))

#This puts everything on a single page where I can specify the organizaiton easily
fluidPage(
  
  #This defines what will be in the first row of the page
  #Currently that is the naming convention selection and previous forecast slider
  fluidRow(
  #Allowing the choice between scientific and common: Maybe remove given the audience and functionality quirks
  column(width = 4, selectInput("Convention", "Choose a naming style:", list(Convention=as.list(name.type)))),
  
  #Allowing for a slider between the forecasts
  column(width = 4, sliderTextInput("Forecast date", "Previous forecasts", choices=fc.df$Date, selected = as.character(max(fc.df$Date))))),
  
  #Deciding what is in the second row
  fluidRow(
    #Picking the species you want to see
    column(width = 4, uiOutput("select_Species"))),
  
  #Deciding what is in the third row
  fluidRow(
    
    #The submit button and warning text
    column(width = 4, "Click submit to see and update results"),
            column(width = 2, submitButton("Submit"))),
  #Fourth row
    fluidRow(
      #The graphs themselves. The only show up when the submit button is pressed
                  uiOutput("plot.thresh.ui")),

)

