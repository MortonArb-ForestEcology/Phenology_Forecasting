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
#Reading in the list of available past forecasts: This system will only work for one year this time. I should split it by year into different folders down the line
fc.df <- read.csv(file.path(path.in, "Old_Forecast_List.csv"))

#This puts everything on a single page where I can specify the organizaiton easily
fluidPage(
  
  titlePanel("Morton Arboretum Eastern Redbud Bloom Forecast"),
  
  #This defines what will be in the first row of the page
  #Currently that is our explanation of what the app is
  fluidRow(
    p(h4("This app provides a visualization of the predicted dates of bloom for Eastern redbud", em("Cercis canadensis"), "at The Morton Arboretum.
         The pink section represents our predicted range of bloom timing. The vertical line represents previous years average date of bloom"))
  ),
  fluidRow(br()), #This line is exclusively to give white space between the description and the options
  
  #The second row is the naming convention selection and previous forecast slider
  fluidRow(
    #Allowing for a slider between the forecasts
    column(width = 4, sliderTextInput("Forecast date", "Previous forecasts", choices=fc.df$Date, selected = as.character(max(fc.df$Date))))),
  
  #Deciding what is in the fourth row
  fluidRow(
    
    #The submit button and warning text
    column(width = 4, "Click submit to see and update results"),
    column(width = 2, submitButton("Submit"))),
  #Fifth row
  fluidRow(
    #The graphs themselves. The only show up when the submit button is pressed
    plotOutput("redbud")),
  
)

