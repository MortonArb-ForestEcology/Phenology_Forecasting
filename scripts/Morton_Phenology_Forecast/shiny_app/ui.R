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
path.in <- "data_raw/meteorology"

#Tconnect <- DBI::dbConnect(RSQLite::SQLite(), "Arb_Pheno.db")

#cat <- tbl(Tconnect, "Species_Catalogue")
#sp.catalogue <- cat %>%
#  collect() 

sp.catalogue<- read.csv(file.path( path.in, "Species_Catalogue.csv"))
sp.list <- sort(unique(sp.catalogue$Scientific))
name.type <- c("Scientific", "Common")

fc.df <- read.csv(file.path(path.in, "Old_Forecast_List.csv"))


fluidPage(
  #Allowing the choice between scientific and common
  fluidRow(
  column(width = 4, selectInput("Convention", "Choose a naming style:", list(Convention=as.list(name.type)))),
  
  column(width = 4, sliderTextInput("Forecast date", "Previous forecasts", choices=fc.df$Date, selected = as.character(Sys.Date())))),
  fluidRow(
    column(width = 4, uiOutput("select_Species"))),
           
  fluidRow(column(width = 4, "Click submit to see and update results"),
            column(width = 2, submitButton("Submit"))),
  
    fluidRow(
      splitLayout(cellWidths = c("33%", "33%", "33%"), 
                  uiOutput("plot.temp.ui"),# click="temp_click"),
                  uiOutput("plot.thresh.ui"),#, click="thresh_click"), 
                  uiOutput("plot.dist.ui")),
        ),
  #fluidRow(
    #splitLayout(cellWidths = c("25%", "25%", "25%", "25%")) 
  #verbatimTextOutput("info.thresh"),
  #verbatimTextOutput("info.temp"),
  #verbatimTextOutput("info.prcp"))
  #verbatimTextOutput("info.dist"))
  #)
)

