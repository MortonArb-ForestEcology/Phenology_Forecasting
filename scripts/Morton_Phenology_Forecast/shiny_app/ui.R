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

Tconnect <- DBI::dbConnect(RSQLite::SQLite(), "Arb_Pheno.db")

cat <- tbl(Tconnect, "Species_Catalogue")
sp.catalogue <- cat %>%
  collect() 


sp.list <- sort(unique(sp.catalogue$Scientific))
name.type <- c("Scientific", "Common")

fluidPage(
  
  #Allowing the choice between scientific and common
  selectInput("Convention", "Choose a naming style:", list(Convention=as.list(name.type))),
  uiOutput("select_Species"),
  
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), uiOutput("plot.thresh.ui", click="thresh_click"), 
                  uiOutput("plot.temp.ui", click="temp_click"), 
                  uiOutput("plot.prcp.ui", click="prcp_click"), 
                  uiOutput("plot.dist.ui", click="dist_click"))
        )),
  verbatimTextOutput("info.thresh"),
  verbatimTextOutput("info.temp"),
  verbatimTextOutput("info.prcp"),
  verbatimTextOutput("info.dist")

)
