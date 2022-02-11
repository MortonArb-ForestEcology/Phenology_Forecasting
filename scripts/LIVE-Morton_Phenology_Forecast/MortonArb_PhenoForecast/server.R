#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script is the server side of the shiny app
# Inputs: Weather_ArbCOOP_historical_latest.csv created by M1_Meterology_download.R
#         Weather_Arb_forecast_ensemble_latest.csv created by M1_Meterology_download.R
#         Species_Name_Catalogue.csv created by SP_Names.R
#         Species_Name_Index.csv created by SP_Names.R
#         Arb_Pheno.db created by SQL_creation.R
# Outputs: 
# Notes: The first half of this script is taken from M2_Meteorology_Graphing_Bayes.R
#         THERE ARE ALTERNATIVE TO DPLYR for communicating with sql. I like dplyr but it's not needed use can use RSQLite 
#-----------------------------------------------------------------------------------------------------------------------------------#
library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(gridExtra)
library(cowplot)
# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.in <- "data/"


sp.index <- read.csv(file.path(path.in, "Species_Index.csv"))
sp.catalogue<- read.csv(file.path( path.in, "Species_Catalogue.csv"))
fc.df <- read.csv(file.path(path.in, "Old_Forecast_List.csv"))

#Default number of species to start
len <- 1
my_i <- 1

function(input, output) { 
  
  #This is where we created the species options including the ability to pick between common and scientific names
  output$select_Species <- renderUI({
    if(input$Convention == "Common"){
      spp.avail <- sp.catalogue$Scientific
      #If people want to use the Common names, they are presented common names but under the hood we use scientific to match file names
      #THis is sadly also why alphabetical only works one way...
      names(spp.avail) <- sp.catalogue$Common
    } else{
      spp.avail <- sort(unique(paste(sp.index$Name[sp.index$Type==input$Convention])))
    }
    #This determines the ui output that is loaded in. 
    #This is defined here because it requires a response from the naming convention selection (it needs an input)
    pickerInput('Species','Choose a Species: ', choices = c(spp.avail), selected= "Quercus acutissima", options = list('live-search' = FALSE), multiple = T)
  })

  #Observe here means it is waiting to observe something. In this case it is the click of the submit button. It prevents the page from loading things before a selection
  observe({ 
    #Req requires that it recieves and input of a species to run. This prevents crashing from an NA input
    req(input$Species)
    #Checking how many species were selected
    len <- length(unique(input$Species))
    for (i in 1:len){
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated. Shiny doesn't like for loops because how it updates
      local({
        #Keeping a local value to cycle through
        my_i <- i
        plotname <- paste("plot_thresh", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          SP <- input$Species[my_i]
          #png::readPNG(paste0("figures/", input$`Forecast date`, "/" , input$Species, "_visualization.png"))
          load(paste0("figures/", input$`Forecast date` , "/" , SP, "_visualization.rdata"))
          plots

        })
      })
    }
  })
  
  
  #THe actual final output that is dynamic based on how may species were chosen
  output$plot.thresh.ui <- renderUI({
    len <- length(unique(input$Species))
    plot_output_list <- lapply(1:len, function(i) {
      plotname <- paste("plot_thresh", i, sep="")
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
}  