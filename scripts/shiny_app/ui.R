library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(shinyWidgets)
# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.in <- "data_raw/meteorology"

# ---------------
# Met Data
# ---------------
dat.ghcn <- read.csv(file.path(path.in, "data", "Weather_ArbCOOP_historical_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)


dat.forecast <- read.csv(file.path(path.in, "data", "Weather_Arb_forecast_ensemble_latest.csv"))
dat.forecast$DATE <- as.Date(dat.forecast$DATE)

dat.b <- read.csv("data_processed/Oak_collection_budburst.csv")
sp.list <- sort(unique(dat.b$Species))
name.type <- c("Scientific", "Common")

fluidPage(
  
  selectInput("Convention", "Choose a naming style:", list(Convention=as.list(name.type))),
  uiOutput("select_Species"),

  
  mainPanel(
    
    # Output: Histogram ----
    plotOutput(outputId = "plot1", width = "150%", height = "800px")

  )
)
