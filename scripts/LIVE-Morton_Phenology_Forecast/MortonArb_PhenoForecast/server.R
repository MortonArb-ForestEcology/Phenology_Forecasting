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
dat.ghcn <- read.csv(file.path(path.in, "meteorology" , "Historical_Weather.csv"))

# -------------------------------------
# Creating our budburst visualizations
# -------------------------------------
# Creating some day and axis labels
day.labels <- data.frame(Date=seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

calc.bud <- function(dat, VAR, THRESH){
  min(dat[which(dat[,VAR] >= THRESH),"YDAY"])
}

day.labels2 <- data.frame(Date=seq.Date(as.Date("2021-01-03"), as.Date("2021-7-01"), by="day"))
day.labels2$yday <- lubridate::yday(day.labels2$Date)
day.labels2$Text <- paste(lubridate::month(day.labels2$Date, label=T), lubridate::day(day.labels2$Date))
summary(day.labels2)


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
          
          dat.dist <- read.csv(file.path(path.in, "budburst", paste0("Oak_Budburst_Prediction_", Sys.Date() ,".csv")))
          dat.budsum <- read.csv(file.path(path.in, "budburst", paste0("Oak_Prediciton_Summary_", Sys.Date() ,".csv")))
          dat.forecast <- read.csv(file.path(path.in, "meteorology", paste0("Forecast_data_", Sys.Date(),".csv")))
          
          
          #Taking the mean date of each year
          Year <- unique(dat.b[dat.b$Species == SP, "Year"])
          prev.date <- as.data.frame(Year)
          for(YR in unique(dat.b[dat.b$Species == SP, "Year"])){
            prev.date[prev.date$Year == YR, "Mean.Date"] <- mean.Date(as.Date(format(as.Date(dat.b[dat.b$Species == SP & dat.b$Year == YR,"Date"]),"%m-%d"), format = "%m-%d"))
          }

          plot.threshB <- ggplot(data=dat.ghcn) +
            stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshB), fun=mean, color="black", geom="line", size=1, na.rm = T) +
            geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=threshB, color="observed"), size=2) +
            
            #geom_ribbon(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            geom_ribbon(data=dat.forecast[dat.forecast$VAR == "threshB" & dat.forecast$TYPE == "forecast" ,], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            
            #geom_line(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
            geom_line(data=dat.forecast[dat.forecast$VAR == "threshB" & dat.forecast$TYPE == "forecast" ,], aes(x=YDAY, y=mean, color="forecast")) +
            
            
            scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("Cumulative GDD5 across time for ", SP))+
            theme(legend.position = c(0.2, 0.75),
                  legend.title=element_blank(),
                  legend.background = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
          if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))) {
            plot.threshB <- plot.threshB + 
              scale_y_continuous(name="Cum. GrowDD, base 5 C" ,expand=c(0,0)) +
              scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)])  +
              coord_cartesian(ylim=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY<=180])))
            
            # pheno.sum; dat.thresh
          } else {
            plot.threshB <- plot.threshB + 
              scale_y_continuous(name="Cum. ChillDD, base -2 C" ,expand=c(0,0)) +
              scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)])   +
              coord_cartesian(ylim=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY>=155])))
          }  
          
          Mean.d.temp <- ggplot(data=dat.ghcn) +
            stat_summary(data=dat.ghcn, aes(x=YDAY, y=TMEAN), fun=mean, color="black", geom="line", size=1,  na.rm = T) +
            geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=TMEAN, color="observed"), size=2) +
            
            #geom_ribbon(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            geom_ribbon(data=dat.forecast[dat.forecast$VAR == "TMEAN" & dat.forecast$TYPE == "forecast" ,], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            
            #geom_line(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast"), na.rm = T) +
            geom_line(data=dat.forecast[dat.forecast$VAR == "TMEAN" & dat.forecast$TYPE == "forecast" ,], aes(x=YDAY, y=mean, color="forecast"), na.rm = T) +
            
            
            
            
            scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)], limits = c(0,180))  +
            scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("Mean temp (C) across time for ",SP))+
            theme(legend.position = c(0.2, 0.75),
                  legend.title=element_blank(),
                  legend.background = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank()) +
            geom_rect(data=dat.budsum[dat.budsum$Species == SP,],
                      aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
            geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))+
            theme(text = element_text(size = 15))     
          
          Cum.gdd5 <- plot.threshB + 
            geom_rect(data=dat.budsum[dat.budsum$Species == SP,],
                      aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
            geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))+
            theme(text = element_text(size = 15))     
          
          Prob.dist <- ggplot() + 
            geom_density(data=dat.dist[dat.dist$Species == SP,], aes(x=yday), adjust=3.5, fill="green3", alpha=0.5) +
            geom_vline(data=dat.budsum[dat.budsum$Species == SP,], aes(xintercept=lb), color="darkgreen", linetype="dashed") +
            geom_vline(data=dat.budsum[dat.budsum$Species == SP,], aes(xintercept=ub), color="darkgreen", linetype="dashed") +
            scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels2$yday[seq(8, nrow(day.labels2), by=7)], labels=day.labels2$Text[seq(8, nrow(day.labels2), by=7)])  +
            scale_y_continuous(name="Probability of Bud Burst" ,expand=c(0,0)) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("75% Confidence Interval for ",SP))+
            theme(legend.position = c(0.5, 0.2),
                  legend.title=element_blank(),
                  legend.background = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank())+
            theme(text = element_text(size = 15))     
          
          
         plot_grid(Mean.d.temp, Cum.gdd5, Prob.dist, nrow = 1)
          
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