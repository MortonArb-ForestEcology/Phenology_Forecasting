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
# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.in <- "data_raw/meteorology"


dat.ghcn <- read.csv(file.path( path.in, "Historical_Weather.csv"))
sp.index <- read.csv(file.path( path.in, "Species_Index.csv"))
sp.catalogue<- read.csv(file.path( path.in, "Species_Catalogue.csv"))
dat.b <- read.csv(file.path( path.in, "Oak_collection_budburst.csv"))
fc.df <- read.csv(file.path(path.in, "Old_Forecast_List.csv"))

#Default number of species to start
len <- 1
my_i <- 1

#This is where we interface with the sql. This is establishing the connection and determining which table we want to pull from 
#Tconnect <- DBI::dbConnect(RSQLite::SQLite(), "Arb_Pheno.db")
#ghcn <- tbl(Tconnect, "Historical_Weather")

#This gets the datatable
#dat.ghcn <- as.data.frame(ghcn) %>%
#  collect() 

#Date gets converted when made into sql...may argue against using it too freely
#dat.ghcn$DATE <- as.Date(dat.ghcn$DATE, origin ="1970-01-01")

#forecast <- tbl(Tconnect, "Forecast_Weather")

#dat.forecast <- as.data.frame(forecast) %>%
#  collect() 

#dat.forecast$DATE <- as.Date(dat.forecast$DATE, origin ="1970-01-01")

#Here is where we read in the names of our oak species for indexing in our app
#cat <- tbl(Tconnect, "Species_Catalogue")
#sp.catalogue <- as.data.frame(cat) %>%
#  collect() 

#dex <- tbl(Tconnect, "Species_Index")
#sp.index <- as.data.frame(dex) %>%
#  collect() 

# -------------------------------------
# Doing some quick graphing
# -------------------------------------
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



#Making the connection to our budburst predicitons before we pull them out by species
#full <- tbl(Tconnect, "Budburst_Model")
full <- read.csv(file.path(path.in, "Budburst_Model.csv"))
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
    #pickerInput('Species','Choose a Species: ', choices = c(spp.avail), selected= "Quercus acutissima", options = list('live-search' = TRUE), multiple = F)
    pickerInput('Species','Choose a Species: ', choices = c(spp.avail), selected= "Quercus acutissima", options = list('live-search' = FALSE), multiple = T)
  })
  
  #observeEvent(input$Submit,{
  observe({
    req(input$Species)
    len <- length(unique(input$Species))
    for (i in 1:len) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot_temp", my_i, sep="")
        output[[plotname]] <- renderPlot({
          Species.pick <- input$Species[my_i]
          #Checking if it's the first instance
          #Species.pick <- ifelse(is.null(Species.pick), "Quercus acutissima", Species.pick)
          
          #Here is where we interact witht the sql and full what we want
          #gdd.prior <- full %>%
          #  filter(species == !!input$Species)%>% #Choosing our species
          #select(THRESH)%>% #picking the variable we want from those species
          #  collect() #A neccessary line to collect all the data instead of only the 10 first
          thresh <- full[full$species == Species.pick, "THRESH"]
          
          Year <- unique(dat.b[dat.b$Species == Species.pick, "Year"])
          prev.date <- as.data.frame(Year)
          
          for(YR in unique(dat.b[dat.b$Species == Species.pick, "Year"])){
            prev.date[prev.date$Year == YR, "Mean.Date"] <- mean.Date(as.Date(format(as.Date(dat.b[dat.b$Species == Species.pick & dat.b$Year == YR,"Date"]),"%m-%d"), format = "%m-%d"))
          }
          
          
          dat.forecast <- read.csv(file.path(path.in, fc.df[fc.df$Date == input$`Forecast date`, "File"] ))
          dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
          vars.agg <- c("TMEAN", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
          ens.forecast <- list()
          for(VAR in vars.agg){
            ens.forecast[[VAR]] <- aggregate(dat.forecast[,VAR],
                                             by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                             FUN=mean, na.rm=T)
            names(ens.forecast[[VAR]])[names(ens.forecast[[VAR]])=="x"] <- "mean"
            ens.forecast[[VAR]]$min <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=min, na.rm=T)$x
            ens.forecast[[VAR]]$max <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=max, na.rm=T)$x
          }
          
          pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
          #We want to pull from the Budburst_Model table for out GDD predictions
          ens <- unique(dat.forecast$ENS)
          for(i in 1:length(ens)){
            pred.array[,i] <- unlist(lapply(dat=dat.forecast[dat.forecast$ENS==ens[i],], FUN=calc.bud, VAR="GDD5.cum", thresh))
          }
          pred.df <- data.frame(x=as.vector(pred.array))
          
          # Create some useful indices and labels
          dat.lim <- data.frame(q50=quantile(pred.array, c(0.25, 0.75)),
                                q75=quantile(pred.array,c(0.125, 0.875)),
                                q95=quantile(pred.array,c(0.025, 0.975)))
          row.names(dat.lim) <- c("lb", "ub")
          dat.lim <- data.frame(t(dat.lim))
          pred.range = as.Date(as.numeric(dat.lim["q75",]), origin=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")))
          pred.range <- paste(lubridate::month(pred.range, label=T), lubridate::day(pred.range))
          
          
          ggplot(data=dat.ghcn) +
            stat_summary(data=dat.ghcn, aes(x=YDAY, y=TMEAN), fun=mean, color="black", geom="line", size=1,  na.rm = T) +
            geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=TMEAN, color="observed"), size=2) +
            geom_ribbon(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            geom_line(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast"), na.rm = T) +
            scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)], limits = c(0,180))  +
            scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("Avereage temperature (C) across time for ",Species.pick))+
            theme(legend.position = c(0.2, 0.75),
                  legend.title=element_blank(),
                  legend.background = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank()) +
            geom_rect(data=dat.lim["q75",],
                      aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
            geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))
        })
      })
    }
  })
  
  #observeEvent(input$Submit,{
  observe({ 
    req(input$Species)
   len <- length(unique(input$Species))
    for (i in 1:len){
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot_thresh", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          Species.pick <- input$Species[my_i]
          #Checking if it's the first instance
          #Species.pick <- ifelse(is.null(Species.pick), "Quercus acutissima", Species.pick)
          
          #for(i in length(unique(input$Species))){
          #Here is where we interact witht the sql and full what we want
          #gdd.prior <- full %>%
          #filter(species == !!input$Species)%>% #Choosing our species
          #select(THRESH)%>% #picking the variable we want from those species
          # collect() #A neccessary line to collect all the data instead of only the 10 first
          thresh <- full[full$species == Species.pick, "THRESH"]
          #Taking the mean date of each year
          Year <- unique(dat.b[dat.b$Species == Species.pick, "Year"])
          prev.date <- as.data.frame(Year)
          
          for(YR in unique(dat.b[dat.b$Species == Species.pick, "Year"])){
            prev.date[prev.date$Year == YR, "Mean.Date"] <- mean.Date(as.Date(format(as.Date(dat.b[dat.b$Species == Species.pick & dat.b$Year == YR,"Date"]),"%m-%d"), format = "%m-%d"))
          }
          
          dat.forecast <- read.csv(file.path(path.in, fc.df[fc.df$Date == input$`Forecast date`, "File"] ))
          dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
          vars.agg <- c("TMEAN", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
          ens.forecast <- list()
          for(VAR in vars.agg){
            ens.forecast[[VAR]] <- aggregate(dat.forecast[,VAR],
                                             by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                             FUN=mean, na.rm=T)
            names(ens.forecast[[VAR]])[names(ens.forecast[[VAR]])=="x"] <- "mean"
            ens.forecast[[VAR]]$min <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=min, na.rm=T)$x
            ens.forecast[[VAR]]$max <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=max, na.rm=T)$x
          }
          
          
          pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
          #We want to pull from the Budburst_Model table for out GDD predictions
          ens <- unique(dat.forecast$ENS)
          for(i in 1:length(ens)){
            pred.array[,i] <- unlist(lapply(dat=dat.forecast[dat.forecast$ENS==ens[i],], FUN=calc.bud, VAR="GDD5.cum", thresh))
          }
          pred.df <- data.frame(x=as.vector(pred.array))
          
          # Create some useful indices and labels
          dat.lim <- data.frame(q50=quantile(pred.array, c(0.25, 0.75)),
                                q75=quantile(pred.array,c(0.125, 0.875)),
                                q95=quantile(pred.array,c(0.025, 0.975)))
          row.names(dat.lim) <- c("lb", "ub")
          dat.lim <- data.frame(t(dat.lim))
          pred.range = as.Date(as.numeric(dat.lim["q75",]), origin=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")))
          pred.range <- paste(lubridate::month(pred.range, label=T), lubridate::day(pred.range))
          
          
          
          if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
            dat.ghcn$threshB <- dat.ghcn$GDD5.cum
            ens.forecast$threshB <- ens.forecast$GDD5.cum
          } else {
            dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
            ens.forecast$threshB <- ens.forecast$CDD0.cum
          }
          
          plot.threshB <- ggplot(data=dat.ghcn) +
            stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshB), fun=mean, color="black", geom="line", size=1, na.rm = T) +
            geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=threshB, color="observed"), size=2) +
            geom_ribbon(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
            geom_line(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
            scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
            scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("Cumulative Growing Degree Days across time for ",Species.pick))+
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
          
          plot.threshB + 
            geom_rect(data=dat.lim["q75",],
                      aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
            geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))
        })
      })
    }
  })
  
  #observeEvent(input$Submit,{
  observe({
    req(input$Species)
    len <- length(unique(input$Species))
    for (i in 1:len) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot_dist", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          Species.pick <- input$Species[my_i]
          Species.pick <- ifelse(is.null(Species.pick), "Quercus acutissima", Species.pick)
          
 
          #for(i in length(unique(input$Species))){
          #Here is where we interact witht the sql and full what we want
          #gdd.prior <- full %>%
          #filter(species == !!input$Species)%>% #Choosing our species
          #select(THRESH)%>% #picking the variable we want from those species
          # collect() #A neccessary line to collect all the data instead of only the 10 first
          thresh <- full[full$species == Species.pick, "THRESH"]
          #Taking the mean date of each year
          Year <- unique(dat.b[dat.b$Species == Species.pick, "Year"])
          prev.date <- as.data.frame(Year)
          
          for(YR in unique(dat.b[dat.b$Species == Species.pick, "Year"])){
            prev.date[prev.date$Year == YR, "Mean.Date"] <- mean.Date(as.Date(format(as.Date(dat.b[dat.b$Species == Species.pick & dat.b$Year == YR,"Date"]),"%m-%d"), format = "%m-%d"))
          }
          
          dat.forecast <- read.csv(file.path(path.in, fc.df[fc.df$Date == input$`Forecast date`, "File"] ))
          dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
          vars.agg <- c("TMEAN", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
          ens.forecast <- list()
          for(VAR in vars.agg){
            ens.forecast[[VAR]] <- aggregate(dat.forecast[,VAR],
                                             by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                             FUN=mean, na.rm=T)
            names(ens.forecast[[VAR]])[names(ens.forecast[[VAR]])=="x"] <- "mean"
            ens.forecast[[VAR]]$min <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=min, na.rm=T)$x
            ens.forecast[[VAR]]$max <- aggregate(dat.forecast[,VAR],
                                                 by=dat.forecast[,c("DATE", "YDAY", "TYPE")],
                                                 FUN=max, na.rm=T)$x
          }
          
          
          pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
          ens <- unique(dat.forecast$ENS)
          for(i in 1:length(ens)){
            pred.array[,i] <- unlist(lapply(dat=dat.forecast[dat.forecast$ENS==ens[i],], FUN=calc.bud, VAR="GDD5.cum", thresh))
          }
          pred.df <- data.frame(x=as.vector(pred.array))
          
          # Create some useful indices and labels
          dat.lim <- data.frame(q50=quantile(pred.array, c(0.25, 0.75)),
                                q75=quantile(pred.array,c(0.125, 0.875)),
                                q95=quantile(pred.array,c(0.025, 0.975)))
          row.names(dat.lim) <- c("lb", "ub")
          dat.lim <- data.frame(t(dat.lim))
          pred.range = as.Date(as.numeric(dat.lim["q75",]), origin=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")))
          pred.range <- paste(lubridate::month(pred.range, label=T), lubridate::day(pred.range))
          
          ggplot() + 
            geom_density(data=pred.df, aes(x=x), adjust=3.5, fill="green3", alpha=0.5) +
            geom_vline(data=dat.lim["q75",], aes(xintercept=lb), color="darkgreen", linetype="dashed") +
            geom_vline(data=dat.lim["q75",], aes(xintercept=ub), color="darkgreen", linetype="dashed") +
            scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels2$yday[seq(8, nrow(day.labels2), by=7)], labels=day.labels2$Text[seq(8, nrow(day.labels2), by=7)])  +
            scale_y_continuous(name="Probability of Bud Burst" ,expand=c(0,0)) +
            theme_bw() +
            guides(fill="none") +
            ggtitle(paste0("75% Confidence Interval for ",Species.pick))+
            theme(legend.position = c(0.5, 0.2),
                  legend.title=element_blank(),
                  legend.background = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank())
          
          # plot.title <- ggdraw() + 
          #draw_label(paste("The Morton Arboretum Bud Burst Forecast (updated:", Sys.Date(), ")\n   ", input$Species, ": ", pred.range[1], " - ", pred.range[2], " (75% CI)"),
          #           fontface = 'bold', x = 0,hjust = 0) +
          #theme(plot.margin = margin(0, 0, 0, l=10)
          # )
        })
      })
    }
  })
  #Setting up the clicking ui for each  of our plots. This makes the plots visible and makes them clickable
  output$plot.thresh.ui <- renderUI({
    len <- length(unique(input$Species))
      plot_output_list <- lapply(1:len, function(i) {
        plotname <- paste("plot_thresh", i, sep="")
        plotOutput(plotname)#, height = 480, width = 640)
      })
      do.call(tagList, plot_output_list)
  })
  
  #Setting up the clicking ui for each  of our plots. This makes the plots visible and makes them clickable
  output$plot.temp.ui <- renderUI({
    len <- length(unique(input$Species))
    plot_output_list <- lapply(1:len, function(i) {
      plotname <- paste("plot_temp", i, sep="")
      plotOutput(plotname)#, height = 480, width = 640)
    })
    do.call(tagList, plot_output_list)
  })
  

  output$plot.dist.ui <- renderUI({
    len <- length(unique(input$Species))
    plot_output_list <- lapply(1:len, function(i) {
      plotname <- paste("plot_dist", i, sep="")
      plotOutput(plotname)#, height = 480, width = 640)
    })
    do.call(tagList, plot_output_list)
  })
  
  
  #Defining the output information we get from out clicks
  
  output$info.thresh <- renderPrint({
    o.row <- nearPoints(dat.forecast[, c("TYPE", "DATE", "YDAY", "GDD5.cum")], input$thresh_click, 
                        xvar = "YDAY", yvar = "GDD5.cum",
                        threshold = 50, maxpoints = 1)
    
    #f.row <- dat.forecast[(dat.forecast$TYPE == "forecast" & dat.forecast$YDAY == o.row$YDAY), c("TYPE", "DATE", "YDAY", "GDD5.cum")]
    
    print(o.row)
    #print(f.row)
  })
  output$info.temp <- renderPrint({
    
    o.row <- nearPoints(dat.forecast[, c("TYPE", "DATE", "YDAY", "TMEAN")], input$temp_click, 
                        threshold = 50, maxpoints = 1)
    
    #f.row <- dat.forecast[(dat.forecast$TYPE == "forecast" & dat.forecast$YDAY == o.row$YDAY), c("TYPE", "DATE", "YDAY", "TMEAN")]
    
    print(o.row)
    #print(f.row)
  })

  
  output$info.dist <- renderPrint({
    f.row <- nearPoints(pred.df[, c("x")], input$dist_click, 
                        threshold = 5, maxpoints = 1)
    print(f.row)
  })
  
}
