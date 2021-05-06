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

#This is where we interface with the sql. This is establishing the connection and determining which table we want to pull from 
Tconnect <- DBI::dbConnect(RSQLite::SQLite(), "Arb_Pheno.db")
ghcn <- tbl(Tconnect, "Historical_Weather")

#This gets the datatable
dat.ghcn <- as.data.frame(ghcn) %>%
  collect() 

#Date gets converted when made into sql...may argue against using it too freely
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE, origin ="1970-01-01")

forecast <- tbl(Tconnect, "Forecast_Weather")

dat.forecast <- as.data.frame(forecast) %>%
  collect() 

dat.forecast$DATE <- as.Date(dat.forecast$DATE, origin ="1970-01-01")

#path.weath <- "data_raw/meteorology/"
#Reading in the historical weather
#dat.ghcn <- read.csv(file.path(paste0(path.weath, "Weather_ArbCOOP_historical_latest.csv")))
#dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Reading in the forecast weather
#dat.forecast <- read.csv(file.path(paste0(path.weath, "MortonArb_GEFS_daily_FORECAST-READY-LONGRANGE.csv")))
#dat.forecast$DATE <- as.Date(dat.forecast$DATE)

#Here is where we read in the names of our oak species for indexing in our app
cat <- tbl(Tconnect, "Species_Catalogue")
sp.catalogue <- as.data.frame(cat) %>%
  collect() 

dex <- tbl(Tconnect, "Species_Index")
sp.index <- as.data.frame(dex) %>%
  collect() 

# Subset to just the forecasts
# dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
vars.agg <- c("TMEAN", "PRCP.cum", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
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
  # summary(ens.forecast[[VAR]])
}

# ---------------

# -------------------------------------


# -------------------------------------
# Doing some quick graphing
# -------------------------------------
day.labels <- data.frame(Date=seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
  dat.ghcn$threshB <- dat.ghcn$GDD5.cum
  ens.forecast$threshB <- ens.forecast$GDD5.cum
} else {
  dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
  ens.forecast$threshB <- ens.forecast$CDD0.cum
}

plot.threshB <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshB), fun=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=threshB, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=threshB, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$threshB[ens.forecast$threshB$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
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

plot.prcp <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum), fun=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=PRCP.cum, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$PRCP.cum[ens.forecast$PRCP.cum$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$PRCP.cum[ens.forecast$PRCP.cum$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)], limits = c(0,180))  +
  scale_y_continuous(name="Cum. Precip (mm)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot.tmean <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=TMEAN), fun=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=TMEAN, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=TMEAN, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)], limits = c(0,180))  +
  scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.5, 0.2),
        legend.title=element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

calc.bud <- function(dat, VAR, THRESH){
  min(dat[which(dat[,VAR] >= THRESH),"YDAY"])
}


day.labels2 <- data.frame(Date=seq.Date(as.Date("2021-01-03"), as.Date("2021-7-01"), by="day"))
day.labels2$yday <- lubridate::yday(day.labels2$Date)
day.labels2$Text <- paste(lubridate::month(day.labels2$Date, label=T), lubridate::day(day.labels2$Date))
summary(day.labels2)



#Making the connection to our budburst predicitons before we pull them out by species
full <- tbl(Tconnect, "Budburst_Model")

#We want to pull from the Budburst_Model table for out GDD predictions
ens <- unique(dat.forecast$ENS)
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
  pickerInput('Species','Choose a Species: ', choices = c(spp.avail), selected= sort(spp.avail), options = list('live-search' = TRUE), multiple = F)
  })

  output$plot.thresh <- renderPlot({
    #Here is where we interact witht the sql and full what we want
    gdd.prior <- full %>%
      filter(species == !!input$Species)%>% #Choosing our species
      #select(THRESH)%>% #picking the variable we want from those species
      collect() #A neccessary line to collect all the data instead of only the 10 first
    
    set.seed(902)
    #thresh <- sample(gdd.prior$THRESH, 500)
    #FAKE VALUES CURRENTLY TO TEST FUNCTIONALITY 
    
    thresh <- runif(500, 300, 600)
    pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
    
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
    
    plot.threshB + 
      geom_rect(data=dat.lim["q75",],
                aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)
    
  })
  
  output$plot.temp <- renderPlot({
    #Here is where we interact witht the sql and full what we want
    gdd.prior <- full %>%
      filter(species == !!input$Species)%>% #Choosing our species
      #select(THRESH)%>% #picking the variable we want from those species
      collect() #A neccessary line to collect all the data instead of only the 10 first
    
    set.seed(902)
    #thresh <- sample(gdd.prior$THRESH, 500)
    #FAKE VALUES CURRENTLY TO TEST FUNCTIONALITY 
    
    thresh <- runif(500, 300, 600)
    pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
    
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
    
    
    plot.tmean +
    geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)

    
  }) 
  
  output$plot.prcp <- renderPlot({
    #Here is where we interact witht the sql and full what we want
    gdd.prior <- full %>%
      filter(species == !!input$Species)%>% #Choosing our species
      #select(THRESH)%>% #picking the variable we want from those species
      collect() #A neccessary line to collect all the data instead of only the 10 first
    
    set.seed(902)
    #thresh <- sample(gdd.prior$THRESH, 500)
    #FAKE VALUES CURRENTLY TO TEST FUNCTIONALITY 
    
    thresh <- runif(500, 300, 600)
    pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
    
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
    
    
    plot.prcp +
      geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)
    
    
  }) 
  
  output$plot.dist <- renderPlot({
    #Here is where we interact witht the sql and full what we want
    gdd.prior <- full %>%
      filter(species == !!input$Species)%>% #Choosing our species
      #select(THRESH)%>% #picking the variable we want from those species
      collect() #A neccessary line to collect all the data instead of only the 10 first
    
    set.seed(902)
    #thresh <- sample(gdd.prior$THRESH, 500)
    #FAKE VALUES CURRENTLY TO TEST FUNCTIONALITY 
    
    thresh <- runif(500, 300, 600)
    pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ENS))))
    
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
      guides(fill=F) +
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
  
  
  #Setting up the clicking ui for each  of our plots. This makes the plots visible and makes them clickable
  output$plot.thresh.ui <- renderUI({
    plotOutput("plot.thresh", click="thresh_click")
  })
  
  output$plot.temp.ui <- renderUI({
    plotOutput("plot.temp", click="temp_click")
  })
  
  output$plot.prcp.ui <- renderUI({
    plotOutput("plot.prcp", click="prcp_click")
  })
  
  output$plot.dist.ui <- renderUI({
    plotOutput("plot.dist", click="dist_click")
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
  output$info.prcp <- renderPrint({
    
    o.row <- nearPoints(dat.forecast[, c("TYPE", "DATE", "YDAY", "PRCP.cum")], input$prcp_click, 
                        threshold = 50, maxpoints = 1)
    
    #f.row <- dat.forecast[(dat.forecast$TYPE == "forecast" & dat.forecast$YDAY == o.row$YDAY), c("TYPE", "DATE", "YDAY", "PRCP.cum")]
    
    print(o.row)
    #print(f.row)
  })
  
  output$info.dist <- renderPrint({
    f.row <- nearPoints(pred.df[, c("x")], input$dist_click, 
                      threshold = 5, maxpoints = 1)
    print(f.row)
  })
  
}
