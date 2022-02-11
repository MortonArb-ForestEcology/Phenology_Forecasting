#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Phenology forecasting app
# Purpose: This script is for the creation of the sql database that is pulled for the app
# Inputs: Thermal Time model output for Oak species
#         Quercuscollection googlesheet
#         Oak_collection_budburst.csv
#         Weather_ArbCOOP_historical_latest.csv created by M1_Meterology_download.R
#         Weather_Arb_forecast_ensemble_latest.csv created by M1_Meterology_download.R
# Outputs: Arb_Pheno.db created
# Notes: #Currently this establishes the database on a local device and then loads it with our information of interest
#-----------------------------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(gridExtra)
library(cowplot)
library(ggplot2)
path.weath <- "data_raw/meteorology/"
path.ghcn=c("data_raw/meteorology/GHCN_extracted/")
dir.met <- "data_raw/meteorology"

path.temp <- "MortonArb_PhenoForecast/data/meteorology/"
if(!dir.exists(path.temp)) dir.create(path.temp, recursive=T, showWarnings = F)

path.vis <- "MortonArb_PhenoForecast/figures/"
if(!dir.exists(path.vis)) dir.create(path.vis, recursive=T, showWarnings = F)

path.date <- file.path("MortonArb_PhenoForecast/figures", Sys.Date())
if(!dir.exists(path.date)) dir.create(path.date, recursive=T, showWarnings = F)

#Reading in budburst model
path.mod <- "../../data_processed/model_output"
bud.files <- list.files(path = path.mod, pattern = "TT_model_budburst.csv", full.names = T)
Budburst_Model <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% 
                                  bind_rows(.id = "id"))

Budburst_Model <- Budburst_Model[,c("THRESH", "aPrec", "sd", "species")]

convg.df <- read.csv(file.path(path.mod, paste0("Budburst_convergence.csv")))
good.sp <- convg.df[convg.df$burst.converge < 1.05, "species"]

Budburst_Model <- Budburst_Model[Budburst_Model$species %in% good.sp,]

set.seed(903)
#Taking a  random sample of 1000 pulls
b.model <- do.call(rbind, 
        lapply(split(Budburst_Model, Budburst_Model$species), 
               function(x) x[sample(nrow(x), 1000), ]))

rownames(b.model) <- NULL

#Reading in the oak observations
dat.b <- read.csv(file.path(path.mod, "../Oak_collection_budburst.csv"))
dat.b <- dat.b[dat.b$Species %in% good.sp,]

#Reading in the historical weather
dat.ghcn <- read.csv(file.path(dir.met, "Weather_ArbCOOP_historical_latest.csv"))

dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)

#Reading in our latest forecast
dat.forecast <- read.csv(file.path(dir.met, paste0("Mortonarb_daily_FORECAST-READY-LONGRANGE_", Sys.Date(),".csv")))
dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
vars.agg <- c("TMEAN", "GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum")
ens.forecast <- list()

#Populating our uncertainty
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


for(SP in unique(b.model$species)){
#Pulling out the gdd5.cum vlaues
  thresh <- b.model[b.model$species == SP, "THRESH"]
  
  #Taking the mean date of each year
  Year <- unique(dat.b[dat.b$Species == SP, "Year"])
  prev.date <- as.data.frame(Year)
  for(YR in unique(dat.b[dat.b$Species == SP, "Year"])){
    prev.date[prev.date$Year == YR, "Mean.Date"] <- mean.Date(as.Date(format(as.Date(dat.b[dat.b$Species == SP & dat.b$Year == YR,"Date"]),"%m-%d"), format = "%m-%d"))
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
    geom_ribbon(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
    geom_line(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast"), na.rm = T) +
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
    geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
    geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))+
    theme(text = element_text(size = 15))     
  
  Cum.gdd5 <- plot.threshB + 
    geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)+
    geom_vline(data = prev.date, aes(xintercept=lubridate::yday(Mean.Date), linetype = as.character(Year)))+
    theme(text = element_text(size = 15))     
  
  Prob.dist <- ggplot() + 
    geom_density(data=pred.df, aes(x=x), adjust=3.5, fill="green3", alpha=0.5) +
    geom_vline(data=dat.lim["q75",], aes(xintercept=lb), color="darkgreen", linetype="dashed") +
    geom_vline(data=dat.lim["q75",], aes(xintercept=ub), color="darkgreen", linetype="dashed") +
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
  

  plots <- plot_grid(Mean.d.temp, Cum.gdd5, Prob.dist, nrow = 1)
  save(plots, file = paste0(path.date, "/", SP, "_visualization.rdata"))

}


#Creating the name indexes used for the name picker (This is for having both common and scientific names)
## This is a file Lucien created based on something in our Google Drive and will need to be manually updated if necessary; This is is a small, static file, so now pushed to GitHub so everybody can have the same version
oaks <- read.csv(file.path("../../data_static//Quercus_Collection.csv"))

sp.catalogue <- as.data.frame(unique(dat.b$Species))
colnames(sp.catalogue) <- c("Scientific")
sp.catalogue$Common <- oaks$Common.Name[match(sp.catalogue$Scientific, oaks$Species)]

sci <- as.data.frame(unique(dat.b$Species))
colnames(sci) <- "Name"
sci$Type <- "Scientific"

com <- as.data.frame(sp.catalogue$Common)
colnames(com) <- "Name"
com$Type <- "Common"

sp.index <- rbind(sci, com)

#Checking the dates we have done a forecast for so we can pick using the slider
past.fc <- list.files(path = file.path(path.vis), full.names = F)
past.dates <- data.frame(past.fc)
colnames(past.dates) <- c("Date")

#Creating the different csv's used for options
write.csv(sp.index , file.path(path.temp, "Species_Index.csv"), row.names = F)
write.csv(sp.catalogue, file.path(path.temp, "Species_Catalogue.csv"), row.names = F)
write.csv(past.dates, file.path(path.temp, "Old_Forecast_List.csv"), row.names = F)
