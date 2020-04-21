library(ggplot2)
# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting"


# ---------------
# Met Data
# ---------------
dat.ghcn <- read.csv(file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)
summary(dat.ghcn)

dat.forecast <- read.csv(file.path(path.out, "data", "Weather_Arb_forecast_ensemble_latest.csv"))
dat.forecast$DATE <- as.Date(dat.forecast$DATE)
# 
summary(dat.forecast)

# Add species predictions to the forecast




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
day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

if(Sys.Date()<=as.Date(paste0(lubridate::year(Sys.Date()), "-06-20"))){
  dat.ghcn$threshA <- dat.ghcn$GDD0.cum 
  dat.ghcn$threshB <- dat.ghcn$GDD5.cum
  ens.forecast$threshA <- ens.forecast$GDD0.cum
  ens.forecast$threshB <- ens.forecast$GDD5.cum
} else {
  dat.ghcn$threshA <- dat.ghcn$CDD0.cum 
  dat.ghcn$threshB <- dat.ghcn$CDD2.cum 
  ens.forecast$threshA <- ens.forecast$CDD0.cum
  ens.forecast$threshB <- ens.forecast$CDD0.cum
}

plot.threshA <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshA), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=threshA, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=threshA, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$threshA[ens.forecast$threshA$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$threshA[ens.forecast$threshA$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot.threshB <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshB), fun.y=mean, color="black", geom="line", size=1) +
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
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. GrowDD, base 0 C" ,expand=c(0,0)) + 
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)]) +
    coord_cartesian(ylim=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY<=180])))
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. GrowDD, base 5 C" ,expand=c(0,0)) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(1, 180), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)])  +
    coord_cartesian(ylim=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY<=180])))
  
  # pheno.sum; dat.thresh
} else {
  plot.threshA <- plot.threshA + 
    scale_y_continuous(name="Cum. ChillDD, base 0 C" ,expand=c(0,0)) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)])  +
    coord_cartesian(ylim=c(0, max(dat.ghcn$threshA[dat.ghcn$YDAY>=155])))
  
  
  plot.threshB <- plot.threshB + 
    scale_y_continuous(name="Cum. ChillDD, base -2 C" ,expand=c(0,0)) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), limits=c(155, 366), breaks=day.labels$yday[seq(2, 12, by=1)], labels=day.labels$Text[seq(2, 12, by=1)])   +
    coord_cartesian(ylim=c(0, max(dat.ghcn$threshB[dat.ghcn$YDAY>=155])))
}  

plot.prcp <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=PRCP.cum, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$PRCP.cum[ens.forecast$PRCP.cum$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$PRCP.cum[ens.forecast$PRCP.cum$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
 scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)])  +
  scale_y_continuous(name="Cum. Precip (mm)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

plot.tmean <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=TMEAN), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=TMEAN, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=TMEAN, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$TMEAN[ens.forecast$TMEAN$TYPE=="forecast",], aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)])  +
  scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.5, 0.2),
        legend.title=element_blank(),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

library(cowplot)
plot.title <- ggdraw() + 
  draw_label(paste("The Morton Arboretum weather, last updated:", Sys.Date(), "\n   "),
             fontface = 'bold', x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 1)
  )

plot.dat <- cowplot::plot_grid(plot.tmean, plot.prcp, plot.threshB, plot.threshA)


png(file.path(path.out, "figures", "Weather_latest.png"), height=6, width=8, units="in", res=220)
cowplot::plot_grid(plot.title, plot.dat, ncol=1, rel_heights = c(0.15, 1))
dev.off()

calc.bud <- function(dat, VAR, THRESH){
  min(dat[which(dat[,VAR] >= THRESH),"YDAY"])
}


day.labels2 <- data.frame(Date=seq.Date(as.Date("2020-01-03"), as.Date("2020-7-01"), by="day"))
day.labels2$yday <- lubridate::yday(day.labels2$Date)
day.labels2$Text <- paste(lubridate::month(day.labels2$Date, label=T), lubridate::day(day.labels2$Date))
summary(day.labels2)

spp.forecast <- "Quercus macrocarpa"
ens <- unique(dat.forecast$ID)
SPP=spp.forecast
for(SPP in spp.forecast){
  gdd.prior <- read.csv(file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", SPP), ".csv")))
  summary(gdd.prior)
  
  thresh <- sample(gdd.prior$THRESH, 200)
  pred.array <- array(dim=c(length(thresh), length(unique(dat.forecast$ID))))
  
  for(i in 1:length(ens)){
    pred.array[,i] <- unlist(lapply(dat=dat.forecast[dat.forecast$ID==ens[i],], FUN=calc.bud, VAR="GDD5.cum", thresh))
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
  
  
  plot.yday.dens <- ggplot() + 
    geom_density(data=pred.df, aes(x=x), adjust=1.5, fill="green3", alpha=0.5) +
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

    plot.threshB.spp <- plot.threshB + 
    geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)
  
  plot.tmean.spp <- plot.tmean +
    geom_rect(data=dat.lim["q75",],
              aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)

  plot.prcp.spp <- plot.prcp + 
      geom_rect(data=dat.lim["q75",],
                aes(xmin = lb, xmax=ub, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.5)
    
  plot.dat <- cowplot::plot_grid(plot.tmean.spp, plot.prcp.spp, plot.threshB.spp, plot.yday.dens)
  
 
  plot.title <- ggdraw() + 
    draw_label(paste("The Morton Arboretum Bud Burst Forecast (updated:", Sys.Date(), ")\n   ", SPP, ": ", pred.range[1], " - ", pred.range[2], " (75% CI)"),
               fontface = 'bold', x = 0,hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, l=10)
    )
  
  
  png(file.path(path.out, "figures", paste0("Forecast_latest_", sub(" ", "_", SPP), ".png")), height=6, width=8, units="in", res=220)
  print(cowplot::plot_grid(plot.title, plot.dat, ncol=1, rel_heights = c(0.15, 1)))
  dev.off()
  
}
# -------------------------------------
