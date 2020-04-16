# -------------------------------------
# Load in the data and calculate ensemble spread
# -------------------------------------
path.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting"


# ---------------
# Doing some quick forecasts
# ---------------
library(nlme)
spp.forecast <- c("Quercus macrocarpa", "Quercus alba", "Acer rubrum", "Acer saccharum")
dat.comb <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.comb <- dat.comb[dat.comb$Species %in% species,]

dat.thresh <- data.frame(Species=spp.forecast, 
                         YDAY.mean=NA, YDAY.sd=NA,
                         GDD0.mean=NA, GDD0.sd=NA,
                         GDD5.mean=NA, GDD5.sd=NA,
                         YDAY.aic=NA, GDD0.aic=NA, GDD5.aic=NA)

for(SPP in spp.forecast){
  mod.yday <- lme(Yday ~ 1, random=list(PlantNumber=~1), data=dat.comb[dat.comb$Species==SPP,], na.action=na.omit)
  mod.gdd0 <- lme(GDD0.cum ~ 1, random=list(PlantNumber=~1), data=dat.comb[dat.comb$Species==SPP,], na.action=na.omit)
  mod.gdd5 <- lme(GDD5.cum ~ 1, random=list(PlantNumber=~1), data=dat.comb[dat.comb$Species==SPP,], na.action=na.omit)
  
  
  dat.thresh[dat.thresh$Species==SPP,"YDAY.mean"] <- summary(mod.yday)$tTable[,"Value"]
  dat.thresh[dat.thresh$Species==SPP,"YDAY.sd"] <- summary(mod.yday)$tTable[,"Std.Error"]*sqrt(summary(mod.yday)$tTable[,"DF"]+1)

  dat.thresh[dat.thresh$Species==SPP,"GDD0.mean"] <- summary(mod.gdd0)$tTable[,"Value"]
  dat.thresh[dat.thresh$Species==SPP,"GDD0.sd"] <- summary(mod.gdd0)$tTable[,"Std.Error"]*sqrt(summary(mod.gdd0)$tTable[,"DF"]+1)

  dat.thresh[dat.thresh$Species==SPP,"GDD5.mean"] <- summary(mod.gdd5)$tTable[,"Value"]
  dat.thresh[dat.thresh$Species==SPP,"GDD5.sd"] <- summary(mod.gdd5)$tTable[,"Std.Error"]*sqrt(summary(mod.gdd5)$tTable[,"DF"]+1)
  
  dat.thresh[dat.thresh$Species==SPP,"YDAY.aic"] <- AIC(mod.yday)[1]
  dat.thresh[dat.thresh$Species==SPP,"GDD0.aic"] <- AIC(mod.gdd0)[1]
  dat.thresh[dat.thresh$Species==SPP,"GDD5.aic"] <- AIC(mod.gdd5)[1]
  
}
dat.thresh
# ---------------


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
calc.bud <- function(dat, VAR, THRESH){
  min(dat[which(dat[,VAR] >= THRESH),"YDAY"])
}

hist.pred <- data.frame(Year=rep(min(dat.ghcn2$YEAR):(max(dat.ghcn2$YEAR)-1), 
                                 each=length(spp.forecast)),
                        Species=spp.forecast, 
                        YDAY=NA, YDAY.GDD5 = NA, YDAY.GDD0 = NA)
for(YR in unique(hist.pred$Year)){
  dat.yr <- dat.ghcn[dat.ghcn2$YEAR==YR,]
  for(SPP in spp.forecast){
    hist.pred[hist.pred$Year==YR & hist.pred$Species==SPP,"YDAY"] <- calc.bud(dat=dat.yr, VAR="YDAY", THRESH=dat.thresh[dat.thresh$Species==SPP,"YDAY.mean"])
    
    hist.pred[hist.pred$Year==YR & hist.pred$Species==SPP,"YDAY.GDD5"] <- calc.bud(dat=dat.yr, VAR="GDD5.cum", THRESH=dat.thresh[dat.thresh$Species==SPP,"GDD5.mean"])
    hist.pred[hist.pred$Year==YR & hist.pred$Species==SPP,"YDAY.GDD0"] <- calc.bud(dat=dat.yr, VAR="GDD0.cum", THRESH=dat.thresh[dat.thresh$Species==SPP,"GDD0.mean"])
    
  }
}
summary(hist.pred)

pheno.forecast <- data.frame(ENS=rep(unique(dat.forecast$ID), 
                                    each=length(spp.forecast)), 
                            Species=spp.forecast, 
                            YDAY=NA, YDAY.GDD5 = NA, YDAY.GDD0 = NA)
for(ENS in unique(dat.forecast$ID)){
  dat.ens <- dat.forecast[dat.forecast$ID==ENS,]
  for(SPP in spp.forecast){
    pheno.forecast[pheno.forecast$ENS==ENS & pheno.forecast$Species==SPP,"YDAY"] <- calc.bud(dat=dat.ens, VAR="YDAY", THRESH=dat.thresh[dat.thresh$Species==SPP,"YDAY.mean"])
    pheno.forecast[pheno.forecast$ENS==ENS & pheno.forecast$Species==SPP,"YDAY.GDD5"] <- calc.bud(dat=dat.ens, VAR="GDD5.cum", THRESH=dat.thresh[dat.thresh$Species==SPP,"GDD5.mean"])
    pheno.forecast[pheno.forecast$ENS==ENS & pheno.forecast$Species==SPP,"YDAY.GDD0"] <- calc.bud(dat=dat.ens, VAR="GDD0.cum", THRESH=dat.thresh[dat.thresh$Species==SPP,"GDD0.mean"])
  }
}
summary(pheno.forecast)

pheno.sum <- aggregate(pheno.forecast[,c("YDAY", "YDAY.GDD5", "YDAY.GDD0")],
                       by=list(pheno.forecast[,c("Species")]),
                       FUN=mean)
names(pheno.sum)[1] <- "Species"
pheno.sum[,c("GDD5.sd", "GDD0.sd")] <- aggregate(pheno.forecast[,c("YDAY.GDD5", "YDAY.GDD0")],
                                                   by=list(pheno.forecast[,c("Species")]),
                                                   FUN=sd)[,c("YDAY.GDD5", "YDAY.GDD0")]
pheno.sum[,c("GDD5.min", "GDD0.min")] <- aggregate(pheno.forecast[,c("YDAY.GDD5", "YDAY.GDD0")],
                       by=list(pheno.forecast[,c("Species")]),
                       FUN=min)[,c("YDAY.GDD5", "YDAY.GDD0")]
pheno.sum[,c("GDD5.max", "GDD0.max")] <- aggregate(pheno.forecast[,c("YDAY.GDD5", "YDAY.GDD0")],
                                                   by=list(pheno.forecast[,c("Species")]),
                                                   FUN=max)[,c("YDAY.GDD5", "YDAY.GDD0")]
pheno.sum

# Subset to just the forecasts
dat.forecast <- dat.forecast[dat.forecast$TYPE=="forecast",]
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
  geom_ribbon(data=ens.forecast$threshA, aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$threshA, aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.threshB <- ggplot(data=dat.ghcn) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=threshB), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=threshB, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=threshB, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$threshB, aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$threshB, aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

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

plot.prcp <- ggplot(data=dat.ghcn, aes(x=YDAY)) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=PRCP.cum, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=PRCP.cum, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$PRCP.cum, aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$PRCP.cum, aes(x=YDAY, y=mean, color="forecast")) +
 scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)])  +
  scale_y_continuous(name="Cum. Precip (mm)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.2, 0.9),
        legend.title=element_blank(),
        legend.background = element_blank())

plot.tmean <- ggplot(data=dat.ghcn, aes(x=YDAY)) +
  stat_summary(data=dat.ghcn, aes(x=YDAY, y=TMEAN), fun.y=mean, color="black", geom="line", size=1) +
  geom_line(data=dat.ghcn, aes(x=YDAY, y=TMEAN, group=YEAR), alpha=0.2, size=0.5) +
  geom_line(data=dat.ghcn[dat.ghcn$YEAR==lubridate::year(Sys.Date()), ], aes(x=YDAY, y=TMEAN, color="observed"), size=2) +
  # geom_ribbon(aes(fill="observed")) +
  geom_ribbon(data=ens.forecast$TMEAN, aes(x=YDAY, ymin=min, ymax=max, fill="forecast"), alpha=0.5) +
  geom_line(data=ens.forecast$TMEAN, aes(x=YDAY, y=mean, color="forecast")) +
  scale_color_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_fill_manual(name="data type", values = c("skyblue", "blue2")) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=2)], labels=day.labels$Text[seq(2, 12, by=2)])  +
  scale_y_continuous(name="Mean Daily Temp (C)" ,expand=c(0,0)) +
  theme_bw() +
  guides(fill=F) +
  theme(legend.position = c(0.5, 0.2),
        legend.title=element_blank(),
        legend.background = element_blank())

library(cowplot)
plot.title <- ggdraw() + 
  draw_label(paste("The Morton Arboretum weather, last updated:", max(dat.ghcn$DATE), "\n   "),
             fontface = 'bold', x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 1)
  )

plot.dat <- cowplot::plot_grid(plot.tmean, plot.prcp, plot.threshA, plot.threshB)


png(file.path(path.out, "figures", "Weather_latest.png"), height=6, width=8, units="in", res=220)
cowplot::plot_grid(plot.title, plot.dat, ncol=1, rel_heights = c(0.15, 1))
dev.off()


for(SPP in spp.forecast){
  print(SPP)
  
  plot.threshA.spp <- plot.threshA + 
    geom_vline(data=hist.pred[hist.pred$Species==SPP,],
               aes(xintercept =mean(YDAY))) 
  
  plot.threshB.spp <- plot.threshB + 
    geom_vline(data=dat.thresh[dat.thresh$Species==SPP,],
               aes(xintercept =YDAY.mean)) +
    geom_rect(data=pheno.sum[pheno.sum$Species==SPP,],
              aes(xmin = GDD5.min, xmax=GDD5.max, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.8)
  
  plot.tmean.spp <- plot.tmean + 
    geom_vline(data=dat.thresh[dat.thresh$Species==SPP,],
               aes(xintercept =YDAY.mean)) +
    geom_rect(data=pheno.sum[pheno.sum$Species==SPP,],
              aes(xmin = GDD5.min, xmax=GDD5.max, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.8)
  plot.prcp.spp <- plot.prcp + 
    geom_vline(data=dat.thresh[dat.thresh$Species==SPP,],
               aes(xintercept =YDAY.mean)) +
    geom_rect(data=pheno.sum[pheno.sum$Species==SPP,],
              aes(xmin = GDD5.min, xmax=GDD5.max, ymin=-Inf, ymax=Inf), fill="green4", alpha=0.8)
  
  plot.dat <- cowplot::plot_grid(plot.tmean.spp, plot.prcp.spp, plot.threshA.spp, plot.threshB.spp)
  
  plot.title <- ggdraw() + 
    draw_label(paste("The Morton Arboretum weather, last updated:", max(dat.ghcn$DATE), "\n    Forecast: ", SPP, ", green = predicted ", lubridate::year(Sys.Date())),
               fontface = 'bold', x = 0,hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 1)
    )
  
  
  png(file.path(path.out, "figures", paste0("Forecast_latest_", sub(" ", "_", SPP), ".png")), height=6, width=8, units="in", res=220)
  print(cowplot::plot_grid(plot.title, plot.dat, ncol=1, rel_heights = c(0.15, 1)))
  dev.off()
  
}
# -------------------------------------
