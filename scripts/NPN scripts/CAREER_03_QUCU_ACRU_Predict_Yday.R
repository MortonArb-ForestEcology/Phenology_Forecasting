# Predicting day of budburst for 
# 1) Current climatic norm from 1981-2010 (Daymet)
# 2) End-of-century prediction from MACA
# Workflow:
# 1: Read in MCMC model output
# 3: Read in Met Data: a) Daymet; b) MACA ensemble
# 4. Calculate accumulated GDD 
# 5. Extract predicted GDD
# 6. Plot
library(ggplot2)
#---------------------------------------------
# 1. Read in model summary output
#---------------------------------------------
dat.quru <- read.csv("../../data_processed/CAREER_ModelOut_QURU_all.csv")
dat.acru <- read.csv("../../data_processed/CAREER_ModelOut_ACRU_all.csv")

# set.seed(0728202015)
# dat.quru <- dat.quru[sample(1:nrow(dat.quru), 1e3), c("THRESH", "aPrec")]
# dat.acru <- dat.acru[sample(1:nrow(dat.acru), 1e3), c("THRESH", "aPrec")]
dat.quru$Species <- as.factor("Quercus rubra")
dat.acru$Species <- as.factor("Acer rubrum")

quru.quant.thresh <- quantile(dat.quru$THRESH, c(0.005, 0.995))
quru.quant.thresh <- quantile(dat.quru$THRESH, c(0.005, 0.995))
dat.quru <- quantile

mod.npn <- rbind(dat.quru[,c("THRESH", "aPrec", "Species")], dat.acru[,c("THRESH", "aPrec", "Species")])
mod.npn$sd <- 1/sqrt(mod.npn[,"aPrec"])
mod.npn$thresh.pred <- apply(mod.npn, 1, FUN=function(x){rnorm(1, as.numeric(x["THRESH"]), as.numeric(x["sd"]))})
summary(mod.npn)

ggplot(data=mod.npn) +
  geom_density(aes(x=THRESH, fill=Species), alpha=0.5)
#---------------------------------------------

#---------------------------------------------
# 2. Read in met datasets
# 3. Calculate GDD
#---------------------------------------------
path.met <- "/Volumes/GoogleDrive/My Drive/Arboretum Met Data/"
dat.daymet <- read.csv(file.path(path.met,"Daymet/MortonArb-VisitorCenter/Daymet_MortonArb_1980-2018.csv"))
dat.daymet$tmax..deg.c. <- dat.daymet$tmax.C
dat.daymet$tmin..deg.c. <- dat.daymet$tmin.C
summary(dat.daymet)

dat.maca <- read.csv(file.path(path.met, "GCM_future_Projections/MACAv2_raw/MACA.v2_daily_gcms.csv"), stringsAsFactors = T)
dat.maca$year <- dat.maca$YEAR
dat.maca$yday <- dat.maca$yday
dat.maca$tmax..deg.c. <- dat.maca$TMAX
dat.maca$tmin..deg.c. <- dat.maca$TMIN
# dat.maca[dat.maca$YEAR>=2070,]
summary(dat.maca)

# Turning data into a list to make life easier
list.daymet <- list()
for(YR in unique(dat.daymet$year)){
  list.daymet[[paste(YR)]] <- dat.daymet[dat.daymet$year==YR,]
}
summary(list.daymet)

list.maca <- list()
for(MOD in unique(dat.maca$model)){
  list.maca[[MOD]] <- list()
  
  dat.mod <- dat.maca[dat.maca$model==MOD,]
  for(RCP in unique(dat.mod$scenario)){
    list.maca[[MOD]][[RCP]] <- list()
    
    dat.rcp <- dat.mod[dat.mod$scenario==RCP,]
    for(YR in min(dat.maca$YEAR):max(dat.maca$YEAR)){
      list.maca[[MOD]][[RCP]][[YR]] <- dat.rcp[dat.rcp$YEAR==YR,]
    }
  }
}

source("../weather_calc.R")

list.daymet<- lapply(list.daymet, weather_calc)
dat.daymet <- dplyr::bind_rows(list.daymet)
summary(dat.daymet)

write.csv(dat.daymet, "../../data_processed/CAREER_Met_Arb_Daymet_GDD.csv", row.names=F)


list.maca2 <- list()
for(MOD in names(list.maca)){
  mod.list <- list()
  for(RCP in names(list.maca[[MOD]])){
    list.maca[[MOD]][[RCP]] <- lapply(list.maca[[MOD]][[RCP]], weather_calc)
    
    mod.list[[RCP]] <- dplyr::bind_rows(list.maca[[MOD]][[RCP]])
  }
  list.maca2[[MOD]] <- dplyr::bind_rows(mod.list)
}
summary(list.maca2[[1]])
dat.maca <- dplyr::bind_rows(list.maca2)

write.csv(dat.maca, "../../data_processed/CAREER_Met_Arb_MACA_GDD.csv", row.names=F)

#---------------------------------------------


#---------------------------------------------
# Find the predicted buddburst with current and future RCP
#---------------------------------------------
dat.quru <- read.csv("../../data_processed/CAREER_ModelOut_QURU_all.csv")
dat.acru <- read.csv("../../data_processed/CAREER_ModelOut_ACRU_all.csv")

set.seed(0728202015)
dat.quru <- dat.quru[sample(1:nrow(dat.quru), 2e3), c("THRESH", "aPrec")]
dat.acru <- dat.acru[sample(1:nrow(dat.acru), 2e3), c("THRESH", "aPrec")]
dat.quru$Species <- as.factor("Quercus rubra")
dat.acru$Species <- as.factor("Acer rubrum")

mod.npn <- rbind(dat.quru, dat.acru)
mod.npn$sd <- 1/sqrt(mod.npn[,"aPrec"])
mod.npn$thresh.pred <- apply(mod.npn, 1, FUN=function(x){rnorm(1, as.numeric(x["THRESH"]), as.numeric(x["sd"]))})
summary(mod.npn)


dat.daymet <- read.csv("../../data_processed/CAREER_Met_Arb_Daymet_GDD.csv", stringsAsFactors = T)
dat.maca <- read.csv("../../data_processed/CAREER_Met_Arb_MACA_GDD.csv", stringsAsFactors = T)


dat.daymet$Freeze <- ifelse(dat.daymet$tmin.C<0, 1, 0)
summary(dat.daymet)

dat.maca$Freeze <- ifelse(dat.maca$TMIN<0, 1, 0)
summary(dat.maca)

# dat.maca <- dat.maca[dat.maca$YEAR>=2070,]
# dat.maca[dat.maca$YEAR<=2030,"scenario"] <- "modeled present"
# dat.maca <- dat.maca[dat.maca$YEAR>=2035 & dat.maca$YEAR<2065,]


summary(mod.npn)
summary(dat.daymet)
daymet.norm <- aggregate(cbind(GDD5.cum, Freeze) ~ yday, data=dat.daymet[dat.daymet$year>=1998,], FUN=mean)
daymet.norm$model <- as.factor("Daymet")
daymet.norm$scenario <- as.factor("present")
summary(daymet.norm)

dat.maca$yday <- dat.maca$YDAY
maca.norm.present <- aggregate(cbind(GDD5.cum, Freeze) ~ yday + scenario, data=dat.maca[dat.maca$YEAR<=2025,], FUN=mean)
maca.norm.present$model <- as.factor("MACA present")
# maca.norm.present$scenario <- as.factor("modeled present")
summary(maca.norm.present)

maca.norm.future <- aggregate(cbind(GDD5.cum, Freeze) ~ yday + scenario, data=dat.maca[dat.maca$YEAR>=2041 & dat.maca$YEAR<=2060,], FUN=mean)
# maca.norm.future$TMIN <- aggregate(TMIN ~ yday + scenario, data=dat.maca[dat.maca$YEAR>=2041 & dat.maca$YEAR<=2060,], FUN=mean)
maca.norm.future$model <- as.factor("MACA mid-century")
summary(maca.norm.future)

maca.norm.future2 <- aggregate(cbind(GDD5.cum, Freeze) ~ yday + scenario, data=dat.maca[dat.maca$YEAR>=2080,], FUN=mean)
maca.norm.future2$model <- as.factor("MACA end-century")
summary(maca.norm.future2)

dat.met <- rbind(daymet.norm, maca.norm.present[,names(daymet.norm)], maca.norm.future[,names(daymet.norm)], maca.norm.future2[,names(daymet.norm)])
summary(dat.met)

write.csv(dat.met, "../../data_processed/CAREER_Met_Predictions.csv", row.names=F)


pred.list <- list()
for(MOD in unique(dat.met$model)){
  for(RCP in unique(dat.met[dat.met$model==MOD, "scenario"])){
    met.now <- dat.met[dat.met$model==MOD & dat.met$scenario==RCP,]
    npn.samp <- mod.npn
    # npn.samp <- mod.npn[sample(1:nrow(mod.npn), 500),]
    
    pred.list[[paste(MOD, RCP, sep="-")]] <- mod.npn
    pred.list[[paste(MOD, RCP, sep="-")]]$model <- as.factor(MOD)
    pred.list[[paste(MOD, RCP, sep="-")]]$scenario <- as.factor(RCP)
    pred.list[[paste(MOD, RCP, sep="-")]]$yday.pred <- apply(npn.samp, 1, FUN=function(x){min(met.now[met.now$GDD5.cum>=as.numeric(x["thresh.pred"]), "yday"])})
    
    pred.list[[paste(MOD, RCP, sep="-")]]$freeze.prob <- apply(pred.list[[paste(MOD, RCP, sep="-")]], 1, FUN=function(x){met.now$Freeze[met.now$yday==round(as.numeric(x["yday.pred"]))]})
  }
}
yday.pred <- dplyr::bind_rows(pred.list)
summary(yday.pred)

write.csv(yday.pred, "../../data_processed/CAREER_BudBurst_Predictions.csv", row.names=F)

# finding different dates of last freeze
freeze.time <- data.frame(model=rep(unique(dat.met$model), length(unique(dat.met$scenario))),
                          scenario=rep(unique(dat.met$scenario), each=length(unique(dat.met$model))))
for(i in 1:nrow(freeze.time)){
  met.now <- dat.met[dat.met$model==freeze.time$model[i] & dat.met$scenario==freeze.time$scenario[i],]
  d10 <- 0.10 - met.now$Freeze[met.now$yday<180]
  d20 <- 0.20 - met.now$Freeze[met.now$yday<180]
  d25 <- 0.25 - met.now$Freeze[met.now$yday<180]
  d50 <- 0.50 - met.now$Freeze[met.now$yday<180]
  d75 <- 0.75 - met.now$Freeze[met.now$yday<180]
  
  freeze.time[i,"p10"] <- max(met.now$yday[which(abs(d10)==min(abs(d10)))])
  freeze.time[i,"p20"] <- max(met.now$yday[which(abs(d20)==min(abs(d20)))])
  freeze.time[i,"p25"] <- max(met.now$yday[which(abs(d25)==min(abs(d25)))])
  freeze.time[i,"p50"] <- max(met.now$yday[which(abs(d50)==min(abs(d50)))])
  freeze.time[i,"p75"] <- max(met.now$yday[which(abs(d75)==min(abs(d75)))])
  
}
freeze.time <- freeze.time[freeze.time$p25 > -Inf, ]

ggplot(data=yday.pred) +
  facet_grid(model~scenario) +
  geom_density(aes(x=yday.pred, fill=Species), alpha=0.5, adjust=3) +
  geom_vline(data=freeze.time, aes(xintercept=p25, linetype=scenario), color="black")

ggplot(data=yday.pred[yday.pred$model %in% c("MACA present", "MACA mid-century") & yday.pred$scenario %in% c("rcp45"),]) +
  facet_grid(model~scenario) +
  geom_density(aes(x=yday.pred, fill=Species), alpha=0.5, adjust=3)


day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-06-30"), by=7))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

plot.annotate <- aggregate(yday.pred ~ Species + model + scenario, data=yday.pred, FUN=median)
names(plot.annotate)[names(plot.annotate)=="yday.pred"] <- "yday.median"
plot.annotate$yday.q05 <- aggregate(yday.pred ~ Species + model + scenario, data=yday.pred, FUN=quantile, 0.05)$yday.pred
plot.annotate$yday.q95 <- aggregate(yday.pred ~ Species + model + scenario, data=yday.pred, FUN=quantile, 0.95)$yday.pred
plot.annotate$yday.90range <- plot.annotate$yday.q95 - plot.annotate$yday.q05
plot.annotate

path.g <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/For NSF career grant/"

png(paste0(path.g, "ModelPredict_YDAY.png"), height=6, width=6, units="in", res=220)
ggplot(data=yday.pred[yday.pred$model %in% c("MACA present", "MACA mid-century") & yday.pred$scenario %in% c("rcp45"),]) +
  # facet_grid(scenario~.) +
  geom_density(aes(x=yday.pred, color=Species, linetype=model), fill=NA, adjust=5, size=3) +
  geom_vline(data=freeze.time[freeze.time$model %in% c("MACA present", "MACA mid-century") & freeze.time$scenario %in% c("rcp45"),], aes(xintercept=p25, linetype=model), color="black") +
  scale_y_continuous(name="Probability", expand=c(0,0), limits=c(0,0.4)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[], labels=day.labels$Text[]) +
  theme_minimal()
dev.off()
#---------------------------------------------

