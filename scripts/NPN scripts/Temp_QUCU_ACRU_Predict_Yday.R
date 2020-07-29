# Predicting day of budburst for 
# 1) Current climatic norm from 1981-2010 (Daymet)
# 2) End-of-century prediction from MACA
# Workflow:
# 1: Read in MCMC model output
# 3: Read in Met Data: a) Daymet; b) MACA ensemble
# 4. Calculate accumulated GDD 
# 5. Extract predicted GDD
# 6. Plot

#---------------------------------------------
# 1. Read in model summary output
#---------------------------------------------
dat.quru <- read.csv("../../data_processed/CAREER_ModelOut_QURU_all.csv")
dat.acru <- read.csv("../../data_processed/CAREER_ModelOut_ACRU_all.csv")

set.seed(0728202015)
dat.quru <- dat.quru[sample(1:nrow(dat.quru), 1e3), c("THRESH", "aPrec")]
dat.acru <- dat.acru[sample(1:nrow(dat.acru), 1e3), c("THRESH", "aPrec")]
dat.quru$Species <- as.factor("Quercus rubra")
dat.acru$Species <- as.factor("Acer rubrum")

mod.npn <- rbind(dat.quru, dat.acru)
mod.npn$sd <- 1/sqrt(mod.npn[,"aPrec"])
test <- rnorm(1, mod.npn$THRESH, mod.npn$sd)
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
dat.maca[dat.maca$YEAR>=2070,]
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

dat.maca <- dat.maca[dat.maca$YEAR>=2070,]
#---------------------------------------------


#---------------------------------------------
# Find the predicted buddburst with current and future RCP
#---------------------------------------------
summary(mod.npn)
pred.daymet <- list()
for(YR in min(dat.daymet$year):max(dat.daymet$year)){
  pred.daymet[[YR]] <- mod.npn[sample(1:nrow(mod.npn), 500),]
  pred.daymet[[YR]]$year <- YR
  pred.daymet[[YR]]$yday.pred <- apply(pred.daymet[[YR]], 1, FUN=function(x){min(dat.daymet[dat.daymet$year==YR & dat.daymet$GDD5.cum>=as.numeric(x["thresh.pred"]), "yday"])})
}
pred.daymet <- dplyr::bind_rows(pred.daymet)
pred.daymet$model <- as.factor("Daymet")
pred.daymet$scenario <- as.factor("historic")
summary(pred.daymet)

pred.maca <- list()
for(MOD in unique(dat.maca$model)){
  mod.list <- list()
  dat.mod <- dat.maca[dat.maca$model==MOD,]
  for(RCP in unique(dat.mod$scenario)){
    rcp.list <- list()
    dat.rcp <- dat.mod[dat.mod$scenario==RCP, ]
    for(YR in min(dat.rcp$YEAR):max(dat.rcp$YEAR)){
      rcp.list[[YR]] <- mod.npn[sample(1:nrow(mod.npn), 500),]
      rcp.list[[YR]]$year <- YR
      rcp.list[[YR]]$yday.pred <- apply(rcp.list[[YR]], 1, FUN=function(x){min(dat.rcp[dat.rcp$year==YR & dat.rcp$GDD5.cum>=as.numeric(x["thresh.pred"]), "YDAY"])}) 
    }
    mod.list[[RCP]] <- dplyr::bind_rows(rcp.list)
    mod.list[[RCP]]$scenario <- RCP
  }
  pred.maca[[MOD]] <- dplyr::bind_rows(mod.list)
  pred.maca[[MOD]]$model <- MOD
}
pred.maca <- dplyr::bind_rows(pred.maca)
summary(pred.maca)

yday.pred <- rbind(pred.daymet, pred.maca[,names(pred.daymet)])
summary(yday.pred)
write.csv(yday.pred, "../../data_processed/CAREER_BudBurst_Predictions.csv", row.names=F)

ggplot(data=yday.pred) +
  facet_grid(scenario~.) +
  geom_density(aes(x=yday.pred, fill=Species), alpha=0.5)
#---------------------------------------------

