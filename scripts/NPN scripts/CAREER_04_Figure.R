# Making a really cool 3-row figure for my proposal
# Row 1 (Panels A & B): Geogrpahic distribution of points & GDD estimates
# Row 2 (Panel C): Estimated species-level growing degree-day threshold
# Row 3 (Panel D): Effects of climate change on phenology

library(ggplot2)

dat.raw <- read.csv("../../data_processed/QURU_ACRU_NPN_combined.csv")
dat.raw$Date <- as.Date(dat.raw$Date)
summary(dat.raw)

dat.sites <- read.csv("../../data_raw/DAYMET/NPN_points.csv")
dat.sites$site_name <- paste0("X", dat.sites$site_id)
summary(dat.sites)


mod.quru <- read.csv("../../data_processed/CAREER_ModelOut_QURU_all.csv")
mod.acru <- read.csv("../../data_processed/CAREER_ModelOut_ACRU_all.csv")

# mod.quru$Species <- as.factor("Quercus rubra")
# mod.acru$Species <- as.factor("Acer rubrum")

set.seed(0728202015)
mod.quru <- mod.quru[sample(1:nrow(mod.quru), 1e3), ]
mod.acru <- mod.acru[sample(1:nrow(mod.acru), 1e3), ]

# -------------------------------------------
# Row 1 (Panels A & B): Geogrpahic distribution of points & GDD estimates
# -------------------------------------------
quru.long <- stack(mod.quru)
acru.long <- stack(mod.acru)
quru.long$Species <- as.factor("Quercus rubra")
acru.long$Species <- as.factor("Acer rubrum")
quru.long$iter <- 1:nrow(mod.quru)
acru.long$iter <- 1:nrow(mod.acru)

mod.long <- rbind(acru.long[!acru.long$ind %in% c("aPrec", "THRESH"),], 
                  quru.long[!quru.long$ind %in% c("aPrec", "THRESH"),])
names(mod.long)[1:2] <- c("values", "site_name")
summary(mod.long)

dat.map <- aggregate(values ~ site_name + Species, data=mod.long, FUN=mean)
summary(dat.map)

dat.map <- merge(dat.map, dat.sites)
dat.map$Species.lab <- ifelse(dat.map$Species=="Acer rubrum", paste("a)", dat.map$Species), paste("b)", dat.map$Species))
summary(dat.map)

map.us <- map_data("state")
plot1 <- ggplot(data=dat.map) +
  facet_wrap(~Species.lab)+
  coord_fixed(1.3, xlim=range(dat.map$longitude)+c(-1,1)) +
  # ggtitle(paste("Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="gray50", size=0.5) +
  geom_point(aes(x=longitude, y=latitude, color=values), alpha=0.75, size=5) +
  scale_color_continuous(type = "viridis") +
  labs(color = "GDD5\nThreshold") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank(),
        # legend.position = "bottom",
        legend.position = c(0.5, 0.1),
        legend.title = element_text(size=rel(1.25), face="bold"),
        legend.text = element_text(size=rel(1.2)),
        legend.key.size=unit(1.5, "lines"),
        legend.margin = margin(c(0.1,0.1,0.1,0.1), "lines"),
        legend.direction = "horizontal",
        # plot.margin = margin(c(0,0,0,0), "lines"),
        strip.background = element_blank(),
        strip.text = element_text(hjust=0, margin = unit(c(0,0,0,0), "lines"), size=rel(2), face='bold'))
# -------------------------------------------


# -------------------------------------------
# Row 2 (Panel C): Estimated species-level growing degree-day threshold
# -------------------------------------------
mod.gdd <- rbind(mod.quru[,c("THRESH", "aPrec")], mod.acru[,c("THRESH", "aPrec")])
mod.gdd$Species <- as.factor(c(rep("Quercus rubra", nrow(mod.quru)), rep("Acer rubrum", nrow(mod.acru))))
mod.gdd$sd <- 1/sqrt(mod.gdd[,"aPrec"])
mod.gdd$thresh.pred <- apply(mod.gdd, 1 , function(x) rnorm(1, mean=as.numeric(x["THRESH"]), sd=as.numeric(x["sd"])))
summary(mod.gdd)

plot2 <- ggplot(data=mod.gdd) +
  geom_density(aes(x=thresh.pred, fill=Species, color=Species), alpha=0.5, size=3) +
  scale_y_continuous(name="Prob.", expand=c(0,0), limits=c(0,0.077)) +
  scale_x_continuous(name=c("GDD Threshold"), expand=c(0,0)) +
  scale_color_manual(values=c("#0072B2", "#D55E00")) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  theme(panel.background=element_rect(fill=NA, color="black"),
        panel.grid = element_blank(),
        legend.position=c("top"),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.text = element_text(size=rel(1.25)),
        legend.background = element_blank(),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title = element_text(size=rel(1.25), color="black", face="bold"))
# -------------------------------------------

# -------------------------------------------
# Row 3 (Panel D): Effects of climate change on phenology
# -------------------------------------------
yday.pred <- read.csv("../../data_processed/CAREER_BudBurst_Predictions.csv", stringsAsFactors = T)
yday.pred$model <- factor(yday.pred$model, levels=c("Daymet", "MACA present", "MACA mid-century", "MACA end-century"))
summary(yday.pred)
summary(yday.pred$model)

# finding different dates of last freeze
dat.met <- read.csv("../../data_processed/CAREER_Met_Predictions.csv", stringsAsFactors = T)
summary(dat.met)

freeze.time <- data.frame(model=rep(unique(dat.met$model), length(unique(dat.met$scenario))),
                          scenario=rep(unique(dat.met$scenario), each=length(unique(dat.met$model))))
for(i in 1:nrow(freeze.time)){
  met.now <- dat.met[dat.met$model==freeze.time$model[i] & dat.met$scenario==freeze.time$scenario[i],]
  d25 <- 0.25 - met.now$Freeze[met.now$yday<180]
  d50 <- 0.50 - met.now$Freeze[met.now$yday<180]
  d75 <- 0.75 - met.now$Freeze[met.now$yday<180]
  
  freeze.time[i,"p25"] <- median(met.now$yday[which(abs(d25)==min(abs(d25)))])
  freeze.time[i,"p50"] <- median(met.now$yday[which(abs(d25)==min(abs(d25)))])
  freeze.time[i,"p75"] <- median(met.now$yday[which(abs(d25)==min(abs(d25)))])
  
}

day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-06-30"), by=7))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

yday.pred$Species <- factor(ypred$Species, levels=c("Acer rubrum", "Quercus rubra"))

plot3 <- ggplot(data=yday.pred[yday.pred$model %in% c("MACA present", "MACA mid-century") & yday.pred$scenario %in% c("rcp45"),]) +
  # facet_grid(scenario~.) +
  geom_density(aes(x=yday.pred, color=Species, linetype=model), fill=NA, adjust=5, size=3) +
  geom_vline(data=freeze.time[freeze.time$model %in% c("MACA present", "MACA mid-century") & freeze.time$scenario %in% c("rcp45"),], aes(xintercept=p25, linetype=model), color="black", size=2.5) +
  # geom_vline(data=freeze.time[freeze.time$model=="MACA mid-century" & freeze.time$scenario %in% c("rcp45"),], aes(xintercept=p25), linetype="dashed", color="black", size=3) +
  # geom_vline(data=freeze.time[freeze.time$model=="MACA present" & freeze.time$scenario %in% c("rcp45"),], aes(xintercept=p25), linetype="solid", color="black", size=3) +
  scale_y_continuous(name="Prob.", expand=c(0,0), limits=c(0,0.4)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[], labels=day.labels$Text[]) +
  scale_color_manual(values=c("#0072B2", "#D55E00")) +
  scale_fill_manual(values=c("#0072B2", "#D55E00")) +
  scale_linetype_manual(name="Time", values=c("solid", "dotted"), labels=c("present", "mid-century")) +
  guides(color=F) +
  theme(panel.background=element_rect(fill=NA, color="black"),
        panel.grid = element_blank(),
        legend.position=c("top"),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.background = element_blank(),
        legend.key = element_rect(fill=NA),
        # legend.key.size = 
        axis.text = element_text(size=rel(1), color="black"),
        axis.title = element_text(size=rel(1.25), color="black", face="bold"))
# -------------------------------------------


# -------------------------------------------
# Stitching everything together
# -------------------------------------------
path.g <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/For NSF career grant/"

png(file.path(path.g, "PhenoPredict_Final.png"), height=8, width=6, units="in", res=220)
cowplot::plot_grid(plot1, plot2, plot3, ncol=1, labels=c("", "c)", "d)"), rel_heights = c(1,0.5, 0.5), label_fontface = "bold", label_size = 24)
dev.off()
# -------------------------------------------
