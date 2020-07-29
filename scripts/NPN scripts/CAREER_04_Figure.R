# Making a really cool 3-row figure for my proposal
# Row 1 (Panels A & B): Geogrpahic distribution of points & GDD estimates
# Row 2 (Panel C): Estimated species-level growing degree-day threshold
# Row 3 (Panel D): Effects of climate change on phenology

library(ggplot2)

dat.raw <- read.csv("../../data_processed/QURU_ACRU_NPN_combined.csv")
dat.raw$Date <- as.Date(dat.raw$Date)
summary(dat.raw)

dat.sites <- read.csv("../../data_raw/DAYMET/NPN_points.csv")
summary(dat.sites)

dat.sites2 <- aggregate(cbind(latitude, longitude) ~ site_id + site_name + site_id2, data=dat.raw, FUN=mean)
summary(dat.sites2)

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

dat.map <- merge(dat.map, dat.sites2)
summary(dat.map)

map.us <- map_data("state")
ggplot(data=dat.map) +
  facet_wrap(~Species)+
  coord_fixed(1.3, xlim=range(dat.map$longitude)+c(-1,1)) +
  ggtitle(paste("Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(aes(x=longitude, y=latitude, color=values), alpha=0.75) +
  scale_color_continuous(type = "viridis") +
  labs(color = "GDD5\nThreshold") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
# -------------------------------------------


# -------------------------------------------
# Row 2 (Panel C): Estimated species-level growing degree-day threshold
# -------------------------------------------
# -------------------------------------------

# -------------------------------------------
# Row 3 (Panel D): Effects of climate change on phenology
# -------------------------------------------
# -------------------------------------------


# -------------------------------------------
# Stitching everything together
# -------------------------------------------

# -------------------------------------------
