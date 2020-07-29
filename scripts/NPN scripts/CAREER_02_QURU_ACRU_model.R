library(rjags)
library(coda)
library(ggmcmc)
library(dplyr)

# path.g <- "G:/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/For NSF career grant/"
path.g <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/For NSF career grant/"

# Read in output of previous script
dat.all <- read.csv("../../data_processed/QURU_ACRU_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)

dat.sites <- read.csv("../../data_raw/DAYMET/NPN_points.csv")
summary(dat.sites)
dat.sites[dat.sites$n.obs>100,] # This is the Harvard Forest. Because of course it's the Harvard Forest
hist(dat.sites$n.obs[dat.sites$n.obs<200])
length(which(dat.sites$n.obs>2));nrow(dat.sites)

dat.all$species_name <- as.factor(dat.all$species_name)
summary(dat.all[dat.all$site_id=="35875",])
summary(dat.all[dat.all$site_id=="35875",])
length(unique(dat.all$individual_id[dat.all$site_id=="35875"]))
summary(dat.all[dat.all$site_id=="Maple Collection",])

summary(dat.all)
dat.all <- dat.all[dat.all$GDD5.cum > 0,]
dat.all <- dat.all[dat.all$site_id %in% dat.sites$site_id[dat.sites$n.obs>2],]

# dat.all$site_id <- site_ids$station_name[match(dat.all$site_id, site_ids$station_id)]

#Making sure different locations with the same name are given unique names by adding site_id
# for(Name in unique(dat.all$site_id)){
#   dat.tmp <- dat.all[dat.all$site_id == Name,]
#   if(length(unique(dat.tmp$site_id)) >1){
#     dat.tmp$site_id <- paste(dat.tmp$site_id, dat.tmp$site_id, sep="_")
#   }
#   dat.all[dat.all$site_id==Name, "site_id"] <- dat.tmp$site_id
# }

png(paste0(path.g, "Histograms_NPN_data_yday.png"), height=6, width=6, units="in", res=220)
ggplot(data=dat.all) +
  facet_grid(species_name~.) +
  geom_histogram(aes(x=Yday, fill=as.factor(site_id))) +
  guides(fill=F)
dev.off()

png(paste0(path.g, "Histograms_NPN_data_gdd5.png"), height=6, width=6, units="in", res=220)
ggplot(data=dat.all) +
  facet_grid(species_name~.) +
  geom_histogram(aes(x=GDD5.cum, fill=as.factor(site_id))) +
  guides(fill=F)
dev.off()

dat.quru <- dat.all[dat.all$species_name == "Quercus rubra", ]
dat.acru <- dat.all[dat.all$species_name == "Acer rubrum", ]

dat.quru <- dat.quru[dat.quru$site_id2 %in% unique(dat.acru$site_id2),]
dat.acru <- dat.acru[dat.acru$site_id2 %in% unique(dat.quru$site_id2),]

#--------------------------------------------#
# #Pulling out unique sites
quru.sites <- as.data.frame(table(dat.quru$site_id))
colnames(quru.sites) <- c("site_id", "Freq")

acru.sites <- as.data.frame(table(dat.acru$site_id))
colnames(acru.sites) <- c("site_id", "Freq")
# 
# #Making sure they only use matching sites
# #This is the second removal because some sites may ahve clean data for one species but not the other
# 
# summary(dat.quru)
# summary(dat.acru)
# 
# #Pulled out for matching the site_id names to the JAGS output
quru.match <- as.data.frame(table(dat.quru$site_id))
colnames(quru.match) <- c("site_id", "Freq")

acru.match <- as.data.frame(table(dat.acru$site_id))
colnames(acru.match) <- c("site_id", "Freq")

#Pulling site locations for each individual to help hierarchy indexing
quru.ind <- aggregate(site_id~individual_id, data=dat.quru,
                 FUN=min)

acru.ind <- aggregate(site_id~individual_id, data=dat.acru,
                      FUN=min)


hierarchical_regression <- "
  model{
      
    for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(Tprior, aPrec[j])
      aPrec[j] ~ dgamma(1, 0.1)
    }

    for(t in 1:nLoc){
      site_id[t] <-  THRESH[sp[t]] + b[t]
      b[t] ~ dnorm(0, bPrec[t])
      bPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  site_id[loc[i]] * c[i]
        c[i] ~ dnorm(1, cPrec)
    }
    
    for(k in 1:n){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
    
    Tprior ~ dunif(100, 200)
    sPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
  }
"
quru.list <- list(y = dat.quru$GDD5.cum, n = length(dat.quru$GDD5.cum),
                   loc = as.numeric(factor(quru.ind$site_id)), nLoc = length(unique(dat.quru$site_id)),
                   pln = as.numeric(factor(dat.quru$individual_id)), nPln = length(unique(dat.quru$individual_id)),
                   sp = as.numeric(factor(dat.quru$species)), nSp = length(unique(dat.quru$species)))

acru.list <- list(y = dat.acru$GDD5.cum, n = length(dat.acru$GDD5.cum),
                  loc = as.numeric(factor(acru.ind$site_id)), nLoc = length(unique(dat.acru$site_id)),
                  pln = as.numeric(factor(dat.acru$individual_id)), nPln = length(unique(dat.acru$individual_id)),
                  sp = as.numeric(factor(dat.acru$species)), nSp = length(unique(dat.acru$species)))


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(sPrec = runif(1,1/200, 1/20))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
quru.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = quru.list,
                             inits = inits,
                             n.chains = nchain)

acru.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = acru.list,
                             inits = inits,
                             n.chains = nchain)

#Converting the output into a workable format
quru.out   <- coda.samples (model = quru.model,
                             variable.names = c("site_id", "THRESH", "aPrec"),
                             n.iter = 200000)

#Converting the output into a workable format
acru.out   <- coda.samples (model = acru.model,
                            variable.names = c("site_id", "THRESH", "aPrec"),
                            n.iter = 200000)


#Renaming parameters to properly match their effects (e.g. sites are renamed to their site_id, species to their species)
# varnames(quru.out)
varnames(quru.out)[grep("site", varnames(quru.out))] <- as.character(quru.match$site_id)
varnames(acru.out)[grep("site", varnames(acru.out))] <- as.character(acru.match$site_id)

# #Checking that convergence happened
gelman.diag(quru.out)
gelman.diag(acru.out)


#Removing burnin before convergence occurred
burnin = 198000   ## determine convergence from GBR output
quru.burn <- window(quru.out,start=burnin)## remove burn-in
acru.burn <- window(acru.out,start=burnin)
summary(quru.burn)
summary(acru.burn)

quru.stats.all <- as.data.frame(as.matrix(quru.burn))
acru.stats.all <- as.data.frame(as.matrix(acru.burn))
dim(acru.stats.all)

write.csv(quru.stats.all, "../../data_processed/CAREER_ModelOut_QURU_all.csv", row.names=F)
write.csv(acru.stats.all, "../../data_processed/CAREER_ModelOut_ACRU_all.csv", row.names=F)

#-------------------------------------------------------------------------------#
#Here begins the NPN map visualisation#
#-------------------------------------------------------------------------------#
df.quru <- ggs(quru.burn)
df.acru <- ggs(acru.burn)
df.quru$Species <- 'Quercus rubra'
df.acru$Species <- 'Acer rubrum'
# summary(df.quru)

df <- rbind(df.quru, df.acru)
dat.vis <- aggregate(df$value,
                     by=list(df$Species, df$Parameter),
                     FUN=mean, na.rm=F)

colnames(dat.vis) <- c("Species", "site_id", "Mean")


#dat.sd <- aggregate(df$value,
#                   by=list(df$Species, df$Parameter),
#                    FUN=sd, na.rm=F)
#
#colnames(dat.sd) <- c("Species", "site_id", "sd")


#dat.vis$sd <- dat.sd$sd[match(dat.vis$Species, dat.sd$Species)]

dat.vis <- dat.vis[!(dat.vis$site_id == "aPrec" | dat.vis$site_id == "THRESH") ,]

dat.vis$site_id <- as.character(dat.vis$site_id)

#FOr when you pull out something else like precision
#dat.vis <- dat.vis %>% separate(site_id, into = c("site_id", "Parameter"))

#Use min here because there can be repeats that have both species
dat.vis$Latitude <- dat.all$latitude[match(dat.vis$site_id, dat.all$site_id)]
dat.vis$Longitude <- dat.all$longitude[match(dat.vis$site_id, dat.all$site_id)]
#dat.vis$Freq <- size$Freq[match(dat.vis$site_id, size$site_id)]
#dat.vis$error <- qnorm(0.975)*dat.vis$sd/sqrt(dat.vis$Freq)
#dat.vis$left <- dat.vis$Mean-dat.vis$error
#dat.vis$right <- dat.vis$Mean+dat.vis$error
#dat.vis$CIrange <- dat.vis$right - dat.vis$left


#SPATAIL VISUALZATIONS WHICH NEED WORK


map.us <- map_data("state")
png(paste(path.g, "QURU_ACRU_BudBurst_log.png", sep=""), height=6, width=8, units="in", res=180)
ggplot() +
  facet_wrap(~Species)+
  coord_fixed(1.3, xlim=range(dat.vis$Longitude)+c(-1,1)) +
  ggtitle(paste("Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.vis, aes(x=Longitude, y=Latitude, color=log(Mean)), alpha=0.75) +
  scale_color_continuous(type = "viridis") +
  labs(color = "Log GDD5\nThreshold") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

png(paste(path.g, "QURU_ACRU_BudBurst.png", sep=""), height=6, width=8, units="in", res=180)
ggplot() +
  facet_wrap(~Species)+
  # coord_fixed(1.3) +
  coord_fixed(1.3, xlim=range(dat.vis$Longitude)+c(-1,1)) +
  ggtitle(paste("Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.vis, aes(x=Longitude, y=Latitude, color=Mean), alpha=0.75) +
  scale_color_continuous(type = "viridis") +
  labs(color = "GDD5\nThreshold") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#-------------------------------------------------------------------------#
#Here begins the THRESHOLD visualization
#-------------------------------------------------------------------------#
#Making them data frames
quru.stats <- as.data.frame(as.matrix(quru.burn))
acru.stats <- as.data.frame(as.matrix(acru.burn))
summary(acru.stats)
summary(quru.stats)

#Converting back into sd
quru.stats$sd <- 1/sqrt(quru.stats[,"aPrec"])
acru.stats$sd <- 1/sqrt(acru.stats[,"aPrec"])

quru.stats <- quru.stats[,c("THRESH", "aPrec", "sd")]
acru.stats <- acru.stats[,c("THRESH", "aPrec", "sd")]

summary(quru.stats)
summary(acru.stats)

#Matching the THRESH and aPrec rows in the rnorm call here
quru.density <- as.data.frame(apply(as.matrix(quru.stats), 1 , function(x) rnorm(1, mean=as.numeric(x["THRESH"]), sd=as.numeric(x["sd"]))))
acru.density <- as.data.frame(apply(as.matrix(acru.stats), 1 , function(x) rnorm(1, mean=as.numeric(x["THRESH"]), sd=as.numeric(x["sd"]))))

colnames(quru.density) <- c("thresh.pred")
colnames(acru.density) <- c("thresh.pred")

#This is currently just a check. Not currently used in the graphing
quru.ci <- apply(as.matrix(quru.density),2,quantile,c(0.025,0.5,0.975))
acru.ci <- apply(as.matrix(acru.density),2,quantile,c(0.025,0.5,0.975))

quru.ci; acru.ci

#--------------------------
#visualization
quru.density$Species <- 'Quercus rubra'
acru.density$Species <- 'Acer rubrum'


#this is wonky! do not use unless you are Andrew for right now
set.seed <- 072820
rows.keep <- 1:nrow(quru.density)
# rows.keep <- sample(1:nrow(quru.density), 1000)
NPN.stats <- rbind(quru.density[rows.keep,], acru.density[rows.keep,])
write.csv(NPN.stats, "../../data_processed/CAREER_ModelSummary_Thresh.csv", row.names=F)

summary(NPN.stats)


library(ggplot2)
png(width= 750, filename= file.path(path.g, paste0('Thresh_NPN_GDD5', '.png')))
ggplot(data= NPN.stats) +
  ggtitle('Thermal Time Thresholds of Quercus rubra and Acer rubrum') +
  geom_density(mapping = aes(x= thresh.pred, fill = Species, color = Species), alpha=0.5) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (Probability)')
dev.off()
