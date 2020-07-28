library(rjags)
library(coda)
library(ggmcmc)
library(dplyr)

# Read in output of previous script
dat.all <- read.csv("../data_processed/QURU_ACRU_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)

summary(dat.all)
dat.all <- dat.all[dat.all$GDD5.cum > 0,]


dat.quru <- dat.all[dat.all$species == "rubra", ]
dat.acru <- dat.all[dat.all$species == "rubrum", ]

#--------------------------------------------#
#Old but I haven't redownloaded to check it's not needed just yet

#Pulling out unique sites
#quru.sites <- as.data.frame(table(dat.quru$site_name))
#colnames(quru.sites) <- c("Site", "Freq")

#acru.sites <- as.data.frame(table(dat.acru$site_name))
#colnames(acru.sites) <- c("Site", "Freq")

#Making sure they only use matching sites
#dat.quru <- dat.quru[dat.quru$site_name %in% acru.sites$Site,]
#dat.acru <- dat.acru[dat.acru$site_name %in% quru.sites$Site,]

summary(dat.quru)
summary(dat.acru)

#Pulled out for matching the Site names to the JAGS output
quru.match <- as.data.frame(table(dat.quru$site_name))
colnames(quru.match) <- c("Site", "Freq")

acru.match <- as.data.frame(table(dat.acru$site_name))
colnames(acru.match) <- c("Site", "Freq")

#Pulling site locations for each individual to help hierarchy indexing
quru.ind <- aggregate(site_name~individual_id, data=dat.quru,
                 FUN=min)

acru.ind <- aggregate(site_name~individual_id, data=dat.acru,
                      FUN=min)


hierarchical_regression <- "
  model{
      
    for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(140, aPrec[j])
      aPrec[j] ~ dgamma(1, 0.1)
    }

    for(t in 1:nLoc){
      Site[t] <-  THRESH[sp[t]] + b[t]
      b[t] ~ dnorm(0, bPrec[t])
      bPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  Site[loc[i]] * c[i]
        c[i] ~ dnorm(1, cPrec)
    }
    
    for(k in 1:n){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
    
    sPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
  }
"
quru.list <- list(y = dat.quru$GDD5.cum, n = length(dat.quru$GDD5.cum),
                   loc = as.numeric(factor(quru.ind$site_name)), nLoc = length(unique(dat.quru$site_name)),
                   pln = as.numeric(factor(dat.quru$individual_id)), nPln = length(unique(dat.quru$individual_id)),
                   sp = as.numeric(factor(dat.quru$species)), nSp = length(unique(dat.quru$species)))

acru.list <- list(y = dat.acru$GDD5.cum, n = length(dat.acru$GDD5.cum),
                  loc = as.numeric(factor(acru.ind$site_name)), nLoc = length(unique(dat.acru$site_name)),
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
                             variable.names = c("Site", "THRESH", "aPrec"),
                             n.iter = 300000)

#Converting the output into a workable format
acru.out   <- coda.samples (model = acru.model,
                            variable.names = c("Site", "THRESH", "aPrec"),
                            n.iter = 300000)


#Renaming parameters to properly match their effects (e.g. sites are renamed to their Site, species to their species)
varnames(quru.out) <- c(paste(as.character(quru.match$Site)), "THRESH", "aPrec")
varnames(acru.out) <- c(paste(as.character(acru.match$Site)), "THRESH", "aPrec")

# #Checking that convergence happened
gelman.diag(quru.out)
gelman.diag(acru.out)


#Removing burnin before convergence occurred
burnin = 150000                                ## determine convergence from GBR output
quru.burn <- window(quru.out,start=burnin)## remove burn-in
acru.burn <- window(acru.out,start=burnin)

#-------------------------------------------------------------------------------#
#Here begins the NPN map visualisation#
#-------------------------------------------------------------------------------#
df.quru <- ggs(quru.burn)
df.acru <- ggs(acru.burn)
df.quru$Species <- 'Quercus rubra'
df.acru$Species <- 'Acer rubrum'

df <- rbind(df.quru, df.acru)
dat.vis <- aggregate(df$value,
                     by=list(df$Species, df$Parameter),
                     FUN=mean, na.rm=F)

colnames(dat.vis) <- c("Species", "Site", "Mean")


#dat.sd <- aggregate(df$value,
#                   by=list(df$Species, df$Parameter),
#                    FUN=sd, na.rm=F)
#
#colnames(dat.sd) <- c("Species", "Site", "sd")


#dat.vis$sd <- dat.sd$sd[match(dat.vis$Species, dat.sd$Species)]

dat.vis <- dat.vis[!(dat.vis$Site == "aPrec" | dat.vis$Site == "THRESH") ,]

dat.vis$Site <- as.character(dat.vis$Site)

#FOr when you pull out something else like precision
#dat.vis <- dat.vis %>% separate(Site, into = c("Site", "Parameter"))

#Use min here because there can be repeats that have both species
dat.vis$Latitude <- dat.all$latitude[match(dat.vis$Site, dat.all$site_name)]
dat.vis$Longitude <- dat.all$longitude[match(dat.vis$Site, dat.all$site_name)]
#dat.vis$Freq <- size$Freq[match(dat.vis$Site, size$Site)]
#dat.vis$error <- qnorm(0.975)*dat.vis$sd/sqrt(dat.vis$Freq)
#dat.vis$left <- dat.vis$Mean-dat.vis$error
#dat.vis$right <- dat.vis$Mean+dat.vis$error
#dat.vis$CIrange <- dat.vis$right - dat.vis$left


#SPATAIL VISUALZATIONS WHICH NEED WORK

path.g <- "G:/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/For NSF career grant/"

map.us <- map_data("state")
png(paste(path.g, "QURU_ACRU_BudBurst.png", sep=""), height=6, width=8, units="in", res=180)
ggplot() +
  facet_wrap(~Species)+
  coord_fixed(1.3) +
  ggtitle(paste("Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.vis, aes(x=Longitude, y=Latitude, color=log(Mean)), alpha=0.75) +
  scale_color_continuous(type = "viridis") +
  labs(color = "Log of GDD5.cum THreshold") +
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

#Converting back into sd
quru.stats$sd <- 1/sqrt(quru.stats[,"aPrec"])
acru.stats$sd <- 1/sqrt(acru.stats[,"aPrec"])

quru.stats <- quru.stats[,c("THRESH", "aPrec", "sd")]
acru.stats <- acru.stats[,c("THRESH", "aPrec", "sd")]

summary(quru.stats)
summary(acru.stats)

#Matching the THRESH and aPrec rows in the rnorm call here
quru.density <- as.data.frame(apply(as.matrix(quru.stats), 1 , function(x) rnorm(1, mean=x[1], sd=x[3])))
acru.density <- as.data.frame(apply(as.matrix(acru.stats), 1 , function(x) rnorm(1, mean=x[1], sd=x[3])))

colnames(quru.density) <- c("THRESH")
colnames(acru.density) <- c("THRESH")

#This is currently just a check. Not currently used in the graphing
quru.ci <- apply(as.matrix(quru.density),2,quantile,c(0.025,0.5,0.975))
acru.ci <- apply(as.matrix(acru.density),2,quantile,c(0.025,0.5,0.975))

quru.ci; acru.ci

#--------------------------
#visualization
quru.density$Species <- 'Quercus rubra'
acru.density$Species <- 'Acer rubrum'


#this is wonky! do not use unless you are Andrew for right now
NPN.stats <- rbind(quru.density, acru.density)

summary(NPN.stats)


library(ggplot2)
path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Thresh_NPN_GDD5', '.png')))
ggplot(data= NPN.stats) +
  ggtitle('Thermal Time Thresholds of Quercus rubra and Acer rubrum') +
  geom_density(mapping = aes(x= THRESH, fill = Species, color = Species), alpha=0.5) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (Probability)')
dev.off()
