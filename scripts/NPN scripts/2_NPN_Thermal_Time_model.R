#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)
library(ggmcmc)
library(dplyr)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Full_Phenology_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)

for(Name in unique(dat.all$site_name)){
  dat.tmp <- dat.all[dat.all$site_name == Name,]
  Num_sp <- length(unique(dat.tmp$species))
  dat.all[dat.all$site_name == Name, "Num_sp"] <- Num_sp
}


trees <- as.data.frame(table(dat.all$species))

#These two lines aren't really neccessary but helps me keep track of what I'm working with
species <- c("alba")
dat.comb <- dat.all[dat.all$species %in% species, ]


Obs <- as.data.frame(table(dat.comb$site_name))
colnames(Obs) <- c("Site", "Freq")
size <- Obs[Obs$Freq > 0, ]

dat.comb <- dat.comb[dat.comb$site_name %in% size$Site,]
dat.comb <- dat.comb[dat.comb$site_name %in% dat.MODIS$site_name,] 

ind <- aggregate(site_name~individual_id, data=dat.comb,
                     FUN=min)


hierarchical_regression <- "
  model{
  
    for(k in 1:nObs){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
      
    for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(0, aPrec)
    }

    for(t in 1:nLoc){
      Site[t] <-  THRESH[sp[t]] + b[t]
      b[t] ~ dnorm(0, bPrec[t])
      bPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  Site[loc[i]] + c[i]
        c[i] ~ dnorm(0, cPrec)
    }
    
    sPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
  }
"
burst.list <- list(y = dat.comb$GDD5.cum, nObs = length(dat.comb$GDD5.cum),
                   loc = as.numeric(factor(ind$site_name)), nLoc = length(unique(dat.comb$site_name)),
                   pln = as.numeric(factor(dat.comb$individual_id)), nPln = length(unique(dat.comb$individual_id)),
                   sp = as.numeric(factor(dat.comb$species)), nSp = length(unique(dat.comb$species)))



#Setting the number of MCMC chains and their parameters
nchain = 5
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(sPrec = runif(1,1/200,30))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = nchain)

green.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = green.list,
                             inits = inits,
                             n.chains = nchain)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("Site"),
                             n.iter = 300000)

#Converting the ooutput into a workable format
green.out   <- coda.samples (model = green.model,
                             variable.names = c("Site"),
                             n.iter = 300000)


#Renaming parameters to properly match their effects (e.g. sites are renamed to their Site, species to their species)
varnames(burst.out) <- c(paste(as.character(size$Site)))
varnames(green.out) <- c(paste(as.character(size$Site)))

# #Checking that convergence happened
gelman.diag(burst.out)
gelman.diag(green.out)


#Removing burnin before convergence occurred
burnin = 290000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)## remove burn-in
green.burn <- window(green.out,start=burnin)

summary(green.burn)

#Paths for figures
path.g <- "G:/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/Site_effects/"
#path.log <- "G:/My Drive/LivingCollections_Phenology/Phenology Forecasting/figures/Site_effects/Using Log Model/"

#Diagnostic Checks#
#ggs_density(df, family = "ind[9]")
#ggs_caterpillar(df, family =  "effect")
#ggs_histogram(df, family = "5754_effect")
#ggs_traceplot(df, family = "27305")


#Creating a dataframe useful for a density plot visual
burst.df <- as.data.frame(as.matrix(burst.burn))
green.df <- as.data.frame(as.matrix(green.burn))


burst.density <- reshape::melt(burst.df)
colnames(burst.density) <- c("Species", "Mean")
green.density <- reshape::melt(green.df)
colnames(green.density) <- c("Site", "Mean")

burst.density$Type <- 'NPN'
green.density$Type <- 'MODIS'

spec <- c("Quercus alba", "Quercus acutissima", "Quercus macrocarpa", "Quercus saulii", "Quercus pyrenaica", "Quercus variabilis")

density.all <- burst.density[burst.density$Species %in% spec,]
#FOr when you pull out something else like precision
#burst.density <- burst.density %>% separate(variable, into = c("Site", "Parameter"))


png(paste(path.g, species, "Oak_Collection_Density.png", sep=""), height=6, width=8, units="in", res=180)
ggplot(data= density.all) +
  #facet_wrap(~Type) + # breaks things into separate panels
  ggtitle(paste("Comparison of Growing Degree Day Thresholds of different Quercus in our Oak Colletion ",sep="")) +
  geom_density(mapping = aes(x= Mean, color = Species, fill=Species), alpha=0.5) +
  scale_x_continuous('THRESH (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
#theme(legend.position='none')
dev.off()

#Creating a dataframe useful for a spatial map

df <- ggs(burst.burn)
df.b$Type <- 'NPN'

df.g <- ggs(green.burn)
df.g$Type <- 'MODIS'

df <- rbind(df.b, df.g)
dat.vis <- aggregate(df$value,
                     by=list(df$Parameter),
                     FUN=mean, na.rm=F)

colnames(dat.vis) <- c("Species", "Mean")


dat.sd <- aggregate(df$value,
                    by=list(df$Parameter),
                    FUN=sd, na.rm=F)

colnames(dat.sd) <- c("Species", "sd")


dat.vis$sd <- dat.sd$sd[match(dat.vis$Species, dat.sd$Species)]

dat.vis$Site <- as.character(dat.vis$Site)

#FOr when you pull out something else like precision
#dat.vis <- dat.vis %>% separate(Site, into = c("Site", "Parameter"))

dat.vis$Latitude <- dat.comb$latitude[match(dat.vis$Site, dat.comb$site_name)]
dat.vis$Longitude <- dat.comb$longitude[match(dat.vis$Site, dat.comb$site_name)]
dat.vis$Freq <- size$Freq[match(dat.vis$Site, size$Site)]
dat.vis$error <- qnorm(0.975)*dat.vis$sd/sqrt(dat.vis$Freq)
dat.vis$left <- dat.vis$Mean-dat.vis$error
dat.vis$right <- dat.vis$Mean+dat.vis$error
dat.vis$CIrange <- dat.vis$right - dat.vis$left


#SPATAIL VISUALZATIONS WHICH NEED WORK

map.us <- map_data("state")
png(paste(path.g, species, "_BudBurst.png", sep=""), height=6, width=8, units="in", res=180)
ggplot() +
  coord_fixed(1.3) +
  ggtitle(paste("Quercus", species, "Breaking Leaf Buds", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.vis, aes(x=Longitude, y=Latitude, size=Freq, color=log(Mean)), alpha=0.75) +
  facet_wrap(~Type)+
  scale_color_continuous(type = "viridis") +
  scale_size_continuous()+
  labs(color = "Log of GDD5.cum THreshold",size="Number of Observations") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()


ggs_caterpillar(df)+
  facet_wrap(~Type)

size$Latitude <- dat.all$latitude[match(size$Site, dat.all$site_name)]
size$Longitude <- dat.all$longitude[match(size$Site, dat.all$site_name)]
size$Num_sp <- dat.all$Num_sp[match(size$Site, dat.all$site_name)]

map.us <- map_data("state")
png(paste(path.g, "Quercus_NPN_Sites_BudBurst.png", sep=""), height=6, width=8, units="in", res=180)
ggplot() +
  coord_fixed(1.3) +
  ggtitle(paste("Quercus Budburst Observation map", sep=" ")) +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=size, aes(x=Longitude, y=Latitude, size=Num_sp, color=Freq)) +
  scale_color_continuous(type = "viridis") +
  scale_size_continuous()+
  labs(color = "Number of unique budburst observations",size="Number of unique species at site") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()



write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "NPN_Oaks_log"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "NPN_Oaks_log"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  print(hist(dat.comb$Yday))
  print(hist(dat.comb$GDD5.cum))
  dev.off()
  dev.off()
} 

