#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)
library(ggmcmc)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Full_Phenology_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)


#These two lines aren't really neccessary but helps me keep track of what I'm working with
species <- c("Quercus rubra")
dat.comb <- dat.all[dat.all$Species %in% species, ]

dat.comb <- dat.comb[dat.comb$GDD5.cum > 0 & dat.comb$Yday < 200, ]

Obs <- as.data.frame(table(dat.comb$Site))
colnames(Obs) <- c("Site", "Freq")
size <- Obs[Obs$Freq > 5, ]

dat.comb <- dat.comb[dat.comb$Site %in% size$Site,]


sum((dat.comb$GDD5.cum - mean(dat.comb$GDD5.cum))^2)

hierarchical_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Ex[loc[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(k in 1:nObs){
      Ynew[k]  ~ dnorm(munew[k], sPrec)
      munew[k] <- Ex[loc[k]]
    }
    
    # Priors
    #for(j in 1:nSp){                      #This loop adds the species effect on Threshold
    #Species[j] ~ dnorm(0, tPrec)
    #}
    
    for(t in 1:nLoc){
    Ex[t] <-  c[t] + ind[pln[t]]
    c[t] ~ dnorm(0, aPrec[t])
    aPrec[t] ~ dgamma(0.1, 0.01)
    }
    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    #tPrec ~ dgamma(1, 0.01)
    
    bPrec ~ dgamma(0.1, 0.1)
    sPrec ~ dgamma(0.1, 0.1)
    #THRESH ~ dnorm(0, .001)
    
    d[1] <- max(Ynew[])
    d[2] <- min(Ynew[])
    d[3] <- max(Ynew[])-min(Ynew[])
    d[4] <- mean(Ynew[])
    d[5] <- sd(Ynew[])
  }
  "

burst.list <- list(y = dat.comb$GDD5.cum, sp = as.numeric(factor(dat.comb$Species)),
                   loc = as.numeric(factor(dat.comb$Site)), nLoc = length(unique(dat.comb$Site)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$GDD5.cum),
                    pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)))


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


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("Ex", "aPrec"),
                             n.iter = 100000)

DIC2 <- dic.samples(burst.model, 50000)

# #Checking that convergence happened
gelman.diag(burst.out)
# 
# #Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)


#Renaming parameters to properly match their effects (e.g. sites are renamed to their Site, species to their species)
varnames(burst.burn) <- c(paste(as.character(size$Site), "_Site", sep=""), paste(as.character(size$Site), "_Prec", sep=""))

df <- ggs(burst.burn)

ggs_density(df, family = "26202")

ggs_caterpillar(df, family =  "Site")


ggs_histogram(df)

ggs_traceplot(df)

ggs_running(df)

ggs_compare_partial(df)

ggs_autocorrelation(df)




burst.df2 <- as.data.frame(as.matrix(burst.burn))
colnames(burst.df2) <- c(as.character(unique(dat.comb$Species)))

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

