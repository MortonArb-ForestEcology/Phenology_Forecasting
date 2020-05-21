#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Bayesian linear model which will become the final product
# Inputs: dat.comb dataframe that is created by the Organize_Pheno_Data.R script
# Outputs: A linear model of budburst timing
# Notes: This script is based on exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#
library(rjags)
library(coda)

dat.all <- read.csv("../data_processed/Phenology_NPN_combined.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)

dat.comb <- dat.all


Linear_NPN <- "
model{

  for(i in 1:nObs){
    a[i] <- Ex[loc[i]] + THRESH[sp[i]]
	  mu[i] <- a[i] + b*x[i]   	## process model
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
  
  for(k in 1:nObs){
      Ynew[k]  ~ dnorm(munew[k], S)
      munew[k] <- a[k] + b*x[k]
  }
    
 # Priors
    for(j in 1:nSp){                    
    THRESH[j] ~ dnorm(0, tPrec)
    }
    
    for(t in 1:nLoc){
    Ex[t] <- ind[pln[t]] + c[t]
    c[t] ~ dnorm(0, cPrec)
    }
    
    for(i in 1:nPln){
        ind[i] <-  p[i]
        p[i] ~ dnorm(0, pPrec)
    }
    
    b ~ dnorm(0, .001)
    tPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
    pPrec ~ dgamma(0.1, 0.1)
    S ~ dgamma(s1, s2)
    
    d[1] <- max(Ynew[])
    d[2] <- min(Ynew[])
    d[3] <- max(Ynew[])-min(Ynew[])
    d[4] <- mean(Ynew[])
    d[5] <- sd(Ynew[])
}
"

burst.list <- list(y = dat.comb$Yday, x = dat.comb$GTmean, sp = as.numeric(factor(dat.comb$Species)),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   loc = as.numeric(factor(dat.comb$Location)), nLoc = length(unique(dat.comb$Location)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$Yday))
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b = rnorm(1,0,5),
                     THRESH=rnorm(length(unique(dat.comb$Species)), 0, 5),
                     S = runif(1,1/200,1/20))
}

burst.model   <- jags.model (file = textConnection(Linear_NPN),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("b", "THRESH"),
                             n.iter = 10000)



gelman.diag(burst.out)


# #Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 5000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)

burst.df2 <- as.data.frame(as.matrix(burst.burn))

write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "NPN_linear_"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "NPN_linear"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  print(hist(dat.comb$Yday))
  dev.off()
  dev.off()
} else {
  png(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "NPN_linear"), ".png")), height=8, width=8, units="in", res=240)
  print(plot(burst.burn))                             ## check diagnostics post burn-in
  dev.off()
  dev.off()
  
}