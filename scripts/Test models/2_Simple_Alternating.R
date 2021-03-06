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
library(dplyr)
library(tidyr)

# Read in output of previous script
dat.b <- read.csv("../data_processed/Oak_collection_budburst.csv")
dat.b$Accession <- unlist(lapply(strsplit(paste(dat.b$PlantNumber), "-"), function(x){x[1]}))
dat.b$Date <- as.Date(dat.b$Date)

# Read in output of previous script
dat.l <- read.csv("../data_processed/Oak_collection_leaf.csv")
dat.l$Accession <- unlist(lapply(strsplit(paste(dat.l$PlantNumber), "-"), function(x){x[1]}))
dat.l$Date <- as.Date(dat.l$Date)

#Pulling out the number of observations we have for each species
SP.burst <- as.data.frame(table(dat.b[, "Species"]))
colnames(SP.burst) <- c("Species", "Freq")

#Pulling out the number of observations we have for each species
SP.leaf <- as.data.frame(table(dat.l[, "Species"]))
colnames(SP.leaf) <- c("Species", "Freq")

#CHecking which species we don't have budburst information for
setdiff(SP.leaf$Species, SP.burst$Species)

#Creating a data frame to check the model output
Check <- data.frame()
l <- 1

for(SP in SP.burst$Species){
  
  #Creating the row for this species information
  Check[l, "species"] <- SP
  Check[l, "burst.nObs"] <- SP.burst[SP.burst$Species == SP, "Freq"]
  Check[l, "leaf.nObs"] <- SP.leaf[SP.leaf$Species == SP, "Freq"]
  
  dat.burst <- dat.b[dat.b$Species == SP, ]
  dat.leaf <- dat.l[dat.l$Species == SP, ]
  
  #Checking the mean
  Check[l, "mean.burst"] <- mean(dat.burst$GDD5.cum)
  Check[l, "mean.leaf"] <- mean(dat.leaf$GDD5.cum)
  
  #Creating indexes so the hierarchy can properly function
  burst.ind <- aggregate(Accession~PlantNumber, data=dat.burst,
                         FUN=min)
  
  burst.acc <- aggregate(Species~Accession, data=dat.burst,
                         FUN=min)
  
  leaf.ind <- aggregate(Accession~PlantNumber, data=dat.leaf,
                        FUN=min)
  
  leaf.acc <- aggregate(Species~Accession, data=dat.leaf,
                        FUN=min)
  
  
  Alternating_model <- "
  model{

  b ~ dunif(0, 5000)
  c ~ dunif(-1, 0)
  S ~ dgamma(0.1,0.1)    ## prior precision

  for(i in 1:n){
	  mu[i] <- ind[pln[i]] + THRESH +  b*exp(c * NCD[i])   	## process model
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
  
  for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(Tprior, tPrec[j])
      tPrec[j] ~ dgamma(0.2, 0.1)
      Tprior[j] ~ dunif(1, 600)
    }
    
  for(t in 1:nAcc){
      Accession[t] <-  THRESH[sp[t]] + e[t]
      e[t] ~ dnorm(0, ePrec[t])
      ePrec[t] ~ dgamma(0.1, 0.1)
  }
    
  for(i in 1:nPln){
      ind[i] <-  Accession[acc[i]] + d[i]
      d[i] ~ dnorm(0, dPrec)
  }
    
    pPrec ~ dgamma(0.1, 0.1)
    dPrec ~ dgamma(0.1, 0.1)
    
}
"
  
  burst.list <- list(y = dat.burst$GDD5.cum, n = length(dat.burst$GDD5.cum), NCD = dat.burst$NCD,
                     pln = as.numeric(factor(dat.burst$PlantNumber)), nPln = length(unique(dat.burst$PlantNumber)),
                     acc = as.numeric(factor(burst.ind$Accession)), nAcc = length(unique(dat.burst$Accession)),
                     sp = as.numeric(factor(burst.acc$Species)), nSp = length(unique(dat.burst$Species)))
  
  leaf.list <- list(y = dat.leaf$GDD5.cum, n = length(dat.leaf$GDD5.cum), NCD = dat.leaf$NCD,
                    pln = as.numeric(factor(dat.leaf$PlantNumber)), nPln = length(unique(dat.leaf$PlantNumber)),
                    acc = as.numeric(factor(leaf.ind$Accession)), nAcc = length(unique(dat.leaf$Accession)),
                    sp = as.numeric(factor(leaf.acc$Species)), nSp = length(unique(dat.leaf$Species)))
  
  
  #Setting the number of MCMC chains and their parameters
  nchain = 3
  inits <- list()
  for(i in 1:nchain){
    inits[[i]] <- list()
  }
  
  #---------------------------------------------------------#
  #This section actually runs the model and then provides ways to check the output and clean it
  #---------------------------------------------------------#
  burst.model   <- jags.model (file = textConnection(Alternating_model),
                               data = burst.list,
                               n.chains = 3)
  
  leaf.model   <- jags.model (file = textConnection(Alternating_model),
                              data = leaf.list,
                              n.chains = 3)
  
  
  #Converting the ooutput into a workable format
  burst.out   <- coda.samples (model = burst.model,
                               variable.names = c("THRESH", "S", "b"),
                               n.iter = 700000)
  
  #Converting the ooutput into a workable format
  leaf.out   <- coda.samples (model = leaf.model,
                              variable.names = c("THRESH", "aPrec"),
                              n.iter = 700000)
  gelman.diag(burst.out)
  
  # #Checking that convergence happened
  bud.con <- gelman.diag(burst.out[,"THRESH"])
  Check[l, "burst.converge"] <- bud.con[[1]][1]
  leaf.con <- gelman.diag(leaf.out[,"THRESH"])
  Check[l, "leaf.converge"] <- leaf.con[[1]][1]
  
  #Removing burnin before convergence occurred
  burnin = 690000                                ## determine convergence from GBR output
  burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
  summary(burst.burn)
  
  leaf.burn <- window(leaf.out, start = burnin)
  summary(leaf.burn)
  
  #converting them into a dataframe 
  burst.df <- as.data.frame(as.matrix(burst.burn))
  leaf.df <- as.data.frame(as.matrix(leaf.burn))
  
  #calculating sd from the precison
  burst.df$sd <- 1/sqrt(burst.df[,"aPrec"])
  summary(burst.df)
  leaf.df$sd <- 1/sqrt(leaf.df[,"aPrec"])
  summary(leaf.df)
  
  
  bud.density <- as.data.frame(apply(as.matrix(burst.df), 1 , function(x) rnorm(1, mean=x[1], sd=x[3])))
  burst.df$species <- SP
  bud.ci <- apply(as.matrix(bud.density),2,quantile,c(0.025,0.5,0.975))
  
  leaf.density <- as.data.frame(apply(as.matrix(leaf.df), 1 , function(x) rnorm(1, mean=x[1], sd=x[3])))
  leaf.df$species <- SP
  leaf.ci <- apply(as.matrix(leaf.density),2,quantile,c(0.025,0.5,0.975))
  
  Check[l, "burst 2.5%"] <- bud.ci[1]
  Check[l, "burst 50%"] <- bud.ci[2]
  Check[l, "burst 97.5%"] <- bud.ci[3]
  
  Check[l, "leaf 2.5%"] <- leaf.ci[1]
  Check[l, "leaf 50%"] <- leaf.ci[2]
  Check[l, "leaf 97.5%"] <- leaf.ci[3]
  
  write.csv(burst.df, file.path("../data_processed/Arb_Pheno", paste0(SP, "_model_budburst.csv")), row.names=F)
  write.csv(leaf.df, file.path("../data_processed/Arb_Pheno", paste0(SP, "_model_leaf.csv")), row.names=F)
  
  l <- l + 1
}


burst.list <- list(NCD = dat.comb$NCD, y = dat.comb$GDD5.cum, sp = as.numeric(factor(dat.comb$Species)),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$GDD5.cum))

burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(S = runif(1,1/4000,1/20),
                     b = runif(1, 0, 3000),
                     c = runif(1, -1, 0))
}

burst.model   <- jags.model (file = textConnection(Alternating_model),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("THRESH", "ind", "b", "c", "S"),
                             n.iter = 100000)



gelman.diag(burst.out)


# #Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)

burst.df2 <- as.data.frame(as.matrix(burst.burn))

write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  print(hist(dat.comb$Yday))
  print(hist(dat.comb$GDD5.cum))
  dev.off()
  dev.off()
} else {
  png(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".png")), height=8, width=8, units="in", res=240)
  print(plot(burst.burn))                             ## check diagnostics post burn-in
  dev.off()
  dev.off()
  
}