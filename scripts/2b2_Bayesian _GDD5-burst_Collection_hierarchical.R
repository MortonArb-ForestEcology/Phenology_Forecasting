#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Bayesian model which will become the final product
# Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script
# Outputs: Currently, a hindcast of a species modeled day of budburst vs observed date of budburst
# Notes: This script is based on exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#
library(rjags)
library(coda)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)
dat.all$Collection <- as.factor(ifelse(substr(dat.all$Species, 1, 2)=="Qu", "Quercus", "Acer"))
summary(dat.all)

# spp.model <- c("Quercus macrocarpa", "Quercus alba") 
for(COL in unique(dat.all$Collection)){
  dat.comb <- dat.all[dat.all$Collection==COL,]
  
  # summary(dat.comb)
  
  #---------------------------------------------------#
  #This section sets up the model itself
  #---------------------------------------------------#
  #rjags for the model and coda for the summary statistics
  #YOU WILL NEED JAGS INSTALLED rjags is a package for interfacting but you need the program itself http://mcmc-jags.sourceforge.net/

  #Currently these priors are uniformative and identical to univariate regression model. Not sure what changes needed if any.
  hierarchical_regression <- "
  model{
    
    
    
    for(k in 1:nObs){
      # Accession effect
      # EfAcc[k] <- ba[acc[k]]
      EfAcc[k] <- dnorm(ba[acc[k]], tau_acc)
      
      # Species Effect with added error
      EfSpp[k] <- dnorm(EfAcc[k], tau_spp)
      mu[k] <- THRESH[1] + EfSpp[k]
      
      y[k] ~ dnorm(mu[k], S)
    }
    
    # Priors
    for(i in 1:nAcc){
      ba[i] ~ dnorm(b0, v0)
    }
    S ~ dgamma(s1, s2)
    THRESH ~ dnorm(b2, v2)
  }
  "
  
  burst.list <- list(y = dat.comb$GDD5.cum, 
                     acc = as.numeric(factor(dat.comb$Accession)), 
                     spp = as.numeric(factor(dat.comb$Species)),
                     nAcc = length(unique(dat.comb$Accession)),
                     nSpp=length(unique(dat.comb$Species)), 
                     nObs = length(dat.comb$GDD5.cum))
  
  #Setting our uniformative priors
  burst.list$b0 <- 0
  burst.list$v0 <- 0.0001
  burst.list$b1 <- 0
  burst.list$v1 <- .0001
  burst.list$b2 <- 0
  burst.list$v2 <- 0.0001
  burst.list$b1 <- 0
  burst.list$v1 <- .0001
  burst.list$s1 <- 0.1                    ## error prior n/2
  burst.list$s2 <- 0.1                    ## error prior SS/2
  
  
  #Setting the number of MCMC chains and their parameters
  nchain = 3
  inits <- list()
  for(i in 1:nchain){
    inits[[i]] <- list(ba=rnorm(burst.list$nAcc,0,5),
                       bs=rnorm(burst.list$nSpp,0,5),
                       THRESH=rnorm(1, 0, 5),
                       S = runif(1,1/200,1/20))
  }
  
  #---------------------------------------------------------#
  #This section actually runs the model and then provides ways to check the output and clean it
  #---------------------------------------------------------#
  #running the model
  burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                               data = burst.list,
                               inits = inits,
                               n.chains = 3)
  
  
  #Converting the ooutput into a workable format
  burst.out   <- coda.samples (model = burst.model,
                               variable.names = c("THRESH", "EfSpp", "S"),
                               n.iter = 5000)
  
  # #Trace plot and distribution. For trace make sure they are very overlapped showing convergence
  # plot(burst.out)
  # 
  # #Checking that convergence happened
  # gelman.diag(burst.out)
  # 
  # #Checking where convergence occured
  # GBR <- gelman.plot(burst.out)
  
  #Removing burnin before convergence occurred
  burnin = 1000                                ## determine convergence from GBR output
  burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
  plot(burst.burn)                             ## check diagnostics post burn-in
  
  # #Checking autocorrelation
  # acfplot(burst.burn)
  # 
  # #Checking effective size
  # effectiveSize(burst.burn)
  
  summary(burst.burn)
  
  burst.df2 <- as.data.frame(as.matrix(burst.burn))
  summary(burst.df2)
}
