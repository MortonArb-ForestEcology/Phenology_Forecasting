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
spp.model <- c("Quercus macrocarpa", "Quercus alba") 
# Read in output of previous script
dat.comb <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.comb <- dat.comb[dat.comb$Species %in% spp.model,]
dat.comb$Date <- as.Date(dat.comb$Date)

summary(dat.comb)

#---------------------------------------------------#
#This section sets up the model itself
#---------------------------------------------------#
#rjags for the model and coda for the summary statistics
library(rjags)
library(coda)
#YOU WILL NEED JAGS INSTALLED rjags is a package for interfacting but you need the program itself http://mcmc-jags.sourceforge.net/


#--------------------------------------------------------#
#Everything below here is a work in progress on making the hierarchicial model. WORKS BUT IN PROGRESS FOR QUALiTY CHECKS :)
#--------------------------------------------------------#
#Subsetting out any trees that don't have multiple years of observations
#I'm not sure if those individuals would mess with the model but I need it working without them to check
dat.comb$Accession <- unlist(lapply(strsplit(paste(dat.comb$PlantNumber), "-"), function(x){x[1]}))
dat.comb$PlantNumber <- factor(dat.comb$Accession)
tab <- table(dat.comb$Accession)
dat.comb <- dat.comb[dat.comb$Accession %in% names(tab)[tab>=2],]
summary(dat.comb)

# acc <- strsplit(paste(dat.comb$PlantNumber), "-")
# acc2 <- unlist(lapply(strsplit(paste(dat.comb$PlantNumber), "-"), function(x){x[1]}))
# acc[[1]]
#Currently these priors are uniformative and identical to univariate regression model. Not sure what changes needed if any.
hierarchical_regression <- "
model{
  
  for(k in 1:nObs){
    mu[k] <- THRESH[1] + b[acc[k]]
    
    y[k] ~ dnorm(mu[k], S)
  }
  
  # Priors
  for(i in 1:nAcc){
    b[i] ~ dnorm(b0, v0)
  }
  S ~ dgamma(s1, s2)
  THRESH ~ dnorm(b1, v1)
  
  # S ~ dgamma(s1,s2)    ## prior precision for all individuals
  # 
  # for(i in 1:p) {
  #   ##a[i] ~ dnorm(b0, S) #random individual effect on slope
  #   b[i] ~  dnorm(b1, S) #random individual effect on intercept
  # }
  # for(k in 1:n) {
  #   ##mu[k] <- a[individual[k]] + b[individual[k]] * x[individual[k]] 
  #   mu[k] <- b[individual[k]]
  #   y[k] ~ dnorm(mu[k], S)
  # }
}
"

burst.list <- list(y = dat.comb$GDD5.cum, acc = as.numeric(factor(dat.comb$Accession)), nAcc = length(unique(dat.comb$Accession)), nObs = length(dat.comb$GDD5.cum))

#Setting our uniformative priors
burst.list$b0 <- 0
burst.list$v0 <- 0.0001
burst.list$b1 <- 0
burst.list$v1 <- .0001
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b=rnorm(burst.list$nAcc,0,5),
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
                             variable.names = c("THRESH", "b","S"),
                             n.iter = 5000)



#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
plot(burst.out)


#Checking that convergence happened
gelman.diag(burst.out)

#Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 1000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)                             ## check diagnostics post burn-in

#Checking autocorrelation
acfplot(burst.burn)

#Checking effective size
effectiveSize(burst.burn)

summary(burst.burn)

burst.df2 <- as.data.frame(as.matrix(burst.burn))
summary(burst.df2)
