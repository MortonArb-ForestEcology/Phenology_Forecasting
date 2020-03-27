#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Bayesian model which will become the finla product
# Inputs: dat.comb dataframe that is created by the Frequentist_GDD%-burst.r script in this repository
# Outputs: Currently, a posterior distribtuion of yday as a funciton of Gdd5.cum
# Notes: This script is basedon exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------#
#This section sets up the model itself
#---------------------------------------------------#
#rjags for the model and coda for the summary statistics
#YOU WILL NEED JAGS INSTALLED rjags is a package for interfacting but you need the program itself http://mcmc-jags.sourceforge.net/
library(rjags)
library(coda)
#Setting up the Jags model itself

univariate_regression <- "
model{

  b ~ dmnorm(b0,Vb)  	## multivariate Normal prior on vector of regression params
  S ~ dgamma(s1,s2)    ## prior precision

  for(i in 1:n){
	  mu[i] <- b[1] + b[2]*x[i]   	## process model (simple linear regression)
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
}
"

#Checking how good of a predictor they currently seem
plot(dat.comb$GDD5.cum, dat.comb$Yday)

#------------------------------------------------------#
#This section converts our observed into the neccessary format, defines our uninformed prior, and sets up our MCMC chains
#------------------------------------------------------#

#Converting to list format needed for JAGs
burst.list <- list(x = dat.comb$GDD5.cum, y = dat.comb$Yday, n = length(dat.comb$Yday))

#Setting our uniformative priors
burst.list$b0 <- as.vector(c(0,0))      ## regression b means
burst.list$Vb <- solve(diag(10000,2))   ## regression b precisions
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b = rnorm(2,0,5), S = runif(1,1/200,1/20))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
#running the model
burst.model   <- jags.model (file = textConnection(univariate_regression),
                            data = burst.list,
                            inits = inits,
                            n.chains = 3)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                            variable.names = c("b","S"),
                            n.iter = 5000)


#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
plot(burst.out)


#Checking that convergence happened
gelman.diag(burst.out)

#Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 4000                                ## determine convergence from TBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)                             ## check diagnostics post burn-in


#Checking autocorrelation
acfplot(burst.burn)

#Checking effective size
effectiveSize(burst.burn)

summary(burst.burn)

#----------------------------------------------------------------------------#
#This section is for taking the model output and visualizing it
#----------------------------------------------------------------------------#

#Matrix for visualizing
burst.mat <- as.matrix(burst.burn)


