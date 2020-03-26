#Script for the Bayesian model based off of the Frequentist_Gdd5-burst.R script

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


#running the model
burst.model   <- jags.model (file = textConnection(univariate_regression),
                            data = burst.list,
                            inits = inits,
                            n.chains = 3)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                            variable.names = c("b","S"),
                            n.iter = 5000)


plot(burst.out)


#Checking that convergence happened
gelman.diag(burst.out)

#Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 500                                ## determine convergence from TBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)                             ## check diagnostics post burn-in


#Checking autocorrelation
acfplot(burst.burn)

#Checking effective size
effectiveSize(burst.burn)


summary(burst.burn)

#Matrix for visualizing
burst.mat <- as.matrix(burst.burn)


