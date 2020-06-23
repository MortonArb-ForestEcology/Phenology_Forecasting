#Setting up the Jags model itself

univariate_regression <- "
model{



  for(i in 1:n){
	  mu[i] <- b[1] + b[2]*x[i]   	## process model (simple linear regression)
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
  
  for(j in 1:2){
    b[j] ~ dnorm(0,0.0001)
  }
  
  S ~ dgamma(s1,s2)    ## prior precision 
}
"

#Checking how good of a predictor they currently seem
plot(dat.comb$GDD5.cum, dat.comb$Yday)

#------------------------------------------------------#
#This section converts our observed into the neccessary format, defines our uninformed prior, and sets up our MCMC chains
#------------------------------------------------------#

#Converting to list format needed for JAGs
burst.list <- list(y = dat.comb$GDD5.cum, n = length(dat.comb$GDD5.cum), x = dat.comb$Yday)

#Setting our uniformative priors
burst.list$s1 <- .1                    ## error prior n/2
burst.list$s2 <- .1                    ## error prior SS/2


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b = rnorm(2,0,5), S = runif(1,1/200,1/20))
}

burst.model   <- jags.model (file = textConnection(univariate_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                            variable.names = c("b","S", "y"),
                            n.iter = 100000)

# #Trace plot and distribution. For trace make sure they are very overlapped showing convergence
# plot(burst.out)
# 
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
