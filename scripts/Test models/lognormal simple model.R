
#This is a test of using a lognormal distribution on our most basic model
#When the result is used with exponential base e the output is essentially identical. 
#The S value changes and I need to look into appropriate conversion
univariate_regression <- "
model{
  b ~ dnorm (b0, v0)
  S ~ dgamma(s1,s2)     
  for(i in 1:n){
	  mu[i] <- b                  
	  y[i]  ~ dlnorm(mu[i],S)		        ##This is changed to dlnorm which is all thats need for this model
  }
}
"

#------------------------------------------------------#
#This section converts our observed into the neccessary format, defines our uninformed prior, and sets up our MCMC chains
#------------------------------------------------------#

#Converting to list format needed for JAGs
burst.list <- list(y = dat.comb$GDD5.cum, n = length(dat.comb$GDD5.cum))

#Setting our uniformative priors
burst.list$b0 <- 0
burst.list$v0 <- .0001
burst.list$s1 <- .1                    ## error prior n/2
burst.list$s2 <- .1                    ## error prior SS/2


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b = rnorm(1,0,5))
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
                             n.iter = 100000)

# #Trace plot and distribution. For trace make sure they are very overlapped showing convergence
# plot(burst.out)
# 
# #Checking that convergence happened
gelman.diag(burst.out)
# 
# #Checking where convergence occured
# GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)

summary(burst.burn)
