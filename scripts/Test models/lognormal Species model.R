#This is the hierarchical model including the species influence on Threshold IN LOGNORMAL FORMAT
#Does not currently work well as range of distribution is much to large compared to the normal distribution
#NOt sure if I need to change conjugate priors or read the output differently or if the distribtuion is not a good choice
hierarchical_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Ex[pln[k]] + THRESH[sp[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dlnorm(mu[k], S)
    }
    
    
    # Priors
    for(j in 1:nSp){                      #This loop adds the species effect on Threshold
    THRESH[j] ~ dnorm(0, tPrec)
    }
    
    for(i in 1:nPln){
        Ex[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    tPrec ~ dgamma(0.1, 0.1)
    bPrec ~ dgamma(0.1, 0.1)
    S ~ dgamma(s1, s2)
  }
  "

burst.list <- list(y = dat.comb$GDD5.cum, sp = as.numeric(factor(dat.comb$Species)),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$GDD5.cum))

#Setting our uniformative priors
burst.list$b0 <- 0
burst.list$v0 <- 0.1
# burst.list$b1 <- 0
# burst.list$v1 <- .0001
# burst.list$b0 <- 0.1
# burst.list$v0 <- 0.1
burst.list$b1 <- 0
burst.list$v1 <- 0.001
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2

#Setting the number of MCMC chains and their parameters
nchain = 10
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b=rnorm(burst.list$nPln,0,5),
                     # b=runif(burst.list$nPln,0,1e4),
                     THRESH=rnorm(length(unique(dat.comb$Species)), 0, 5),  #Added length equal to number of species
                     # THRESH=runif(1,0,1e4),
                     S = runif(1,1/200,30))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 10)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("THRESH","S"),
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
