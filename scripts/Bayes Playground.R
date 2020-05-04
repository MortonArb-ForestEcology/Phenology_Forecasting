#This is a playground for lucien to test models with minor variations.
#The lines that make his codes unique to their counterparts should be commented


library(rjags)
library(coda)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)

species <- c("Quercus macrocarpa", "Quercus alba", "Acer rubrum", "Acer saccharum")
dat.comb <- dat.all[dat.all$Species %in% species, ]

#This is the hierarchical model including the species influence on Threshold
#Currently when 1 species is used it runs identical to the model that doesn't account for species which is good
#When run with multiple species it will give different Thresholds and they have smaller SD than when done individually
#Issue is order of THRESH DOES NOT correspond to their order in our species selection
hierarchical_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Ex[pln[k]] + THRESH[sp[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], S)
    }
    
    
    # Priors
    for(j in 1:nSp){                      #This loop adds the species effect on Threshold
    THRESH[j] ~ dnorm(b1, v1)
    }
    
    for(i in 1:nPln){
        Ex[i] <-  b[i]
        b[i] ~ dnorm(b0, v0)
    }
    
    
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
  inits[[i]] <- list(b=rnorm(burst.list$nAcc,0,5),
                     # b=runif(burst.list$nAcc,0,1e4),
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
    THRESH[j] ~ dnorm(b1, v1)
    }
    
    for(i in 1:nPln){
        Ex[i] <-  b[i]
        b[i] ~ dnorm(b0, v0)
    }
    
    
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
  inits[[i]] <- list(b = rnorm(1,0,5), S = runif(1,1/200,1/20))
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
# gelman.diag(burst.out)
# 
# #Checking where convergence occured
# GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)

summary(burst.burn)
