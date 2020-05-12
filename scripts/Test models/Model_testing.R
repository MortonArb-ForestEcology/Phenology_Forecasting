#This script currently houses some of the model testing code needed
#It applies to all models run as long as aline is added to calculate a log liklihood and initialize the model


library("loo")

#Add this line to your code in the process model loop
log_lik[k] <- logdensity.norm(y[k], mu[k], S)


#Converting the output into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("log_lik"),
                             n.iter = 100000)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out, start=burnin)  ## remove burn-in
summary(burst.burn)


#Converting to a matrix format
log.loo <- as.matrix(burst.burn)

#Check dimension for chain_id in next line
#Chain id will have rep 1:nchain and the length of the matrix/the number of chains (calculated because of burnin)
dim(log.loo)
reff <- relative_eff(exp(log.loo), chain_id = rep(1:nchain, each = nrow(log.loo)/nchain))


check <- loo(log.loo, r_eff = reff)

wcheck <- waic(log.loo)
