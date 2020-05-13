#This script currently houses some of the model testing code needed

#This  is script needed for calculating a Bayesian p value from a predictive posterior distribution

#Insert this loop that alongside the standard process model
#This is creating the posterior predictive distribution

#Will need to have the munew[k] altered depending on which model is being used
for(k in 1:nObs){
  Ynew[k]  ~ dnorm(munew[k], S)
  munew[k] <- Ex[acc[k]] + THRESH[sp[k]]
}

#Test statistics of the new distribution (Run in the jags model alongside priors)
d[1] <- max(Ynew[])
d[2] <- min(Ynew[])
d[3] <- max(Ynew[])-min(Ynew[])
d[4] <- mean(Ynew[])
d[5] <- sd(Ynew[])


#Converting the ooutput into a workable format
p.out   <- coda.samples (model = burst.model,
                             variable.names = c("d"),
                             n.iter = 100000)


#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
p.burn <- window(p.out,start=burnin)  ## remove burn-in
plot(p.burn)
summary(p.burn)


d  <- p.burn[[1]]

do <- c(max(dat.comb$GDD5.cum),min(dat.comb$GDD5.cum),max(dat.comb$GDD5.cum)-min(dat.comb$GDD5.cum),
        mean(dat.comb$GDD5.cum),sd(dat.comb$GDD5.cum))

names <- c("max","min","range","mean","sd")

for(j in 1:ncol(d)){
  pval <- mean(d[,j]>do[j])
  hist(d[,j],breaks=25,xlab="PPD",main=paste("p-value",j," = ",round(pval,2),sep=""))
  abline(v=do[j],lwd=2,col=2)
  legend("topright",names[j],inset=0.05,pch=0)
  
}











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
