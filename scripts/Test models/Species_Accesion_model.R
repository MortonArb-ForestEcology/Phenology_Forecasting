#This is the standard species hierarchical model with an added level for accesion number

library(rjags)
library(coda)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)

species <- c("Quercus acutissima", "Quercus georgiana", "Quercus imbricaria", "Quercus stellata")
dat.comb <- dat.all[dat.all$Species %in% species, ]


hierarchical_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Ex[acc[k]] + THRESH[sp[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dlnorm(mu[k], S)
    }
    
    for(k in 1:nObs){
      Ynew[k]  ~ dlnorm(munew[k], S)
      munew[k] <- Ex[acc[k]] + THRESH[sp[k]]
    }
    
    # Priors
    for(j in 1:nSp){                      #This loop adds the species effect on Threshold
    THRESH[j] ~ dnorm(0, tPrec)
    THRESHY[j] <- exp(THRESH[j])
    }
    
    for(t in 1:nAcc){
    Ex[t] <- ind[pln[t]] + c[t]
    c[t] ~ dnorm(0, aPrec)
    }
    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    tPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
    bPrec ~ dgamma(0.1, 0.1)
    S ~ dgamma(s1, s2)
    
    d[1] <- max(Ynew[])
    d[2] <- min(Ynew[])
    d[3] <- max(Ynew[])-min(Ynew[])
    d[4] <- mean(Ynew[])
    d[5] <- sd(Ynew[])
  }
  "

burst.list <- list(y = dat.comb$GDD5.cum, sp = as.numeric(factor(dat.comb$Species)),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   acc = as.numeric(factor(dat.comb$Accession)), nAcc = length(unique(dat.comb$Accession)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$GDD5.cum))

#Setting our uniformative priors
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2



#Setting the number of MCMC chains and their parameters
nchain = 10
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b=rnorm(burst.list$nPln,0,5),
                     THRESH=rnorm(length(unique(dat.comb$Species)), 0, 5),  #Added length equal to number of species
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
                             variable.names = c("THRESH"),
                             n.iter = 100000)


# #Checking that convergence happened
#gelman.diag(burst.out)


# #Checking where convergence occured
#GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)


burst.df2 <- as.data.frame(as.matrix(burst.burn))
colnames(burst.df2) <- c(as.character(unique(dat.comb$Species)))

write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "Chosen_Oaks"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  dev.off()
  dev.off()
} else {
  png(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks"), ".png")), height=8, width=8, units="in", res=240)
  print(plot(burst.burn))                             ## check diagnostics post burn-in
  dev.off()
  dev.off()
  
}

