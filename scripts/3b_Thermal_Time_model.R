#This is the standard species hierarchical model with an added level for accesion number
library(rjags)
library(coda)
library(dplyr)
library(tidyr)

dir.create("../data_processed/model_output", recursive = T, showWarnings = F)

# Read in output of previous script
dat.b <- read.csv("../data_processed/Oak_collection_budburst.csv")
dat.b$Accession <- unlist(lapply(strsplit(paste(dat.b$PlantNumber), "-"), function(x){x[1]}))
dat.b$Date <- as.Date(dat.b$Date)

# Read in output of previous script
dat.l <- read.csv("../data_processed/Oak_collection_leaf.csv")
dat.l$Accession <- unlist(lapply(strsplit(paste(dat.l$PlantNumber), "-"), function(x){x[1]}))
dat.l$Date <- as.Date(dat.l$Date)

#Pulling out the number of observations we have for each species
SP.burst <- as.data.frame(table(dat.b[, "Species"]))
colnames(SP.burst) <- c("Species", "Freq")

#Pulling out the number of observations we have for each species
SP.leaf <- as.data.frame(table(dat.l[, "Species"]))
colnames(SP.leaf) <- c("Species", "Freq")

#CHecking which species we don't have budburst information for
setdiff(SP.leaf$Species, SP.burst$Species)

#SP.burst <- SP.burst[(1:5),]

#Creating a data frame to check the model output
Check <- data.frame()
l <- 1
SP.burst <- SP.burst[SP.burst$Species %in% SP.leaf$Species,]

#We are using burst to determine the species becasue it is missing two species: fusiformis and laurifolia
for(SP in SP.burst$Species){
  
  #Creating the row for this species information
  Check[l, "species"] <- SP
  Check[l, "burst.nObs"] <- SP.burst[SP.burst$Species == SP, "Freq"]
  Check[l, "leaf.nObs"] <- SP.leaf[SP.leaf$Species == SP, "Freq"]
  
  dat.burst <- dat.b[dat.b$Species == SP, ]
  dat.leaf <- dat.l[dat.l$Species == SP, ]
  
  #Checking the mean
  Check[l, "mean.burst"] <- mean(dat.burst$GDD5.cum, na.rm = T)
  Check[l, "mean.leaf"] <- mean(dat.leaf$GDD5.cum, na.rm = T)
  
  #Creating indexes so the hierarchy can properly function
  burst.ind <- aggregate(Species~PlantNumber, data=dat.burst,
                         FUN=min)

  
  leaf.ind <- aggregate(Species~PlantNumber, data=dat.leaf,
                        FUN=min)

  
  
  hierarchical_regression <- "
  model{
    for(k in 1:n){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(j in 1:nSp){
      THRESH <-  a
      a ~ dnorm(Tprior, aPrec)
      aPrec ~ dgamma(0.2, 0.1)
      Tprior ~ dunif(1, 600)
    }
    
    for(i in 1:nPln){
        ind[i] <-  THRESH[sp[i]] + c[i]
        c[i] ~ dnorm(0, cPrec)
    }
    sPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)

  }
  "
  
  burst.list <- list(y = dat.burst$GDD5.cum, n = length(dat.burst$GDD5.cum),
                     pln = as.numeric(factor(dat.burst$PlantNumber)), nPln = length(unique(dat.burst$PlantNumber)),
                     sp = as.numeric(factor(burst.ind$Species)), nSp = length(unique(dat.burst$Species)))
  
  leaf.list <- list(y = dat.leaf$GDD5.cum, n = length(dat.leaf$GDD5.cum),
                    pln = as.numeric(factor(dat.leaf$PlantNumber)), nPln = length(unique(dat.leaf$PlantNumber)),
                    sp = as.numeric(factor(leaf.ind$Species)), nSp = length(unique(dat.leaf$Species)))
  
  
  #Setting the number of MCMC chains and their parameters
  nchain = 3
  inits <- list()
  rngs <- c(737, 874, 869)
  for(i in 1:nchain){
    inits[[i]] <- list(.RNG.name = "base::Super-Duper",
                       .RNG.seed = as.integer(rngs[i])
    )
  }
  
  #---------------------------------------------------------#
  #This section actually runs the model and then provides ways to check the output and clean it
  #---------------------------------------------------------#
  burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                               data = burst.list,
                               inits = inits,
                               n.chains = 3)
  
  leaf.model   <- jags.model (file = textConnection(hierarchical_regression),
                              data = leaf.list,
                              inits = inits,
                              n.chains = 3)
  
  
  #Converting the ooutput into a workable format
  burst.out   <- coda.samples (model = burst.model,
                               variable.names = c("THRESH", "aPrec"),
                               n.iter = 700000)
  
  #Converting the ooutput into a workable format
  leaf.out   <- coda.samples (model = leaf.model,
                              variable.names = c("THRESH", "aPrec"),
                              n.iter = 700000)
  
  
  # #Checking that convergence happened
  bud.con <- gelman.diag(burst.out)
  Check[l, "burst.converge"] <- bud.con[[1]][1]
  leaf.con <- gelman.diag(leaf.out[,"THRESH"])
  Check[l, "leaf.converge"] <- leaf.con[[1]][1]
  
  #Removing burnin before convergence occurred
  burnin = 690000                                ## determine convergence from GBR output
  burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
  summary(burst.burn)
  
  leaf.burn <- window(leaf.out, start = burnin)
  summary(leaf.burn)
  
  #converting them into a dataframe 
  burst.df <- as.data.frame(as.matrix(burst.burn))
  leaf.df <- as.data.frame(as.matrix(leaf.burn))
  
  #calculating sd from the precison
  burst.df$sd <- 1/sqrt(burst.df[,"aPrec"])
  summary(burst.df)
  leaf.df$sd <- 1/sqrt(leaf.df[,"aPrec"])
  summary(leaf.df)
  
  
  burst.df$species <- SP
  bud.ci <- apply(as.matrix(burst.df$THRESH),2,quantile,c(0.025,0.5,0.975))
  
  leaf.df$species <- SP
  leaf.ci <- apply(as.matrix(leaf.df$THRESH),2,quantile,c(0.025,0.5,0.975))
  
  Check[l, "burst 2.5%"] <- bud.ci[1]
  Check[l, "burst 50%"] <- bud.ci[2]
  Check[l, "burst 97.5%"] <- bud.ci[3]
  
  Check[l, "leaf 2.5%"] <- leaf.ci[1]
  Check[l, "leaf 50%"] <- leaf.ci[2]
  Check[l, "leaf 97.5%"] <- leaf.ci[3]
  
  write.csv(burst.df, file.path("../data_processed/model_output", paste0(SP, "_TT_model_budburst.csv")), row.names=F)
  write.csv(leaf.df, file.path("../data_processed/model_output", paste0(SP, "_TT_model_leaf.csv")), row.names=F)
  
  l <- l + 1
}

#Checking convergence and confidence interval of all species
write.csv(Check, file.path("../data_processed/model_output", paste0("Budburst_convergence.csv")), row.names=F)

#If they are below 1.1 then they are consider converged (I'll get the right citation for that I've seen it in a few papers)
Close <- Check[(Check$burst.converge > 1.05 | Check$leaf.converge > 1.05) , ]
Close

