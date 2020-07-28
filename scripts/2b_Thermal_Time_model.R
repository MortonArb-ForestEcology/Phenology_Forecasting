#This is the standard species hierarchical model with an added level for accesion number

library(rjags)
library(coda)
library(data.table)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)

species <- c("Quercus acutissima", "Quercus georgiana", "Quercus imbricaria", "Quercus stellata")
#dat.comb <- dat.all[dat.all$Species %in% species, ]
dat.comb <- dat.all[dat.all$Species %like% "Quercus",]

SP <- as.data.frame(table(dat.comb$Species))
colnames(SP) <- c("Species", "Freq")

dat.comb[dat.comb$Species == "Quercus macranthera",]

df.ind <- aggregate(Accession~PlantNumber, data=dat.comb,
                     FUN=min)

df.acc <- aggregate(Species~Accession, data=dat.comb,
                    FUN=min)


hierarchical_regression <- "
  model{
    for(k in 1:n){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(k in 1:n){
        munew[k] <- ind[pln[k]]  
        Ynew[k] ~ dnorm(munew[k], sPrec)
    }
      
    for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(120, aPrec[j])
      aPrec[j] ~ dgamma(1, 0.1)
    }

    for(t in 1:nAcc){
      Accession[t] <-  THRESH[sp[t]] + b[t]
      b[t] ~ dnorm(0, bPrec[t])
      bPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  Accession[acc[i]] * c[i]
        c[i] ~ dnorm(1, cPrec)
    }
    
    sPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
    
    d[1] <- max(Ynew[])
    d[2] <- min(Ynew[])
    d[3] <- max(Ynew[])-min(Ynew[])
    d[4] <- mean(Ynew[])
    d[5] <- sd(Ynew[])
  }
  "

burst.list <- list(y = dat.comb$GDD5.cum, n = length(dat.comb$GDD5.cum),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   acc = as.numeric(factor(df.ind$Accession)), nAcc = length(unique(dat.comb$Accession)),
                   sp = as.numeric(factor(df.acc$Species)), nSp = length(unique(dat.comb$Species)))


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(  #Added length equal to number of species
                     sPrec = runif(1,1/200,30))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("THRESH", "aPrec"),
                             n.iter = 300000)


# #Checking that convergence happened
gelman.diag(burst.out)

DIC <- dic.samples(burst.model, 50000)

# #Checking where convergence occured
#GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)


#Renaming parameters to properly match their effects (e.g. sites are renamed to their Site, species to their species)
varnames(burst.burn) <- c(paste(as.character(SP$Species), sep=""))

df <- ggs(burst.burn)

ggs_density(df, family = "Quercus macrocarpa")

ggs_caterpillar(df, family =  "effect")



burst.df2 <- as.data.frame(as.matrix(burst.burn))
colnames(burst.df2) <- c(as.character(unique(dat.comb$Species)))

write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "Chosen_Oaks_norm"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks_norm"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  print(hist(dat.comb$Yday))
  print(hist(dat.comb$GDD5.cum))
  dev.off()
  dev.off()
} else {
  png(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks"), ".png")), height=8, width=8, units="in", res=240)
  print(plot(burst.burn))                             ## check diagnostics post burn-in
  dev.off()
  dev.off()
  
}


