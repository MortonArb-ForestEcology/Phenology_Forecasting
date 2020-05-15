
dat.all <- read.csv("../data_processed/Phenology_Met_combined_linear.csv")
dat.all$Accession <- unlist(lapply(strsplit(paste(dat.all$PlantNumber), "-"), function(x){x[1]}))
dat.all$Date <- as.Date(dat.all$Date)

species <- c("Quercus acutissima", "Quercus georgiana", "Quercus imbricaria", "Quercus stellata")
dat.comb <- dat.all[dat.all$Species %in% species, ]


univariate_regression <- "
model{

  b ~ dmnorm(b0,Vb)  	## multivariate Normal prior on vector of regression params
  S ~ dgamma(s1,s2)    ## prior precision

  for(i in 1:n){
	  mu[i] <- b[1] + b[2]*x[i]   	## process model
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
  
}
"

burst.list <- list(x = dat.comb$GTmean, y = dat.comb$Yday, n = length(dat.comb$Yday))

burst.list$b0 <- as.vector(c(0,0))      ## regression b means
burst.list$Vb <- solve(diag(10000,2))   ## regression b precisions
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b = rnorm(2,0,5), S = runif(1,1/200,1/20))
}

burst.model   <- jags.model (file = textConnection(univariate_regression),
                         data = burst.list,
                         inits = inits,
                         n.chains = 3)


burst.out   <- coda.samples (model = burst.model,
                            variable.names = c("b","S"),
                            n.iter = 5000)



gelman.diag(burst.out)


# #Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 1000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)

burst.df2 <- as.data.frame(as.matrix(burst.burn))

write.csv(burst.df2, file.path("../data_processed/", paste0("Posteriors_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".csv")), row.names=F)


if(ncol(burst.df2)>2){
  pdf(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".pdf")))
  for(i in 1:ncol(burst.df2)){
    print(plot(burst.burn[,i], main=names(burst.df2)[i]))                             ## check diagnostics post burn-in
  }
  print(hist(dat.comb$Yday))
  print(hist(dat.comb$GDD5.cum))
  dev.off()
  dev.off()
} else {
  png(file.path("../data_processed/", paste0("TracePlots_", gsub(" ", "_", "Chosen_Oaks_linear_norm"), ".png")), height=8, width=8, units="in", res=240)
  print(plot(burst.burn))                             ## check diagnostics post burn-in
  dev.off()
  dev.off()
  
}