#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Bayesian model which will become the finla product
# Inputs: dat.comb dataframe that is created by the Frequentist_GDD%-burst.r script in this repository
# Outputs: Currently, a posterior distribtuion of yday as a funciton of Gdd5.cum
# Notes: This script is basedon exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------#
#This section sets up the model itself
#---------------------------------------------------#
#rjags for the model and coda for the summary statistics
library(rjags)
library(coda)
#YOU WILL NEED JAGS INSTALLED rjags is a package for interfacting but you need the program itself http://mcmc-jags.sourceforge.net/


#Setting up the Jags model itself

univariate_regression <- "
model{

  ##b ~ dmnorm(b0,Vb)  	## multivariate Normal prior on vector of regression params
  b ~ dnorm (b0, v0)
  S ~ dgamma(s1,s2)    ## prior precision

  for(i in 1:n){
	  ##mu[i] <- b[1] + b[2]*x[i]   	## process model (simple linear regression)
	  mu[i] <- b[1]                   ## process model ANOVA
	  y[i]  ~ dnorm(mu[i],S)		        ## data model
  }
}
"

#Checking how good of a predictor they currently seem
plot(dat.comb$GDD5.cum, dat.comb$Yday)

#------------------------------------------------------#
#This section converts our observed into the neccessary format, defines our uninformed prior, and sets up our MCMC chains
#------------------------------------------------------#

#Converting to list format needed for JAGs
burst.list <- list( y = dat.comb$GDD5.cum, n = length(dat.comb$GDD5.cum))

#Setting our uniformative priors
##burst.list$b0 <- as.vector(c(0,0))      ## regression b means
##burst.list$Vb <- solve(diag(10000,2))   ## regression b precisions
burst.list$b0 <- 0
burst.list$v0 <- .0001
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


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
                            n.iter = 5000)


#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
plot(burst.out)


#Checking that convergence happened
gelman.diag(burst.out)

#Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 750                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)                             ## check diagnostics post burn-in


#Checking autocorrelation
acfplot(burst.burn)

#Checking effective size
effectiveSize(burst.burn)

summary(burst.burn)

#----------------------------------------------------------------------------#
#This section is for taking the model output and visualizing it
#----------------------------------------------------------------------------#

#Converting it into a matrix so we can work with it
burst.df <- as.data.frame(as.matrix(burst.burn))
burst.df$SD <- 1/sqrt(burst.df[,"S"])

#Creating a dataframe with the mean values of these metrics for every year
dat.yr <- aggregate(met.all[,c("TMAX", "TMIN", "TMEAN", "PRCP", "SNOW")],
                    by=met.all[,c("STATION", "YEAR")],
                    FUN=mean, na.rm=T)

#removing years too before reliable measurments
dat.yr <- dat.yr[dat.yr$YEAR>=2009,]

#Creating a matrix of the right size to be filled with the distribution of predictions for that year
mat.yr <- array(dim=c(nrow(dat.yr), nrow(burst.df)))
dimnames(mat.yr)[[1]] <- dat.yr$YEAR

#A distribution of gdd5.cum at bud burst that will be used to create a corresponding distribution of yday at bud burst
dat.gdd5.vec <- burst.df$b
##dat.gdd5.vec <- rnorm(nrow


#Function used to calculate bud burst day using gdd5
calc.bud <- function(x){min(dat.tmp[which(dat.tmp$GDD5.cum >= x),"YDAY"])}

#Filling a matrix with yday values calculating by running calc.bud on a distribution of gdd5.cum values at bud burst (dat.gdd5.vec)
#2013 is problematic. Doesnt have enough data for accurate gdd5.cum. For now its ok for visuals but check in regarding gdd5.cum limits.
i <- 1
for(i in 1:nrow(dat.yr)){
  YR=dat.yr$YEAR[i]
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  # summary(dat.tmp)
  if(nrow(dat.tmp)==0) next
  
  # Bloom time -- simple
  bud.pred <- calc.bud(mean(burst.df$b))
  if(bud.pred != Inf) dat.yr[i,"bud.oak"] <- bud.pred
  bud.vec <- unlist(lapply(dat.gdd5.vec, calc.bud))
  bud.vec[bud.vec==Inf] <- NA
  
  mat.yr[i,] <- bud.vec
}


#---------------------------------------------------------#
#This section is for summarizing the model data and observed data so they can be visualized and compared
#---------------------------------------------------------#
#Calculating summary statistics of the matrix of values from each year
dat.yr$bud.mean <- apply(mat.yr, 1, mean, na.rm=T)
dat.yr$bud.sd   <- apply(mat.yr, 1, sd, na.rm=T)
dat.yr$bud.lb   <- apply(mat.yr, 1, quantile, 0.025, na.rm=T)
dat.yr$bud.ub   <- apply(mat.yr, 1, quantile, 0.975, na.rm=T)
summary(dat.yr) 


#Aggregating all indivudla measurements into one mean for the every year
oak.bud <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                     by=list(dat.comb$Year),
                     FUN=mean, na.rm=F)


#Calculating sd values to put on our visualizations
oak.bud[,c("Yday.sd", "GDD5.cum.sd")]  <- aggregate(dat.comb[,c("Yday", "GDD5.cum")],
                                                    by=list(dat.comb$Year),
                                                    FUN=sd, na.rm=F)[,c("Yday", "GDD5.cum")]


#Graphing the output of the model vs. the observed data
ggplot(data=dat.yr[,]) +
  geom_ribbon(data=dat.yr[,], aes(x=YEAR, ymin=bud.lb, ymax=bud.ub, fill="Modeled"), alpha=0.5) +
  geom_point(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) +
  geom_line(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) + 
  geom_pointrange(data=oak.bud, aes(x=Group.1, y=Yday, ymin=Yday-Yday.sd, ymax=Yday+Yday.sd,color="Observed"))+
  ggtitle("Modeled and Observed day of budburst for Quercus Macrocarpa")+
  scale_y_continuous(name="Day of Year") +
  scale_x_discrete(name="Year",limits=c(2008,2019)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_text(size=rel(1.5), color="black"),
        axis.title=element_text(size=rel(1.5), face="bold"),)

 





#Creating samples for making credible interval and prediction interval tests
#nsamp <- 5000
#samp <- sample.int(nrow(burst.mat),nsamp)
#xpred <- 0:450  					## sequence of x values we're going to test for
#npred <- length(xpred)				##      make predictions for
#ypred <- matrix(0.0,nrow=nsamp,ncol=npred)	## storage for predictive interval
#ycred <- matrix(0.0,nrow=nsamp,ncol=npred)	## storage for credible interval


#Loop that creates the credible interval from rows in our matrix and the prediction interval by calculating the sd from precision
#for(g in seq_len(nsamp)){
#  theta = burst.mat[samp[g],]
#  ycred[g,] <- theta["b[1]"] + theta["b[2]"]*xpred
#  ypred[g,] <- rnorm(npred,ycred[g,],1/sqrt(theta["S"]))
#}

#Filling out the actual intervals for visualizations
#ci <- apply(ycred,2,quantile,c(0.025,0.5,0.975))  ## credible interval and median
#pi <- apply(ypred,2,quantile,c(0.025,0.975))		## prediction interval

#Plotting our observed data with our median, CI's and PI's of our linear model
#plot(dat.comb$GDD5.cum,dat.comb$Yday,xlim=c(0,480),ylim=c(80,150))
#lines(xpred,ci[1,],col=3,lty=2)	## lower CI
#lines(xpred,ci[2,],col=3,lwd=3)	## median
#lines(xpred,ci[3,],col=3,lty=2)	## upper CI
#lines(xpred,pi[1,],col=4,lty=2)	## lower PI
#lines(xpred,pi[2,],col=4,lty=2)	## upper PI


#ggplot(mosq, aes(x=time, y=density))+
#  facet_wrap(~replicate)+
#  geom_point()





#--------------------------------------------------------#
#Everything below here is a work in progress on making the hierarchicial model. WORKS BUT IN PROGRESS FOR QUALiTY CHECKS :)
#--------------------------------------------------------#

#Subsetting out any trees that don't have multiple years of observations
#I'm not sure if those individuals would mess with the model but I need it working without them to check
dat.comb$PlantNumber <- factor(dat.comb$PlantNumber)
tab <- table(dat.comb$PlantNumber)
dat.comb <- dat.comb[dat.comb$PlantNumber %in% names(tab)[tab>=2],]


#Currently these priors are uniformative and identical to univariate regression model. Not sure what changes needed if any.
hierarchical_regression <- "
model{
  
  S ~ dgamma(s1,s2)    ## prior precision for all individuals

  for(i in 1:p) {
    a[i] ~ dnorm(b0, S) #random individual effect on slope
    b[i] ~  dnorm(b1, S) #random individual effect on intercept
  }
  for(k in 1:n) {
    mu[k] <- a[individual[k]] + b[individual[k]] * x[individual[k]] 
    y[k] ~ dnorm(mu[k], S)
  }
}
"

burst.list <- list(x = dat.comb$GDD5.cum, y = dat.comb$Yday, individual = dat.comb$PlantNumber, p = length(dat.comb$PlantNumber), n = length(dat.comb$Yday))

#Setting our uniformative priors
burst.list$b0 <- dnorm(0,.00001)      ## regression random individual effect on intercept mean
burst.list$b1 <- dnorm(0,.00001)     ## regression random individual effect on slope mean
burst.list$s1 <- 0.1                    ## error prior n/2
burst.list$s2 <- 0.1                    ## error prior SS/2


#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(S = runif(1,1/200,1/20))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
#running the model
burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("a","b"),
                             n.iter = 5000)


#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
plot(burst.out)


#Checking that convergence happened
gelman.diag(burst.out)

#Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 1000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)                             ## check diagnostics post burn-in

#Checking autocorrelation
acfplot(burst.burn)

#Checking effective size
effectiveSize(burst.burn)

summary(burst.burn)

