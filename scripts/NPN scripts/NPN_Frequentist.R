#This is the frequentist model for the npn data
#This doesn't have a perfect lead in script because it isn't really going to be used but is a relic saved just in case
#Will combine with the Organize NPN data script and is a modified version of the Frequentist bud burst script


#loading ggplot for visualization 
library(ggplot2)
# species <- c("Quercus macrocarpa", "Quercus alba", "Acer rubrum", "Acer saccharum")
species <- "Quercus macrocarpa"

# Read in output of previous script
dat.comb <- read.csv("../data_processed/Phenology_Met_combined.csv")
dat.comb <- dat.comb[dat.comb$Species %in% species,]


# Testing whether GDD5 is a good predictor of day 
dat.gdd5.lm <- lm(Yday ~ GDD5.cum*Species, data=dat.comb)
summary(dat.gdd5.lm)
plot(Yday ~ GDD5.cum, data=dat.comb)

mat.yr <- array(dim=c(nrow(df.loc), 1000))
dimnames(mat.yr)[[1]] <- df.loc$year



#loading in nlme and lme4 for their linear mixed effect equations
library(nlme); library(lme4)

#creating a mean and sd for gdd
dat.gdd5.mean <- mean(dat.comb[,"GDD5.cum"], na.rm=T); 
dat.gdd5.sd <- sd(dat.comb[,"GDD5.cum"], na.rm=T)

#Creating a hierarchial mean to compare
mac.cue <- lme(GDD5.cum ~ 1, random=list(PlantNumber=~1), data=dat.comb, na.action=na.omit)
mac.summ <- summary(mac.cue)
MuMIn::r.squaredGLMM(mac.cue)
mod.cue.est <- mac.summ$tTable[,"Value"] # Hierarchical mean
mod.cue.se <- mac.summ$tTable[,"Std.Error"]
mod.cue.sd <- mac.summ$tTable[,"Std.Error"]*sqrt(mac.summ$tTable[,"DF"]+1)

# Compare to non-hierarchical
mod.cue.est; mod.cue.sd
dat.gdd5.mean; dat.gdd5.sd 

#Creating the distribtuion of gdd5 values to be ran through the calc.bud function
dat.gdd5.vec <- rnorm(1000, dat.gdd5.mean, dat.gdd5.sd)

#Function used to calculate bud burst day using gdd5
calc.bud <- function(x){min(dat.tmp[which(dat.tmp$GDD5.cum >= x),"YDAY"])}


for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    df.yr <- df.tmp[df.tmp$year==YR,]
    gdd.cum=0
    d.miss = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[j]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      
      df.yr[j,"GDD5.cum"] <- gdd.cum
    }
    # summary(dat.tmp)
    if(nrow(df.yr)==0) next
    
    # Bloom time -- simple
    bud.pred <- calc.bud(dat.gdd5.mean)
    if(bud.pred != Inf) df.loc[k,"bud.oak"] <- bud.pred
    bud.vec <- unlist(lapply(dat.gdd5.vec, calc.bud))
    bud.vec[bud.vec==Inf] <- NA
    
    # par(mfrow=c(2,1))
    # hist(dat.gdd5.vec); hist(bud.vec)
    # par(mfrow=c(1,1))
    
    mat.yr[k,] <- bud.vec
    k <- k +1
  }
  
}

df.loc$bud.mean <- apply(mat.yr, 1, mean, na.rm=T)
df.loc$bud.sd   <- apply(mat.yr, 1, sd, na.rm=T)
df.loc$bud.lb   <- apply(mat.yr, 1, quantile, 0.025, na.rm=T)
df.loc$bud.ub   <- apply(mat.yr, 1, quantile, 0.975, na.rm=T)

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
  ggtitle("Modeled and Observed day of year of budburst for Quercus Macrocarpa")+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_text(size=rel(2.5), color="black"),
        axis.title=element_text(size=rel(2.5), face="bold"),)
