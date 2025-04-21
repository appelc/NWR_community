## Set your working directory to the "Simple Example Folder"
##     Session -> Set Working Directory ->  Choose Directory
##     Then navigate to where you saved the full example folder

##Load the necessary packages
library(R2jags)

## load workspace with data for analysis
load("SimpleExample_DataSetUp.RData")

####################################
## Examine the layout of the data ##
####################################
## There are a total of 3 layers in our nested strcutrue. Species (i), site.year (j), 
##     and season.visit(k).  It's important to ensure the data is set up to accurately 
##     match the layers at which that variable changes

## The occupancy model covariate, Years Since Ditch Maintance, varies at the site.year
##      level so it can be represent as a vector with length 32 (as there are 32 site.years)
YSDM


## The detection model covariate, Rain, differs at the site.year/season.visit level, so
##     it must be represented as a matrix
Rain


######################
## Setup JAGS Model ##
######################
## Since this is a multi-level model we are going to allow coefficients to vary by some grouping factors
##     In this case we will allow certain coefficients to vary by site.year while others vary by species.
##     The model you specify should make biological sense, for example we do not allow detection probability 
##     to vary by site.year (this is a call survey) but we do allow detection probability to vary by species.


#Specify the model
SimpleExample.occ.model<-function(){
  #Prior distributions on community level parameters
  #  Species specific coefficients will come from these common distributions
  mu.a.0~dnorm(0,0.333)
  mu.a.YSDM~dnorm(0,0.5)
  mu.b.0~dnorm(0,0.333)
  mu.b.Rain~dnorm(0,0.5)
  
    ## JAGS normal distribution uses 1/Var (tau)
  tau.a.0~dgamma(1.5,0.5)
  tau.a.YSDM~dgamma(1.5,0.5)
  tau.b.0~dgamma(1.5,0.5)
  tau.b.Rain~dgamma(1.5,0.5)
  
    ## Still calculate sd as this is what we normally use for a normal distribution 
  sig.a.0<-pow(tau.a.0,-0.5)
  sig.a.YSDM<-pow(tau.a.YSDM,-0.5)
  sig.b.0<-pow(tau.b.0,-0.5)
  sig.b.Rain<-pow(tau.b.Rain,-0.5)
  
  ## Loop through each species, getting species specific parameter estimates
  for(i in 1:nspecies){
    a.0[i]~dnorm(mu.a.0,tau.a.0)
    a.YSDM[i]~dnorm(mu.a.YSDM,tau.a.YSDM)
    b.0[i]~dnorm(mu.b.0,tau.b.0)
    b.Rain[i]~dnorm(mu.b.Rain,tau.b.Rain)
        
    #Loop to estimate true occupancy, start at site/year level as there is where occupancy differs
    for(j in 1:nsite.year){
    
      ## Occupancy Probability Model
      logit(psi[i,j])<- a.0[i] + a.YSDM[i]*YSDM[j]
        
      ## limits to keep occupancy probability away from 0 or 1
      mu.psi[i,j]<-min(0.999,max(psi[i,j],0.001))
      
      ## Estimate latent occupancy at each step along the chain
      z[i,j]~dbern(mu.psi[i,j])
      
      ## Detection Probability Model, now loop through season/visit because 
      ##     detection probabilities vary at this level
      for(k in nseason.visit.1[j]:nseason.visit){
        logit(p[i,j,k])<- b.0[i] + b.Rain[i]*Rain[j,k] 
        
        ## limits to keep detection probability away from 0 or 1
        zzmu.p[i,j,k]<-min(0.999, max(p[i,j,k],0.001))*z[i,j]
        
        ## Finally see that actual observations (X) are modeled by estimated detection probabilities
        X[i,j,k]~dbern(zzmu.p[i,j,k])

      } ## Closes season/visit loop
    } ## Closes site/year loop
    
    ## Because there were no observations in April/May 2012 need to set those detection probabilities to zero
    for(j in 1:16){
      for(k in 1:4){
        zzmu.p[i,j,k]<-0
      }
    }
    
  } ## Closes the species specific loop
  
  
  
  ## Calculate Site Richness for each site in each year at each step along the chain.  Site Richness 
  ##     is defined as the total number of species present at each site.  This value is calculated 
  ##     based on the estimated true occupancy values at each step along the chain. 
  ##     
  for(j in 1:nsite.year){
      SiteRichness[j]<-sum(z[,j])
  }
  
} ## Closes the model loop

#Write text file of model
write("model{SimpleExample.occ.model}","SimpleExample.occ.model.txt")
model.file<-"SimpleExample.occ.model.txt"

## Specify initial values
## Calculate observed occupancy to use as a starting value for true occupancy.  
##     This will be 1 if species i was detected at site.year j and 
##     0 if species i was not detected at site.year j  
zinits<-matrix(NA,nrow=15, ncol=32)
for(i in 1:nspecies){
  for(j in 1:nsite.year){
    zinits[i,j]<-max(X[i,j,],na.rm=TRUE)
  }
}

## Coefficient Initial Values
##     Select starting values for each species specific coefficient value.  Use a normal distribution
##     centered at zero with a sd of 1.  Include the occupancy initial values in the list as well.
inits.Example=function(){
  list(a.0=rnorm(nspecies,0), a.YSDM=rnorm(nspecies,0),
       b.0=rnorm(nspecies,0),b.Rain=rnorm(nspecies,0),
       z=zinits)
}

## A list of all the necessary data pieces used in the model, including loop sizes, covariates, 
##     and raw detection history
data.Example<-list(nspecies=nspecies, nsite.year=nsite.year, nseason.visit=nseason.visit, 
                 nseason.visit.1=nseason.visit.1,
                 YSDM=YSDM, Rain=Rain,X=X)

## A list of the parameters we want to save.  You must list everything you want saved or JAGS 
##     will not save the chain.  In my opinion it's better to save more things than you think 
##     you will need.  You can always ignore something later, but you cant get it back if you 
##     don't save it to begin with.
params.Example<-c("a.0", "a.YSDM", "b.0", "b.Rain", 
                "mu.a.0", "mu.a.YSDM", "mu.b.0", "mu.b.Rain", 
                "sig.a.0", "sig.a.YSDM", "sig.b.0", "sig.b.Rain", 
                "SiteRichness")

## If you want to record how long the model takes to run initalize a begin time befor starting the model
beg.time<-Sys.time()

## Name and fit the model in JAGS
##     Include the data, initial values, parameters to save and the model file as well as information
##     about how many chains to run for how many steps and how many to burnin and how to thin
Example<-jags(data=data.Example,inits=inits.Example,parameters.to.save=params.Example,model.file=SimpleExample.occ.model,
            n.chains=3,n.iter=100000,n.burnin=50000,n.thin=50,
            DIC=TRUE, working.directory=NULL, jags.seed = 1234, refresh = 40, progress.bar = "text", digits=5)

## Get the run time of the model by getting the difference between the begin time and 
##     the time at the end of the model run
runtime<-Sys.time()-beg.time
runtime

## Save workspace.
##     Be careful to not save over the finished run or you will lose your workspace
save.image("SimpleExample_ModelRun.Rdata")

