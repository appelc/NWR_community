# Kaitlyn Gaynor & Lindsey Rich
# Animal Conservation 2020
# Community occupancy model (without functional groups)


# Load libraries ----------------------------------------------------------

library(R2jags)
library(reshape)
library(reshape2)
library(plyr)
library(tidyverse)


# Prepare data for modeling -----------------------------------------------

# Import detection history and camera operation
D <- read.csv("ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-detections.csv")
Covariates <- read.csv("ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-cammetadata.csv")
Spp <- read.csv("ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-species.csv") # Species IDs and traits

# Prep dataframe for analysis
D <- melt(D,vars = c("StudySite"))
colnames(D)[2:3] = c("SppCode","Detections")
D <- merge(D, Covariates, by = "StudySite") 
D <- merge(D, Spp, by = "SppCode")
D <- D[order(D$SppCode, D$StudySite),]

# Define function for standardizing covariates
zstd <- function (x)  (x-mean(x,na.rm = T))/sd(x,na.rm = T) 

# Select & standardize occupancy covariates
X = dplyr::select(D, termite.count.100m, fire.interval, hunting, road.major.distance, urema.distance, tree.hansen)
X = apply(X,2,zstd)

# Select & standardize detection covariates
dX = dplyr::select(D, detect.obscured, cover.ground)
dX = apply(dX,2,zstd)


# Run model ---------------------------------------------------------------

# Define data and parameters
data <- list(D = D$Detections,
             N = ceiling(D[,"Operation"]),
             Species = as.numeric(D$SppCode),
             n = nrow(D), 
             nspp = max(as.numeric(D$SppCode)),
             X = X, 
             dX = dX)

  str(data)
    length(data$D)
    length(data$N)
    length(data$Species)
    data$n
    data$nspp
    dim(data$X)
    dim(data$dX)

# Specify the initial values
inits = function() {list(Z = as.numeric(data$D>0))}

# Specify the parameters to be monitored
params = c("rho","pbeta","spbeta","sigpbeta","mbeta","sigbeta","sbeta",
           "psi.mean","sigma.occ","p.mean","sigma.p","alpha","Z","P")

nc = 3       # number of chains
ni = 60000   # number of iterations
nb = 10000   # burn-in period
nthin = 50   # thinning rate

# Run occupancy model
out3 <- jags(data = data, 
             inits = inits, 
             parameters.to.save = params, 
             model.file ="ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-occupancy-model-nogroups-jags.txt", 
             n.chains = nc, 
             n.iter = ni,
             n.burnin = nb, 
             n.thin = nthin)
  
  #start ~3:11
  #end ~3:25

# Explore model output ----------------------------------------------------

out3.sum <- out3$BUGSoutput$summary

# Save species names (for interpreting model results later)
sppnames <- as.character(unique(Spp$SppCode))

# Name output alpha and P
alpha <- out3$BUGSoutput$sims.list$alpha
p <- out3$BUGSoutput$sims.list$P

# Transform alphas b/c they are on logit scale
expit <- function(x)  1/(1+exp(-x))
logit.alpha <- expit(alpha)
logit.p <- expit(p)

# Calculate psi and p
psimeans <- colMeans(logit.alpha)
names(psimeans) <- sppnames
psimeans <- as.data.frame(psimeans)
head(psimeans)

pmeans <- colMeans(logit.p)
names(pmeans) <- sppnames
head(pmeans)

# Get the quantiles and 95% confidence intervals for psi and p.
psiCI <- apply(logit.alpha, 2, function(x) quantile(x,probs = c(0.025,0.1,0.5,0.9,0.975)))
colnames(psiCI) <- sppnames
head(psiCI)

pCI <- apply(logit.p, 2, function(x) quantile(x,probs = c(0.025,0.1,0.5,0.9,0.975)))
colnames(pCI) <- sppnames
head(pCI)

# Define the occupancy covariate effects where mbeta is the community-level hyperparameter and sbeta is the species-specific parameter.
mbeta <- out3$BUGSoutput$sims.list$mbeta
sbeta <- out3$BUGSoutput$sims.list$sbeta


# Species level estimates 

# Define the species
spec <- Spp[,1]

# Define the occupancy covariates and groups
covs <- colnames(X)

# Create a data frame where the number of rows is equal to the number of covariates * the number of species
species <- data.frame(expand.grid(covs,spec), matrix(NA,length(covs)*length(spec),4))
colnames(species) <- c("Factor","Species","Mean","SD","LCI","UCI")


# Create a loop that will estimate species-specific values for each of the covariates
for (a in 1:length(covs)){
    for (b in 1:length(spec)){
        sims <- mbeta[,a] + sbeta[,b,a]
        species[(ncol(X)*(b-1)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,c(0.025,0.975)))
    }
}

head(species)


# Determine species richness at each site ---------------------------------

# Define the z matrix
z = out3$BUGSoutput$sims.list$Z

# Sort the data frame based on species, study site, and functional group
d <- sort_df(merge(data.frame(ID = 1:nrow(D),D[,1:2]),data.frame(SppCode = spec, Group = Spp$FunctionalGroup)),"ID")[,c(1,3,4)]

# Create a new data frame
dz <- data.frame(d,t(z))

# Melt the data frame
m.dz <- melt(dz,id.vars = c("SppCode","StudySite","Group") )

# Aggregate the data by summing the values in the z matrix for each StudySite station during each iteration
# Use the aggregated values to create probability distributions and estimate mean, sd, and 95% credible interval values for StudySite-station specific species richness
z.all <- acast(m.dz,StudySite ~ variable, fun.aggregate = sum)
z.all <- t(apply(z.all,1,function(x) c(mean(x),sd(x),quantile(x,c(0.025,0.975)))))
names <- rownames(z.all)
rownames(z.all) <- NULL
z.all <- cbind(names,z.all)
colnames(z.all) = c("StudySite", "Mean","SD","LCI","UCI")

head(z.all)


# Calculate Hill numbers --------------------------------------------------

hill1 <- vector("numeric")
hill2 <- vector("numeric")

for(i in 1:nrow(logit.alpha)) {
    
    sum.alpha <- rowSums(logit.alpha)[i] # add up all occupancy probabilities for all species, in that iteration
    
    hill1.input <- vector("numeric")
    hill2.input <- vector("numeric")
    
    for(j in 1:ncol(logit.alpha)) {
        relative.alpha <- logit.alpha[i,j] / sum.alpha
        hill1.input[j] <- relative.alpha * log(relative.alpha)
        hill2.input[j] <- relative.alpha * relative.alpha
    }
    
    hill1[i] <- exp(-1 * sum(hill1.input))
    hill2[i] <- 1/sum(hill2.input)
}

hill <- cbind(hill1, hill2)

head(hill)
