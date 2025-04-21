## Structure data for MSOM

## Adapted from code by Kaitlyn Gaynor and Lindsey Rich, Aninam Conservation 2020
## Community occupancy model (with functional groups)

library(tidyverse)
library(jagsUI)
library(tictoc)


## Read in data ----------------------------------------------------------------

  det_data <- readRDS('ms_occ/inputs/det_data_cleaned.RDS')
  site_data <- readRDS('ms_occ/inputs/site_data.RDS')

  
## Select focal species only ---------------------------------------------------    
  
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')

  det_data <- det_data %>% filter(class %in% focal_species)
  
  
## Merge det data and site cov -------------------------------------------------
  
  head(site_covs); head(dh_simple)
  D <- dh_simple %>% left_join(site_covs, by = join_by(site_primary == site_season))
    head(D)
    dim(D) #nrows should match above
  
    
  #standardize select covariates
  names(D)
  #remember correlated pairs: 
  #  c('prop_M1_3x3','prop_M3_3x3'), 
  #  c('prop_M1_3x3','tree_vol_mean_3x3'),
  #  c('prop_M3_3x3','tree_vol_mean_3x3'), 
  #  c('pct_slope_mean_3x3','elev_sd_3x3'),
  
  X <- D %>% select(elev_mean_3x3, pct_slope_mean_3x3, tree_vol_mean_3x3, dist_cleared_mean_3x3,
                    dist_dambo_mean_3x3, dist_river) #or use M1 or M3 in lieu of tree vol
  X <- apply(X, 2, scale)
  dim(X) #6 covariates right now
  
  #should I log-transform the distance variables?
  
  #now add back any other site-level covar I want to use (no categorical ones now)
    
      
## Add survey-level covar ------------------------------------------------------
  
  #just season, which is already in the D object
  #how could I implement effort here? it's survey-varying, not just site-varying
  
  dX <- D %>% select(season)
  dX$season <- as.numeric(dX$season)
  

## Prep model ------------------------------------------------------------------
  
  # Define data and parameters
  data <- list(D = D$wks_det,
               N = ceiling(D$wks_srv),
               Species = as.numeric(as.factor(D$class)),
               n = nrow(D), 
               nspp = max(as.numeric(as.factor(D$class))),
               X = X, 
               dX = dX)
  
  str(data)
    length(data$D)
    length(data$N)
    length(data$Species)
    data$n #39520 site-seasons
    data$nspp #13 sp
    dim(data$X) #6 site cov
    dim(data$dX) #1 surv covar
      
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
  tic()
  # out3 <- jags(data = data, 
  #              inits = inits, 
  #              parameters.to.save = params, 
  #              model.file ="ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-occupancy-model-nogroups-jags.txt", 
  #              n.chains = nc, 
  #              n.iter = ni,
  #              n.burnin = nb, 
  #              n.thin = nthin)
  output <- jagsUI(data = data, 
                   inits = inits, 
                   parameters = params, 
                   store.data = TRUE,
                   model = 'ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-occupancy-model-nogroups-jags.txt',
                   n.chains=nc, 
                   n.thin=, 
                   n.iter=ni, 
                   n.burnin=nb, 
                   n.adapt=10000, 
                   parallel=TRUE)
  toc()
  Sys.Date()
  #start ~1:39 am
  
  saveRDS(out3, 'ms_occ/outputs/target_sp_no_guilds.RDS')
  
  