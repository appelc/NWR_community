## Structure data for MSOM

## Adapted from code by Kaitlyn Gaynor and Lindsey Rich, Aninam Conservation 2020
## Community occupancy model (with functional groups)

library(data.table)
library(tidyverse)
library(R2jags)
library(tictoc)
# library(reshape)
# library(reshape2)
# library(plyr)


## Read in data ----------------------------------------------------------------

#Stacked detection history (sites x seasons)
dh_stacked <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')

  #Remove site P23-C1. Not sure why there was a second camera here but it was only deployed for 10 wks in 2019
  # and we don't have covariate data for it. Looks like mostly baboon, warthog, small antelope.
  dh_stacked <- lapply(dh_stacked, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  
  #Make sure they're sorted by site
  dh_stacked <- lapply(dh_stacked, function(y) {y %>% arrange(site_primary)})
  
  #QC
  names(dh_stacked)
  head(dh_stacked$elephant); tail(dh_stacked$elephant) #example
  names(dh_stacked$elephant) #18 weeks per season
  dim(dh_stacked$elephant) #3040/16 seasons = 190 sites
  

## Collapse to n_weeks per site ------------------------------------------------
  
  dh_simple <- lapply(dh_stacked, function(z){
    z %>% rowwise() %>%
          mutate(n_det = sum(c_across(`01`:`18`), na.rm = TRUE), #total detections (photos)
                 wks_det = sum(c_across(`01`:`18`) > 0, na.rm = TRUE), #wks with detections
                 wks_srv = sum(!is.na(c_across(`01`:`18`)))) %>%   #wks site was surveyed
          ungroup() %>%
          select(site_primary, fence, class, n_det, wks_det, wks_srv)
  })
  
  list_dh_simple <- dh_simple
  
  #convert to dataframe
  dh_simple <- rbindlist(list_dh_simple)
  
  
# ## Add functional group --------------------------------------------------------  
# 
#   sp_table <- fread('ms_occ/species_table.csv')
#   ggplot(sp_table[sp_table$class_name != 'elephant'], aes(x = class_name, y = kg_high)) +
#     geom_pointrange(aes(ymin = kg_low, ymax = kg_high, color = size)) +
#     theme_bw() + theme(axis.text.x = element_text(angle = 90))
#     
#   sp_table %>% filter(size %in% c('small','med','large','xl')) %>% 
#                       group_by(size) %>% summarise(min = min(kg_low, na.rm = TRUE),
#                                                    max = max(kg_high, na.rm = TRUE))
#   
#   table(sp_table$guild, sp_table$size)
#   head(sp_table[,c('class_name','guild','size')])
#   
#   sp_table$guild_size <- paste(sp_table$size, sp_table$guild, sep = '_')
#     
#   #match up
#   head(dh_simple)
#   head(sp_table)  
#   
#   dh_simple$guild <- sp_table$guild[match(dh_simple$class, sp_table$class_name)]
#   distinct(dh_simple, class, guild)
#   
#   #remove some
#   dh_simple <- dh_simple[!(dh_simple$class %in% c('bat_sp','blue_monkey','bush_squirrel',
#                                                   'domestic_cattle','domestic_dog','goat',
#                                                   'human','other_animal','other_bird',
#                                                   'small_mammal')),]
#   #or select focal species only
#   # focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
#   #                    'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
#   
  
## Add site-level covar --------------------------------------------------------
  
  #read in input files I created for single-species models (stacked design)
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  
  #take from one species model input  
  site_covs <- occu_inputs$elephant@siteCovs
  
  #merge
  head(dh_simple); nrow(dh_simple); length(unique(dh_simple$class)) #should be below * n_species
  head(site_covs); nrow(site_covs) 
  
  D <- dh_simple %>% select(-fence) %>% #remove 'fence' from this dataframe and use the one from site_covs instead
                     left_join(site_covs, by = join_by(site_primary == site_season))
  head(D)

  #make guild covariate?
  unique(D$guild)
  G <- cbind(as.numeric(D$guild=='other'),
             as.numeric(D$guild=='primate'),
             as.numeric(D$guild=='grazer'),
             as.numeric(D$guild=='browser')) #so 'carnivore' will be ref group?
  dim(G)
  dim(D)
    
  #standardize select covariates
  names(D)
  #remember correlated pairs: 
  #  c('prop_M1_3x3','prop_M3_3x3'), 
  #  c('prop_M1_3x3','tree_vol_mean_3x3'),
  #  c('prop_M3_3x3','tree_vol_mean_3x3'), 
  #  c('pct_slope_mean_3x3','elev_sd_3x3'),
  
  X <- D %>% select(elev_mean_3x3, pct_slope_mean_3x3, tree_vol_mean_3x3, dist_cleared_mean_3x3,
                    dist_dambo_mean_3x3, dist_river, prop_M1_3x3, prop_M2_3x3, prop_M3_3x3)
  X <- apply(X, 2, scale)
  
  #now add back to all site-level covar I want to use, right?
  X
      
## Add survey-level covar ------------------------------------------------------
  
  #just season, which is already in the D object
  #how could I implement effort here? it's survey-varying, not just site-varying
  
  dX <- D %>% select(season)
  dX$season <- as.numeric(dX$season)
  
  
## Identify group covariates ---------------------------------------------------
  
  # XG = cbind(X*G[,1], X*G[,2], X*G[,3], X*G[,4])
  # head(XG)  #is this pulling out values matching class in each of the respective guilds? hmm
  
  
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
  tic()
  out3 <- jags(data = data, 
               inits = inits, 
               parameters.to.save = params, 
               model.file ="ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-occupancy-model-nogroups-jags.txt", 
               n.chains = nc, 
               n.iter = ni,
               n.burnin = nb, 
               n.thin = nthin)
  toc()
  Sys.Date()
  #start ~9:45 pm
  
  saveRDS(out3, 'ms_occ/outputs/all_sp_no_guilds.RDS')
  