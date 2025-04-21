## Structure data for MSOM

## Adapted from code by Kaitlyn Gaynor and Lindsey Rich, Animal Conservation 2020
## Community occupancy model (with functional groups)

library(data.table)
library(tidyverse)
library(R2jags)
library(tictoc)


## Read in data ----------------------------------------------------------------

#Stacked detection history (sites x seasons)
dh_stacked <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')

  #Remove site P23-C1
  dh_stacked <- lapply(dh_stacked, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  
  #Make sure they're sorted by site
  dh_stacked <- lapply(dh_stacked, function(y) {y %>% arrange(site_primary)})
  
  #QC
  names(dh_stacked)
  head(dh_stacked$elephant); tail(dh_stacked$elephant) #example
  names(dh_stacked$elephant) #18 weeks per season
  dim(dh_stacked$elephant) #3040/16 seasons = 190 sites
  

## Collapse to n_weeks per site ------------------------------------------------
  
  list_dh_simple <- lapply(dh_stacked, function(z){
    z %>% rowwise() %>%
          mutate(n_det = sum(c_across(`01`:`18`), na.rm = TRUE), #total detections (photos)
                 wks_det = sum(c_across(`01`:`18`) > 0, na.rm = TRUE), #wks with detections
                 wks_srv = sum(!is.na(c_across(`01`:`18`)))) %>%   #wks site was surveyed
          ungroup() %>%
          select(site_primary, class, n_det, wks_det, wks_srv)
  })
  
  #convert to dataframe
  dh_simple <- rbindlist(list_dh_simple)
  
  
## Add functional groups -------------------------------------------------------  

  #read in species data
  sp_table <- fread('ms_occ/species_table.csv')
  
  #how did I do categorizing group size?
  ggplot(sp_table[sp_table$class_name != 'elephant' & !is.na(sp_table$kg_high),], 
         aes(x = reorder(class_name, -kg_high), y = kg_high)) +
         geom_pointrange(aes(ymin = kg_low, ymax = kg_high, color = size)) +
         theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            axis.title.x = element_blank())
    
  sp_table %>% filter(size %in% c('small','med','large','xl')) %>% 
                      group_by(size) %>% summarise(min = min(kg_low, na.rm = TRUE),
                                                   max = max(kg_high, na.rm = TRUE))
  
  table(sp_table$guild, sp_table$size)
  
  #create size/guild group?
  sp_table$guild_size <- paste(sp_table$size, sp_table$guild, sep = '_')
    table(sp_table$guild_size)
  
  #create a few other categories...
  sp_table$guild_4 <- ifelse(sp_table$guild_size == 'med_browser', 'large_browser', sp_table$guild_size)
    table(sp_table$guild_4)
    
  #match up
  head(dh_simple)
  head(sp_table)  
  # dh_simple$guild <- sp_table$guild[match(dh_simple$class, sp_table$class_name)]
  dh_simple$guild <- sp_table$guild_4[match(dh_simple$class, sp_table$class_name)]
    distinct(dh_simple, class, guild)
  
  #remove some species
  # dh_simple <- dh_simple[!(dh_simple$class %in% c('bat_sp','blue_monkey','bush_squirrel',
  #                                                 'domestic_cattle','domestic_dog','goat',
  #                                                 'human','other_animal','other_bird',
  #                                                 'small_mammal')),]
  #or select focal species only
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  dh_focal <- dh_simple %>% filter(class %in% focal_species)
    head(dh_focal)
    distinct(dh_focal, class, guild)
      
  dh_simple <- dh_focal

  
## Add site-level covar --------------------------------------------------------
  
  #read in input files I created for single-species models (stacked design)
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  
  #take site covariates from one species model input  
  site_covs <- occu_inputs$elephant@siteCovs
  
  #QC
  nrow(site_covs) #site cov data: 3040 site-seasons (190 sites * 16 seasons)
  length(unique(dh_simple$class))  #number species: 13
  nrow(dh_simple) #detection data: should be 3040 site-seasons * n_species
  
  #merge
  head(site_covs); head(dh_simple)
  D <- dh_simple %>% left_join(site_covs, by = join_by(site_primary == site_season))
    head(D)
    dim(D) #nrows should match above

  #make guild covariate?
  unique(D$guild)
  G <- cbind(as.numeric(D$guild=='xl_grazer'), #is each row a member of this guild? 0/1
             as.numeric(D$guild=='large_browser'),
             as.numeric(D$guild=='xl_browser')) #so 'large_grazer' will be ref group?
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
                    dist_dambo_mean_3x3, dist_river) #or use M1 or M3 in lieu of tree vol
  X <- apply(X, 2, scale)
  dim(X) #6 covariates right now
  
  #should I log-transform the distance variables?
  
  #now add back any other site-level covar I want to use (no categorical ones now)
  
  
## Add survey-level covar ------------------------------------------------------
  
  #just season, which is already in the D object
  #how could I implement effort here? it's survey-varying, not just site-varying
  
  dX <- D %>% select(season)
  dX$season <- as.numeric(dX$season) #1=cool, 2=dry, 3=wet
  dim(dX)  
  
  
## Identify group covariates ---------------------------------------------------
  
  XG = cbind(X*G[,1], X*G[,2], X*G[,3]) #use 1 fewer than the number of guilds
  head(XG) #pulls out values matching class in each of the respective guilds?
  dim(XG) #number of columns = number of covariates * number of guilds
  
  
## Prep model ------------------------------------------------------------------
  
  # Define data and parameters
  data <- list(D = D$wks_det,
               N = ceiling(D$wks_srv),
               Species = as.numeric(as.factor(D$class)),
               n = nrow(D), 
               nspp = max(as.numeric(as.factor(D$class))),
               X = X, 
               XG = XG,
               dX = dX)
  str(data)
    length(data$D)
    length(data$N)
    length(data$Species)
    data$n #number of sites (site-season)
    data$nspp #13 spp
    dim(data$X) #6 site-level cov
    dim(data$dX) #1 surv-level cov
  
  # Specify the initial values
  inits = function() {list(Z = as.numeric(data$D>0))}
  
  # Specify the parameters to be monitored
  params = c("rho","pbeta","spbeta","sigpbeta","mbeta","sigbeta","sbeta",
             "gbeta",
             "psi.mean","sigma.occ","p.mean","sigma.p","alpha","Z","P")
  
  nc = 3       # number of chains
  ni = 60000   # number of iterations
  nb = 10000   # burn-in period
  nthin = 50   # thinning rate
  
  
## Run model -------------------------------------------------------------------  
  
  tic()
  out3 <- jags.parallel(data = data, 
               inits = inits, 
               parameters.to.save = params, 
               model.file = 'ms_occ/Gaynor_2020/gaynor-animcons/gaynor-animcons-occupancy-model-groups-jags.txt',
               n.chains = nc, 
               n.iter = ni,
               n.burnin = nb, 
               n.thin = nthin)
  toc()
  
  saveRDS(out3, 'ms_occ/outputs/target_sp_4_guilds.RDS')
  
  
  
  