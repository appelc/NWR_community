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
  
  
  
# Explore model output ----------------------------------------------------
  
  #read back in
  out3 <- readRDS('ms_occ/outputs/target_sp_4_guilds.RDS')
  out3.sum <- out3$BUGSoutput$summary
  
  #read back in species names
  species_data <- fread('ms_occ/inputs/species_table_cleaned.csv')
  
  # Save species names (for interpreting model results later)
  sppnames <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  # sppnames <- as.character(unique(Spp$cla))
  
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
  
  # Define the occupancy covariate effects where mbeta is the community-level hyperparameter, gbeta is the group-level hyperparameter, and sbeta is the species-specific parameter.
  mbeta <- out3$BUGSoutput$sims.list$mbeta
  gbeta <- out3$BUGSoutput$sims.list$gbeta
  sbeta <- out3$BUGSoutput$sims.list$sbeta
  
  # Calculate group-level estimates
  covs <- colnames(X) # define covariates
  sizes <- c('large_grazer','xl_grazer','large_browser','xl_browser')
  group <- data.frame(expand.grid(covs, sizes), matrix(NA, length(covs) * length(sizes), 4)) 
  colnames(group) <- c("Factor", "Group", "Mean", "SD", "LCI", "UCI")
  
  # Create a loop estimating the reference group values
  covs <- c('elev_mean_3x3_scaled','pct_slope_mean_3x3_scaled','tree_vol_mean_3x3_scaled',
            'dist_cleared_mean_3x3_log_scaled','dist_dambo_mean_3x3_log_scaled','dist_river_log_scaled')
  for (a in 1:length(covs)){
    group[a,3:6] <- c(mean(mbeta[,a]),sd(mbeta[,a]),quantile(mbeta[,a],c(0.025,0.975)))
  }
  
  # Create a second loop estimating the other group values
  for (a in 1:length(covs)){
    for (b in 1:(length(sizes)-1)){
      sims <- mbeta[,a] + gbeta[,((b-1)*length(covs)+a)] #replaced 'ncol(X)' with length(cov)
      group[(ncol(X)*(b)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,c(0.025,0.975)))
    }
  }
  
  head(group)
  
  ## PLOT!!
  group_plot <- ggplot(group, aes(x = Factor, y = Mean, color = Group)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 14),
                       axis.text.x = element_text(size = 10, angle = 30, hjust = 1))
  group_plot
  
  ggsave('ms_occ/figures/target_sp_guilds_covar.png', group_plot, width = 6, height = 4)
  
  
  # Species level estimates 
  
  # Define the species
  # spec <- Spp[,1]
  spec <- focal_species #I think this is right?

  head(species_data)
  species_data <- species_data[species_data$class_name %in% focal_species,]
  
  # Define the group levels
  levels(species_data$guild_4) <- levels(as.factor(species_data$guild_4))[c(1,2,3,4)]
  gg <- as.numeric(as.factor(species_data$guild_4))
  
  # Define the occupancy covariates and groups
  # covs <- colnames(X)
  # sizes <- c('xl_browser, large_grazer','xl_grazer','large_browser') #WHICH ORDER?????
  
  # Create a data frame where the number of rows is equal to the number of covariates * the number of species
  species <- data.frame(expand.grid(covs,spec), matrix(NA,length(covs)*length(spec),4))
  colnames(species) <- c("Factor","Species","Mean","SD","LCI","UCI")
  
  # Re-define gbeta
  gbeta <- cbind(gbeta, matrix(0,nrow(gbeta),length(covs)))
  
  # Create a loop that will estimate species-specific values for each of the covariates
  for (a in 1:length(covs)){
    for (b in 1:length(spec)){
      sims <- mbeta[,a] + gbeta[,((gg[b]-1)*length(covs)+a)] + sbeta[,b,a] #replaced ncol(X) with length(covs)
      species[(ncol(X)*(b-1)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,c(0.025,0.975)))
    }
  }
  
  head(species)
  
  ## PLOT!! ##would be good to color by guild and shape by species or something. or facet by guild
  
  #merge guilds/species
  species$guild <- species_data$guild_4[match(species$Species, species_data$class_name)]
  
  species_plot <- ggplot(species, aes(x = Factor, y = Mean, color = Species)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI), position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_grid(~guild) +
    ylab('Beta +/- 95% CI') +
    theme_bw() + theme(axis.text = element_text(size = 14),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 14),
                       axis.text.x = element_text(size = 10, angle = 30, hjust = 1))
  species_plot
  
  ggsave('ms_occ/figures/target_sp_species_covar.png', species_plot, width = 8, height = 4)
  
  
  
  
  # Determine species richness at each site ---------------------------------
  
  # Define the z matrix
  z = out3$BUGSoutput$sims.list$Z
  
  # Sort the data frame based on species, study site, and functional group
  # d <- sort_df(merge(data.frame(ID = 1:nrow(D),D[,1:2]), 
  #                    data.frame(SppCode = spec, 
  #                               Group = species_data$guild_4)),
  #              "ID")[,c(1,3,4)]
  
  #break out the above into steps:
  tmp1 <- merge(data.frame(ID = 1:nrow(D),D[,1:2]), 
                data.frame(SppCode = spec, 
                           Group = species_data$guild_4))
  d <- tmp1 %>% arrange(ID) %>% select(ID, class, SppCode) #are these the col we want?
  
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
  
  
  # Determine group richness at each site -----------------------------------
  
  # Aggregate the data by summing the group-specific values in the z matrix for each StudySite station during each iteration
  z.group <- acast(m.dz,StudySite + Group ~ variable, fun.aggregate = sum)
  
  # Use the aggregated values to create probability distributions representing estimated StudySite-station specific group richness
  z.group <- t(apply(z.group,1,function(x) c(mean(x),sd(x),quantile(x,c(0.025,0.975)))))
  names <- rownames(z.group)
  rownames(z.group) <- NULL
  z.group <- cbind(names,z.group)
  colnames(z.group) = c("StudySite", "Mean","SD","LCI","UCI")
  
  head(z.group)
  
  
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
  
  