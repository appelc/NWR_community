## Structure data for MSOM

library(data.table)
library(tidyverse)
library(jagsUI)
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
  sp_table <- fread('ms_occ/inputs/species_table.csv')
  
  #how did I do categorizing group size? (elephant is off the charts)
  grp_size_plot <- ggplot(sp_table[sp_table$class_name != 'elephant' & !is.na(sp_table$kg_high),], 
         aes(x = reorder(class_name, -kg_high), y = kg_high)) +
         geom_pointrange(aes(ymin = kg_low, ymax = kg_high, color = size)) +
         theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            axis.title.x = element_blank())
  
  ggsave('ms_occ/guild_body_size.png', grp_size_plot, height = 4, width = 8)
    
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
  
  #create a few other categories...
    sp_table$guild_6 <- sp_table$guild
    sp_table[sp_table$guild_6 == 'primate',]$guild_6 <- 'other'
    sp_table[sp_table$guild_4 == 'large_browser',]$guild_6 <- 'small_browser'
    sp_table[sp_table$guild_4 == 'large_grazer',]$guild_6 <- 'small_grazer'
    sp_table[sp_table$guild_4 == 'xl_browser',]$guild_6 <- 'large_browser'
    sp_table[sp_table$guild_4 == 'xl_grazer',]$guild_6 <- 'large_grazer'
    
      table(sp_table$guild_6)
    
  #match up
  head(dh_simple)
  head(sp_table)  
  dh_simple$guild <- sp_table$guild[match(dh_simple$class, sp_table$class_name)]
  dh_simple$guild4 <- sp_table$guild_4[match(dh_simple$class, sp_table$class_name)]
  dh_simple$guild6 <- sp_table$guild_6[match(dh_simple$class, sp_table$class_name)]
    distinct(dh_simple, class, guild6)
  
  #remove some species
  dh_simple <- dh_simple[!(dh_simple$class %in% c('bat_sp','blue_monkey','bush_squirrel',
                                                  'domestic_cattle','domestic_dog','goat',
                                                  'human','other_animal','other_bird',
                                                  'small_mammal','scrub_hare')),]
  sp_table_simple <- sp_table[!(sp_table$class_name %in% c('bat_sp','blue_monkey','bush_squirrel',
                                                       'domestic_cattle','domestic_dog','goat',
                                                       'human','other_animal','other_bird',
                                                       'small_mammal','scrub_hare')),]
      
  #save
  write.csv(sp_table_simple, 'ms_occ/inputs/species_table_cleaned.csv')
  
  #save
  saveRDS(dh_simple, 'ms_occ/inputs/det_data_cleaned.RDS')

  
## Add site-level covar --------------------------------------------------------
  
  #read in input files I created for single-species models (stacked design)
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  
  #take site covariates from one species model input  
  site_covs <- occu_inputs$elephant@siteCovs
  
  #QC
  nrow(site_covs) #site cov data: 3040 site-seasons (190 sites * 16 seasons)
  length(unique(dh_simple$class))  #number species: 13
  nrow(dh_simple) #detection data: should be 3040 site-seasons * n_species
  
  #create log versions of dist cov
  site_covs <- site_covs %>% mutate(across(contains('dist'),
                                           ~log(.),
                                           .names = '{.col}_log'))
  
  #standardize/scale all continuous cov
  site_covs_scaled <- site_covs %>% mutate(across(where(is.numeric), 
                                    ~as.numeric(scale(.)),
                                    .names = '{.col}_scaled'))
  
  #make indicators for site/year/season
  site_covs$year <- sapply(strsplit(as.character(site_covs$site_season), '_'), '[', 2)
  site_covs$year_i <- as.numeric(factor(site_covs$year))

  site_covs_scaled$year <- sapply(strsplit(as.character(site_covs_scaled$site_season), '_'), '[', 2)
  site_covs_scaled$year_i <- as.numeric(factor(site_covs_scaled$year))

  site_covs$site <- sapply(strsplit(as.character(site_covs$site_season), '_'), '[', 1)
  site_covs$site_i <- as.numeric(factor(site_covs_scaled$site))
  
  site_covs_scaled$site <- sapply(strsplit(as.character(site_covs_scaled$site_season), '_'), '[', 1)
  site_covs_scaled$site_i <- as.numeric(factor(site_covs_scaled$site))
  
  site_covs$season_i <- as.numeric(factor(site_covs$season))
  site_covs_scaled$season_i <- as.numeric(factor(site_covs_scaled$season))
  
  
  #save
  saveRDS(site_covs, 'ms_occ/inputs/site_data.RDS')
  saveRDS(site_covs_scaled, 'ms_occ/inputs/site_data_scaled.RDS')
  
  
  
  