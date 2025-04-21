## Single-species, multi-season occ models

library(data.table)
library(unmarked)
library(tidyverse)
library(knitr)
library(rlang)
library(flextable) #to save tables to Word
library(officer) #to save tables to Word


## READ IN DETECTION DATA ------------------------------------------------------

dh_all <- readRDS('data/cleaned/detection_histories_seasonal/dh_seasonal_all.RDS')
  names(dh_all)
  names(dh_all$elephant)
  head(dh_all$elephant$wet)


## READ IN SURVEY COVARIATES ---------------------------------------------------

effort_covar_wet <- fread('data/cleaned/covariates/effort/effort_seasonal_wet.csv')
effort_covar_cool <- fread('data/cleaned/covariates/effort/effort_seasonal_cool.csv')
effort_covar_dry <- fread('data/cleaned/covariates/effort/effort_seasonal_dry.csv')

#combine and format
effort_covar <- list('wet' = effort_covar_wet, 'cool' = effort_covar_cool, 'dry' = effort_covar_dry)

effort_covar <- lapply(effort_covar, function(x) {
                  x %>% arrange(site) %>% 
                        column_to_rownames(var = 'site') %>%
                        select(-V1)
  })


  #SUMMARIZE EFFORT FOR TABLE 1 (with 0s removed)
  #overall
  summary(unlist(effort_covar)[unlist(effort_covar) > 0], na.rm = TRUE) #mean = 6.7
  sd(unlist(effort_covar)[unlist(effort_covar) > 0], na.rm = TRUE) #SD = 1.2
  
  #wet
  summary(unlist(effort_covar$wet)[unlist(effort_covar$wet) > 0], na.rm = TRUE) #mean = 6.8
  sd(unlist(effort_covar$wet)[unlist(effort_covar$wet) > 0], na.rm = TRUE) #SD = 0.7

  #cool
  summary(unlist(effort_covar$cool)[unlist(effort_covar$cool) > 0], na.rm = TRUE) #mean = 6.4
  sd(unlist(effort_covar$cool)[unlist(effort_covar$cool) > 0], na.rm = TRUE) #SD = 1.7
  
  #dry
  summary(unlist(effort_covar$dry)[unlist(effort_covar$dry) > 0], na.rm = TRUE) #mean = 6.6
  sd(unlist(effort_covar$dry)[unlist(effort_covar$dry) > 0], na.rm = TRUE) #SD = 1.2
  

## READ IN SITE COVARIATES -----------------------------------------------------

site_covar <- fread('data/cleaned/covariates/cleaned_covariates.csv')
  names(site_covar)
  
  #which to keep?
  # (covar_names <- names(site_covar)[grepl('Site|3x3|dist|fence', names(site_covar))])
  (covar_names <- c('Site','elev_mean_3x3','elev_sd_3x3','pct_slope_sd_3x3','tree_vol_mean_3x3','tree_vol_sd_3x3',
                    'dist_cleared_mean_3x3','dist_cleared_sd_3x3','dist_cleared_mean_5x5','dist_cleared_sd_5x5',
                    'dist_dambo_mean_3x3','dist_dambo_sd_3x3','dist_river','prop_M1_3x3','prop_M2_3x3','prop_M3_3x3',
                    'fence'))
  head(site_covar[,..covar_names]) #these are NOT standardized (we'll do it in the model specification)

  #keep only these covariates
  site_covar <- site_covar %>% select(all_of(covar_names))    
    length(unique(site_covar$Site)) #all 210 sites here
    
  #keep only sites that have data in det histories
  (sites_keep <- unique(dh_all$elephant$wet$site)) #same for all seasons/species; should be 190
  site_covar <- site_covar %>% filter(Site %in% sites_keep)
    length(unique(site_covar$Site)) #190 now
  
  #use South/North for 'fence' instead of 0/1
  site_covar$fence <- ifelse(site_covar$fence == 0, 'North', 'South')
  
  #make sure it's sorted by site
  site_covar <- site_covar %>% arrange(Site)
  rownames(site_covar) <- site_covar$Site


## LIST FOCAL SPECIES TO REPORT ------------------------------------------------    
  
  names(dh_all)
  focal_species <- c('buffalo','bushbuck','bushpig','duiker','eland','elephant',
                     'grysbok','impala','klipspringer','kudu','reedbuck','roan',
                     'sable','small_antelope','warthog','waterbuck','zebra')
  
  
## CREATE MODEL INPUTS ---------------------------------------------------------
  
  ##DO WE NEED TO REMOVE SITES WITH NO DETECTIONS PER SEASON? OK TO HAVE THOSE FULLY NA ROWS?
  
  #how many years?
  n_years = 5
  
  #create object to store
  occu_inputs <- list()
  naive_occ <- NULL
  naive_occ_yearly <- NULL
  
  #loop thru species
  for (aa in names(dh_all)){
    
    for (bb in c('wet','cool','dry')) {
      
      #extract detection history
      dh_aa <- data.frame(dh_all[[aa]][[bb]])
      rownames(dh_aa) <- dh_aa$site
      
      #calculate naive occupancy
      naive_occ_aa <- dh_aa %>%
        summarise(total_det = sum(across(starts_with('X')), na.rm = TRUE),
                  sites_surveyed = sum(rowSums(!is.na(across(starts_with('X')))) > 0),
                  sites_detected = sum(rowSums(across(starts_with('X')) > 0, na.rm = TRUE) > 0),
                  naive_occ = sites_detected / sites_surveyed) %>%
        mutate(species = aa, season = bb)
      naive_occ <- rbind(naive_occ, naive_occ_aa)
      
      #calculate naive occupancy -- by year
      years <- c('2018','2019','2020','2021','2022','2023')
      naive_occ_year_aa <- map_dfr(years, function(yr) {
        dh_aa %>%
          summarise(year = yr,
                    total_det = sum(across(starts_with(paste0("X", yr))), na.rm = TRUE),
                    sites_surveyed = sum(rowSums(!is.na(across(starts_with(paste0("X", yr))))) > 0),
                    sites_detected = sum(rowSums(across(starts_with(paste0("X", yr)), ~ ifelse(is.na(.), 0, .)) > 0) > 0),
                    naive_occ = sites_detected / sites_surveyed) %>%
          mutate(species = aa, season = bb)
      })
      naive_occ_yearly <- rbind(naive_occ_yearly, naive_occ_year_aa)
      
      
      #keep only 'week' columns
      dh_aa <- dh_aa %>% select(starts_with('X'))
      
      #create yearly site covariate to get diff estimates by year (?)
      year <- matrix(as.factor(rep(1:n_years)), nrow(dh_aa), n_years, byrow = TRUE)
      
      #get effort covar for this season
      effort_bb <- effort_covar[[bb]]
      
      #create input
      occu_input_aa <- unmarkedMultFrame(y = as.matrix(dh_aa),
                                         siteCovs = site_covar,
                                         obsCovs = list(effort = effort_bb),
                                         numPrimary = n_years,
                                         yearlySiteCovs = list(year = year))
      #store
      occu_inputs[[bb]][[aa]] <- occu_input_aa
    }
  }
  #warnings are OK (converting characters to factors)
  
  #inspect
  names(occu_inputs)
  names(occu_inputs$wet)

  #report sites surveyed in each season
  distinct(naive_occ, season, sites_surveyed)
  
  #export naive_occ table for paper (TABLE 2)
  naive_occ_table <- naive_occ %>% 
    select(total_det, naive_occ, species, season) %>%
    filter(species %in% focal_species) %>%
    pivot_wider(names_from = c(season), values_from = c(naive_occ, total_det), names_sort = TRUE) %>%
    #reorder columns:
    select(1,naive_occ_wet,naive_occ_cool,naive_occ_dry,total_det_wet,total_det_cool,total_det_dry) %>% 
    mutate(across(starts_with('naive_occ',), ~ round(.x, 2))) %>%
    flextable() %>% 
    colformat_num(big.mark = '') 

  #print to Word
  doc0 <- read_docx()
  doc0 <- doc0 %>%
    body_add_par('Total detections and naive occupancy ', style = 'heading 1') %>%
    body_add_flextable(naive_occ_table) %>%
    body_add_par(value = '')
  print(doc0, target = paste('dynamic_occ/outputs', 'naive_occ_target_sp.docx', sep = '/'))
  
  #save inputs as R object
  saveRDS(occu_inputs, 'dynamic_occ/dynamic_occu_inputs.RDS')
  
  
## PLOT NAIVE OCC --------------------------------------------------------------
  
  focal_species
  
  season_colors <- c("cool" = "#66c2a5", "dry" = "#fc8d62", "wet" = "#8da0cb")
  
  naive_occ_yearly$season <- factor(naive_occ_yearly$season, levels = c('wet','cool','dry'))

  for(cc in focal_species){
    naive_plot <- ggplot(naive_occ_yearly[naive_occ_yearly$species == cc,], 
                         aes(x = year, y = naive_occ, group = season)) +
      geom_point(aes(color = season, shape = season), 
                 position = position_dodge(width = 0.3),
                 size = 3) +
      geom_line(aes(color = season, linetype = season),
                position = position_dodge(width = 0.3),) +
      scale_color_manual(values = season_colors) +
      ylim(c(0,1)) +
      ggtitle(cc) +
      ylab('Naive occupancy') +
      # facet_grid(~season) +
      theme_bw() + theme(axis.title = element_text(size = 14),
                         axis.text = element_text(size = 12),
                         axis.title.x = element_blank(),
                         legend.position = 'inside',
                         legend.position.inside = c(0.85,0.8),
                         legend.title = element_blank(),
                         legend.text = element_text(size = 12),
                         plot.title = element_text(size = 16, face = 'bold'))
    
    ggsave(paste('dynamic_occ/figures/naive_occ/naive_occ_', cc, '.png', sep = ''), 
           naive_plot, dpi = 300, width = 4, height = 3)
  }
  
  #plot number of sites surveyed each season?
  tmp <- distinct(naive_occ_yearly, season, sites_surveyed, year)
  season_surv <- ggplot(tmp, aes(x = year, y = sites_surveyed, group = season)) +
    # geom_point(aes(color = season, shape = season), 
    #            position = position_dodge(width = 0.3),
    #            size = 3) +
    geom_col(aes(fill = season)) +
    scale_fill_manual(values = season_colors) +
    facet_grid(~season) +
    ylim(c(0,200)) +
    ylab('Cameras active') +
    geom_hline(yintercept = 190, linetype = 'dashed') +
    theme_bw() + theme(axis.title = element_text(size = 14),
                       axis.text = element_text(size = 12),
                       axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(),
                       strip.text = element_text(size = 14),
                       legend.position = 'none')
  
  ggsave(paste('dynamic_occ/figures/surv_per_season', '.png', sep = ''), 
         season_surv, dpi = 300, width = 5, height = 3)
  
  
## TEST ------------------------------------------------------------------------

  #read in if necessary
  occu_inputs <- readRDS('dynamic_occ/dynamic_occu_inputs.RDS')
  
  #need to remove data from cov so we can compare using AIC?
  
  #null model
  null <- colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1,
               pformula = ~ 1, data = occu_inputs$wet$elephant, method="BFGS")
    summary(null)
    plogis(-0.629)
    
    #projected
    traj <- data.frame(smoothed(null)) %>% rownames_to_column(var = 'occ') %>% 
      pivot_longer(cols = starts_with('X'), names_to = 'year')
    
    #SE
    null_boot <- nonparboot(null, B = 100)
    traj_se <- data.frame(null_boot@smoothed.mean.bsse)
    traj_se$occ <- 'occupied' #what do the two rows mean?
    traj_se <- traj_se[1,] %>% pivot_longer(cols = starts_with('X'), names_to = 'year', values_to = 'SE')
    
    #combine
    traj_df <- traj %>% left_join(traj_se, by = c('occ','year'))
    
    #plot  
    ggplot(traj_df[traj_df$occ == 'occupied',], aes(x = year, y = value)) +
      geom_point() +
      geom_line() +
      geom_pointrange(aes(ymin = value - SE, ymax = value + SE)) +
      ylim(c(0,1)) +
      ggtitle('~1 ~1 ~1 ~1') +
      theme_bw()
    
    
  #year model
  yr_mod <- colext(psiformula = ~1,   # First-year occupancy
                   gammaformula = ~ year-1,    # Colonization
                   epsilonformula = ~ year-1,  # Extinction
                   pformula = ~ year-1,        # Detection
                   data = occu_inputs$wet$elephant)
    summary(yr_mod)
  
    #projected
    traj2 <- data.frame(smoothed(yr_mod)) %>% rownames_to_column(var = 'occ') %>% 
      pivot_longer(cols = starts_with('X'), names_to = 'year')
    
    #SE
    year_boot <- nonparboot(yr_mod, B = 100)
    traj2_se <- data.frame(year_boot@smoothed.mean.bsse)
    traj2_se$occ <- 'occupied' #what do the two rows mean?
    traj2_se <- traj2_se[1,] %>% pivot_longer(cols = starts_with('X'), names_to = 'year', values_to = 'SE')
    
    #combine
    traj2_df <- traj2 %>% left_join(traj2_se, by = c('occ','year'))
    
    #plot  
    ggplot(traj2_df[traj2_df$occ == 'occupied',], aes(x = year, y = value)) +
      geom_point() +
      geom_line() +
      geom_pointrange(aes(ymin = value - SE, ymax = value + SE)) +
      ylim(c(0,1)) +
      ggtitle('~1 ~year ~year ~year') +
      theme_bw()
    
    #plot col ext
    ## Prediction
    nd <- data.frame(year=c('1','2','3','4'))
    E.ext <- predict(yr_mod, type='ext', newdata=nd)
    E.col <- predict(yr_mod, type='col', newdata=nd)
    nd <- data.frame(year=c('1','2','3','4'))
    E.det <- predict(yr_mod, type='det', newdata=nd)
    
    op <- par(mfrow=c(3,1), mai=c(0.6, 0.6, 0.1, 0.1))
    
    with(E.ext, {   # Plot for extinction probability
      plot(1:4, E.ext$Predicted, pch=1, xaxt='n', xlab='Year',
           ylab=expression(paste('Extinction probability ( ', epsilon, ' )')),
           ylim=c(0,1), col=4)
      axis(1, at=1:4, labels=nd$year[1:4])
      arrows(1:4, lower, 1:4, upper, code=3, angle=90, length=0.03, col=4)
      points((1:4)-0.1, 1-phi, col=1, lwd = 1, pch=16)
      legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
             cex=0.8)
    })
    with(E.col, {   # Plot for colonization probability
      plot(1:4, E.col$Predicted, pch=1, xaxt='n', xlab='Year',
           ylab=expression(paste('Colonization probability ( ', gamma, ' )')),
           ylim=c(0,1), col=4)
      axis(1, at=1:4, labels=nd$year[1:4])
      arrows(1:4, lower, 1:4, upper, code=3, angle=90, length=0.03, col=4)
      points((1:4)-0.1, gamma, col=1, lwd = 1, pch=16)
      legend(7, 1, c('Parameter', 'Estimate'), col=c(1,4), pch=c(16, 1),
             cex=0.8)
    })
    
    with(E.det, {   # Plot for detection probability: note 10 years
      plot(1:4, E.det$Predicted, pch=1, xaxt='n', xlab='Year',
           ylab=expression(paste('Detection probability ( ', p, ' )')),
           ylim=c(0,1), col=4)
      axis(1, at=1:4, labels=nd$year)
      arrows(1:4, lower, 1:4, upper, code=3, angle=90, length=0.03, col=4)
      points((1:4)-0.1, p, col=1, lwd = 1, pch=16)
      legend(7.5, 1, c('Parameter','Estimate'), col=c(1,4), pch=c(16, 1),
             cex=0.8)
    })
    
    
    
    #n sites occupied (empirical Bayes est)
    re <- unmarked::ranef(yr_mod)
    modes <- colSums(bup(re, stat = 'mode'))
    plot(1:7, modes, xlab = 'Year', ylab = 'Sites occupied', ylim = c(0, 70))
    
            
  ## mod sel
    models <- fitList('null' = null, 'year' = yr_mod)
    ms <- modSel(models)
    ms
    
    as(ms, 'data.frame')

    
    
        
## RUN DETECTION MODELS --------------------------------------------------------
  
  
  
  
  
  
  
  