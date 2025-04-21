## Marginal plots for occupancy models

library(unmarked)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)


#### PREP ----------------------------------------------------------------------

## Source function 'occPred' I wrote for easy predictions from package 'unmarked'
  source('scripts/11a_occ_mod_functions.R')

## Read in model results
  mod_results <- readRDS('output/11_occ_models/all_model_results.RDS')

## Read in covariates
  covariates <- readRDS('output/11_occ_models/covars_for_plotting.RDS') #forgot to add bo_prop_wk here but just do 'bo_days_wk_grid / 7'

## Read in covariate means
  covar_means <- read.csv('./output/11_occ_models/covar_means.csv')

  
#### MAKE PLOTS ----------------------------------------------------------------
  
  ## PLOT EFFECTS FOR TOP MODELS:
      #F AND M: MOD08
      #ANY: MOD09
    

## Model 08 (for female/male) #### ---------------------------------------------

(mod08_formula <- mod_results$input_any$model08@formula)
#p ~ dist + site + noise + log(effort) + bo_total_wk
#psi ~ dist_actual + site_year

  ## Predict p ~ distance and psi ~ distance
  preds_08_dist <- NULL
  for (group in c('input_any','input_female','input_male')){
    preds_08_dist_group <- occPred(mod_results[[group]]$model08,
                                   nickname = group,
                                   new_data = expand_grid('dist' = c(1500, covar_means[covar_means$covar == 'dist',]$value, covariates$dist_grid),
                                                          'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                          'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                          'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                          'site' = covariates$site_grid, #for p ... double check site = site_year
                                                          'dist_actual' = c(1500, covar_means[covar_means$covar == 'dist',]$value, covariates$dist_grid),  #psi
                                                          'site_year' = covariates$site_grid, #psi
                                                          'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value) #psi
    )
    preds_08_dist <- rbind(preds_08_dist, preds_08_dist_group)
  }
  
  #keep only where site = site_year and dist = dist_actual (covar has different names for p and psi but don't need combos)
  preds_08_dist <- preds_08_dist[preds_08_dist$site == preds_08_dist$site_year &
                                   preds_08_dist$dist == preds_08_dist$dist_actual,]
  
  #clean up group names
  preds_08_dist$group <- gsub('input_', '', preds_08_dist$group)
  
  #remove male CC
  preds_08_dist <- preds_08_dist[!(preds_08_dist$group == 'male' & preds_08_dist$site_year == 'CC_2022'),]

  
  ## Change site names
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'BC_2022', 'BC', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'CC_2022', 'CC', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'DC_2021', 'DC', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'DC_2022', 'DC2', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'LM_2022', 'LM', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'MC_2021', 'MC', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'UG_2021', 'UG', preds_08_dist$site)
  preds_08_dist$site <- ifelse(preds_08_dist$site == 'WC_2021', 'WC', preds_08_dist$site)
  
  
  ## Plot p ~ distance  --------------------------------------------------------
  
  #all sites
  plot08_p_dist_mf <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'det' &
                                                          preds_08_dist$group != 'any',], 
                                   aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    # ylim(0,1) +
    ylab('Probability of detection (\u00B1 95 CI)') +
    xlab('Distance (km)') +
    # ggtitle(paste('p ', as.character(mod08_formula)[2], sep = '')) +
    facet_wrap(vars(site), nrow = 2) + 
    theme_bw() + theme(legend.position = 'inside',
                       legend.background = element_rect('transparent'),
                       legend.justification = c(0.99,0.95),
                       strip.text = element_text(size = 12),
                       axis.text.x = element_text(size = 12),
                       axis.text.y = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12),
                       legend.title = element_blank())
  plot08_p_dist_mf
  
  #faceted by site and m/f
  # plot08_p_dist_mf_facet <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'det' &
  #                                                   preds_08_dist$group != 'any',], 
  #                            aes(x = dist/1000, y = value, color = group, fill = group)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
  #   scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
  #   scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
  #   # ylim(0,1) +
  #   ylab('Probability of detection (\u00B1 95 CI)') +
  #   xlab('Distance (km)') +
  #   ggtitle(paste('p ', as.character(mod08_formula)[2], sep = '')) +
  #   facet_grid(cols = vars(group), rows = vars(site)) + 
  #   theme_bw() + theme(legend.position = 'none',
  #                      strip.text = element_text(size = 12))
  # plot08_p_dist_mf_facet
  
  #now for one site only (MC_2021)
  #facet
  plot08_p_dist_mf_mc21a <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'det' &
                                                                   preds_08_dist$group != 'any' &
                                                                   preds_08_dist$site == 'MC',], 
                                            aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    ylab('Probability of detection (\u00B1 95 CI)') +
    xlab('Distance (km)') + 
    ggtitle(paste('MC_2021\np ', as.character(mod08_formula)[2], sep = '')) +
    facet_grid(cols = vars(group)) +
    theme_bw() + theme(legend.position = 'none',
                       strip.text = element_text(size = 12),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12))
  plot08_p_dist_mf_mc21a
  
  #together
  plot08_p_dist_mf_mc21b <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'det' &
                                                                   preds_08_dist$group != 'any' &
                                                                   preds_08_dist$site == 'MC_2021',], 
                                            aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    ylab('Probability of detection (\u00B1 95 CI)') +
    xlab('Distance (km)') + 
    ggtitle(paste('MC_2021\n','p ', as.character(mod08_formula)[2], sep = '')) +
    theme_bw() + theme(legend.position = 'inside',
                       legend.background = element_rect(fill = 'transparent'),
                       legend.title = element_blank(),
                       legend.justification = c(0.9, 0.9),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12))
  plot08_p_dist_mf_mc21b
  
  #save
  plot08_p_dist_mf
    ggsave('plots/mod08_p_dist_mf.png', units="in", width=10, height=8, dpi=600, bg = 'transparent')
  plot08_p_dist_mf_mc21a
    ggsave('plots/mod09_p_dist_mf_mc21_facet.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
  plot08_p_dist_mf_mc21b
    ggsave('plots/mod09_p_dist_mf_mc21.png', units="in", width = 5, height = 5, dpi = 600, bg = 'transparent')
  
  
  ## Plot psi ~ distance -------------------------------------------------------
  
  #all sites/groups
  plot08_psi_dist_mf <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'occ' &
                                                      preds_08_dist$group != 'any',], 
                               aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    # ylim(0,1) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance (km)') +
    facet_wrap(vars(site), nrow = 2) + 
    theme_bw() + theme(legend.position = 'inside',
                       legend.background = element_rect('transparent'),
                       legend.justification = c(0.99,0.95),
                       strip.text = element_text(size = 12),
                       axis.text.x = element_text(size = 12),
                       axis.text.y = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12),
                       legend.title = element_blank())
  plot08_psi_dist_mf
  
  # #faceted by site and m/f
  # plot08_psi_dist_mf_facet <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'occ' &
  #                                                     preds_08_dist$group != 'any',], 
  #                              aes(x = dist/1000, y = value, color = group, fill = group)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
  #   # ylim(0,1) +
  #   ylab('Probability of use (\u00B1 95 CI)') +
  #   xlab('Distance (km)') +
  #   ggtitle(paste('psi ~', as.character(mod08_formula)[3], sep = '')) +
  #   facet_grid(cols = vars(group), rows = vars(site)) + 
  #   theme_bw() + theme(legend.position = 'none',
  #                      strip.text = element_text(size = 12))
  # plot08_psi_dist_mf_facet
  
  #now for one site only (MC_2021)
  #facet
  plot08_psi_dist_mf_mc21a <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'occ' &
                                                                     preds_08_dist$group != 'any' &
                                                                     preds_08_dist$site == 'MC_2021',], 
                                              aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    # ylim(0,1) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance (km)') +
    ggtitle(paste('MC_2021', '\npsi ~', as.character(mod08_formula)[3], sep = '')) +
    facet_grid(cols = vars(group)) +
    theme_bw() + theme(legend.position = 'none',
                       strip.text = element_text(size = 12),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12))
  plot08_psi_dist_mf_mc21a
  
  #together
  plot08_psi_dist_mf_mc21b <- ggplot(data = preds_08_dist[preds_08_dist$variable == 'occ' &
                                                                      preds_08_dist$group != 'any' &
                                                                      preds_08_dist$site == 'MC_2021',], 
                                               aes(x = dist/1000, y = value, color = group, fill = group, linetype = group)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
    scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
    # ylim(0,1) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance (km)') +
    ggtitle(paste('MC_2021', '\npsi ~', as.character(mod08_formula)[3], sep = '')) +
    theme_bw() + theme(legend.position = 'inside',
                       legend.background = element_rect(fill = 'transparent'),
                       legend.title = element_blank(),
                       legend.justification = c(0.9, 0.9),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12))
  plot08_psi_dist_mf_mc21b  

  
  #save
  plot08_psi_dist_mf
    ggsave('plots/mod08_psi_dist_mf.png', units="in", width=10, height=8, dpi=600, bg = 'transparent')
  plot08_psi_dist_mf_mc21a
    ggsave('plots/mod09_psi_dist_mf_mc21_facet.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
  plot08_psi_dist_mf_mc21b
    ggsave('plots/mod09_psi_dist_mf_mc21_together.png', units="in", width = 5, height = 5, dpi = 600, bg = 'transparent')
  
    
  #plot p and psi together
  p_psi_dist_mf <- ((plot08_p_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1))) | 
                   (plot08_psi_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1)))) + 
    plot_annotation(title = 'MC_2021 (model 08)')
  
  p_psi_dist_mf
    ggsave('plots/mod08_p_psi_dist_mf_mc21.png', units = 'in', width = 10, height = 5, dpi = 600, bg = 'transparent')

    
  ## Predict p ~ noise ---------------------------------------------------------
    
    preds_08_noise <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_08_noise_group <- occPred(mod_results[[group]]$model08, 
                                      nickname = group,
                                      new_data = expand_grid('dist' = c(0,1000,1500,covar_means[covar_means$covar == 'dist',]$value,2000,3000), #a few values
                                                             'noise' = covariates$noise_grid,
                                                             'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                             'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                             'site' = covariates$site_grid, #for p ... double check site = site_year
                                                             'dist_actual' = c(0,1000,covar_means[covar_means$covar == 'dist',]$value,2000,3000),  #psi
                                                             'site_year' = covariates$site_grid, #psi
                                                             ) 
      )
      preds_08_noise <- rbind(preds_08_noise, preds_08_noise_group)
    }
    
    ## Predict p ~ effort --------------------------------------------------------
    preds_08_effort <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_08_effort_group <- occPred(mod_results[[group]]$model08, 
                                       nickname = group,
                                       new_data = expand_grid('dist' = c(0,1000,1500,covar_means[covar_means$covar == 'dist',]$value,2000,3000), #a few values (instad of mean)
                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                              'effort' = covariates$effort_grid,
                                                              'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                              'site' = covariates$site_grid, #for p ... double check site = site_year
                                                              'dist_actual' = c(0,1000,covar_means[covar_means$covar == 'dist',]$value,2000,3000),  #psi
                                                              'site_year' = covariates$site_grid, #psi
                                                              )
      )
      preds_08_effort <- rbind(preds_08_effort, preds_08_effort_group)
    }
    
    ## Predict p ~ barred owl  ---------------------------------------------------
    preds_08_bo_p <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_08_bo_p_group <- occPred(mod_results[[group]]$model08, 
                                     nickname = group,
                                     new_data = expand_grid('dist' = c(0,1000,1500,covar_means[covar_means$covar == 'dist',]$value,2000,3000), #a few values (instad of mean)
                                                            'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                            'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                            'bo_total_wk' = covariates$bo_total_wk_grid, 
                                                            'site' = covariates$site_grid, #for p ... double check site = site_year
                                                            'dist_actual' = c(0,1000,covar_means[covar_means$covar == 'dist',]$value,2000,3000),  #psi
                                                            'site_year' = covariates$site_grid #psi
                                                            ) #psi
      )
      preds_08_bo_p <- rbind(preds_08_bo_p, preds_08_bo_p_group)
    }
    
    #keep only where site = site_year and where dist = dist_actual
    preds_08_noise <- preds_08_noise[preds_08_noise$site == preds_08_noise$site_year &
                                       preds_08_noise$dist == preds_08_noise$dist_actual,]
    preds_08_effort <- preds_08_effort[preds_08_effort$site == preds_08_effort$site_year &
                                         preds_08_effort$dist == preds_08_effort$dist_actual,]
    preds_08_bo_p <- preds_08_bo_p[preds_08_bo_p$site == preds_08_bo_p$site_year &
                                     preds_08_bo_p$dist == preds_08_bo_p$dist_actual,]
    
    #clean up group names
    preds_08_noise$group <- gsub('input_', '', preds_08_noise$group)
    preds_08_effort$group <- gsub('input_', '', preds_08_effort$group)
    preds_08_bo_p$group <- gsub('input_', '', preds_08_bo_p$group)
    
    #remove male CC
    preds_08_noise <- preds_08_noise[!(preds_08_noise$group == 'male' & preds_08_noise$site_year == 'CC_2022'),]
    preds_08_effort <- preds_08_effort[!(preds_08_effort$group == 'male' & preds_08_effort$site_year == 'CC_2022'),]
    preds_08_bo_p <- preds_08_bo_p[!(preds_08_bo_p$group == 'male' & preds_08_bo_p$site_year == 'CC_2022'),]
    
    ## Change site names
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'BC_2022', 'BC', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'CC_2022', 'CC', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'DC_2021', 'DC', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'DC_2022', 'DC2', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'LM_2022', 'LM', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'MC_2021', 'MC', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'UG_2021', 'UG', preds_08_noise$site)
    preds_08_noise$site <- ifelse(preds_08_noise$site == 'WC_2021', 'WC', preds_08_noise$site)
    
  ## Plot p ~ noise  --------------------------------------------------------
    
    #all sites -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_noise_mf_mean <- ggplot(data = preds_08_noise[preds_08_noise$variable == 'det' &
                                                           preds_08_noise$group != 'any' &
                                                           preds_08_noise$dist == dd,], 
                                   aes(x = noise, y = value, color = group, fill = group, linetype = group)) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.5, color = NA) +
      geom_line(lwd = 0.8) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      ylim(0,0.6) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      # ggtitle(paste('Dist = ', round(dd), ' m \n', 'p ', as.character(mod08_formula)[2], sep = '')) +
      facet_wrap(vars(site), nrow = 2) + 
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect('transparent'),
                         legend.justification = c(0.99,0.95),
                         strip.text = element_text(size = 12),
                         axis.text.x = element_text(size = 12, angle = 45, vjust = 0.6),
                         axis.text.y = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12),
                         legend.title = element_blank())
    plot08_p_noise_mf_mean
    
    plot08_p_noise_mf_mean
    ggsave('plots/mod08_p_noise_mf_new040325.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    

    
  ###  
    
    #now for one site only (MC_2021) -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_noise_mf_mc21_mean <- ggplot(data = preds_08_noise[preds_08_noise$variable == 'det' &
                                                                preds_08_noise$group != 'any' &
                                                                preds_08_noise$dist == dd &
                                                                preds_08_noise$site == 'MC_2021',], 
                                        aes(x = noise, y = value, color = group, fill = group, linetype = group)) +
      geom_line(lwd = 0.8) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', round(dd), ' m \n', 'p ', as.character(mod08_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_blank(),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot08_p_noise_mf_mc21_mean

    # #all distances together (facet)
    # plot08_p_noise_mf_mc21_alldist_a <- ggplot(data = preds_08_noise[preds_08_noise$variable == 'det' &
    #                                                                     preds_08_noise$group != 'any' &
    #                                                                     preds_08_noise$site == 'MC_2021',], 
    #                                             aes(x = noise, y = value, color = group, fill = group, linetype = group)) +
    #   geom_line() +
    #   geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #   # ylim(0,1) +
    #   ylab('Probability of detection (\u00B1 95 CI)') +
    #   xlab('Noise (dBFS)') +
    #   ggtitle(paste('MC_2021\n', 'p ', as.character(mod08_formula)[2], sep = '')) +
    #   facet_grid(~dist) +
    #   theme_bw() + theme(#legend.position = 'none',
    #                      strip.text = element_text(size = 12))
    # plot08_p_noise_mf_mc21_alldist_a
    # 
    # #two distances together (colors)
    # plot08_p_noise_mf_mc21_alldist_b <- ggplot(data = preds_08_noise[preds_08_noise$variable == 'det' &
    #                                                                     preds_08_noise$group == 'any' &
    #                                                                     preds_08_noise$site == 'MC_2021' &
    #                                                                     preds_08_noise$dist %in% c(0,3000),], #maybe only 0km and 3km? v busy otherwise with overlapping CI
    #                                             aes(x = noise, y = value, group = as.factor(dist/1000))) +
    #   geom_line(aes(color = as.factor(dist/1000), linetype = as.factor(dist/1000)), lwd = 0.8) +
    #   geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = as.factor(dist/1000)), alpha = 0.3, color = NA) +
    #   scale_color_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
    #   scale_fill_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
    #   scale_linetype_manual(values = c('0' = 'dashed', '3' = 'solid'), name = 'Distance (km)') +
    #   # ylim(0,1) +
    #   ylab('Probability of detection (\u00B1 95 CI)') +
    #   xlab('Noise (dBFS)') +
    #   ggtitle(paste('MC_2021\n', 'p ', as.character(mod08_formula)[2], sep = '')) +
    #   # facet_grid(~dist) +
    #   theme_bw() + theme(legend.position = 'inside',
    #                      legend.background = element_rect(fill = 'transparent'),
    #                      legend.title = element_text(size = 12, face = 'bold'),
    #                      legend.justification = c(0.9, 0.9),
    #                      axis.text = element_text(size = 12),
    #                      axis.title = element_text(size = 14),
    #                      legend.text = element_text(size = 12))
    # plot08_p_noise_mf_mc21_alldist_b
    
    
    #save
    plot08_p_noise_mf_mc21_mean
      ggsave('plots/mod09_p_noise_mf_mc21.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    # plot08_p_noise_mf_mc21_alldist_a
    #   ggsave('plots/mod09_p_noise_mf_mc21_panel.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    # plot08_p_noise_mf_mc21_alldist_b
    #   ggsave('plots/mod09_p_noise_mf_mc21_0_3km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    

  ## Plot p ~ effort  ----------------------------------------------------------
    
    ## Change site names
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'BC_2022', 'BC', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'CC_2022', 'CC', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'DC_2021', 'DC', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'DC_2022', 'DC2', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'LM_2022', 'LM', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'MC_2021', 'MC', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'UG_2021', 'UG', preds_08_effort$site)
    preds_08_effort$site <- ifelse(preds_08_effort$site == 'WC_2021', 'WC', preds_08_effort$site)  
      
    #all sites -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_effort_mf_mean <- ggplot(data = preds_08_effort[preds_08_effort$variable == 'det' &
                                                               preds_08_effort$group != 'any' &
                                                               preds_08_effort$dist == dd,], 
                                     aes(x = effort/60, y = value, color = group, fill = group, linetype = group)) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.5, color = NA) +
      geom_line(lwd = 0.8) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      ylim(0,0.6) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Effort (min/week)') +
      # ggtitle(paste('Dist = ', round(dd), ' m \n', 'p ', as.character(mod08_formula)[2], sep = '')) +
      facet_wrap(vars(site), nrow = 2) + 
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect('transparent'),
                         legend.justification = c(0.99,0.95),
                         strip.text = element_text(size = 12),
                         axis.text.x = element_text(size = 12, angle = 45, vjust = 0.6),
                         axis.text.y = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12),
                         legend.title = element_blank())
    plot08_p_effort_mf_mean
    
    plot08_p_effort_mf_mean
    ggsave('plots/mod08_p_effort_mf_new040325.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
      
    
    ## COMBINE NOISE AND EFFORT PLOTS ----------------------------------------   
    
    plot08_p_effort_noise_mf_mean <- (plot08_p_noise_mf_mean) / (plot08_p_effort_mf_mean) + plot_annotation(tag_levels = 'A')
    plot08_p_effort_noise_mf_mean
    
    ggsave('plots/mod08_p_effort_noise_new040325.png', units="in", width=10, height=10, dpi=600, bg = 'transparent')
    
    ##
    
    #now for one site only (MC_2021) -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_effort_mf_mc21_mean <- ggplot(data = preds_08_effort[preds_08_effort$variable == 'det' &
                                                                    preds_08_effort$group != 'any' &
                                                                    preds_08_effort$dist == dd &
                                                                    preds_08_effort$site == 'MC_2021',], 
                                          aes(x = effort/60, y = value, color = group, fill = group, linetype = group)) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      geom_line(lwd = 0.8) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Effort (min/week)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', round(dd), ' m \n', 'p ', as.character(mod08_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_blank(),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot08_p_effort_mf_mc21_mean
      
    #save
    plot08_p_effort_mf_mean
      ggsave('plots/mod08_p_effort_mf.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot08_p_effort_mf_mc21_mean
      ggsave('plots/mod09_p_effort_mf_mc21.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
      
  ## Plot p ~ barred owl -------------------------------------------------------
    
    #all sites -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_bo_mf_mean <- ggplot(data = preds_08_bo_p[preds_08_bo_p$variable == 'det' &
                                                          preds_08_bo_p$group != 'any' &
                                                          preds_08_bo_p$dist == dd,], 
                                      aes(x = bo_total_wk, y = value, color = group, fill = group, linetype = group)) +
      geom_line(lwd = 0.8) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      ylim(0,0.6) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/week)') +
      # ggtitle(paste('Dist = ', round(dd), ' m \n', 'p ', as.character(mod08_formula)[2], sep = '')) +
      facet_wrap(vars(site), nrow = 2) + 
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect('transparent'),
                         legend.justification = c(0.99,0.95),
                         strip.text = element_text(size = 12),
                         axis.text.x = element_text(size = 12, angle = 45, vjust = 0.6),
                         axis.text.y = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12),
                         legend.title = element_blank())
    plot08_p_bo_mf_mean
  
    # #all distances together (facet)
    # plot08_p_bo_mf_mc21_alldist_a <- ggplot(data = preds_08_bo_p[preds_08_bo_p$variable == 'det' &
    #                                                                 preds_08_bo_p$group != 'any' &
    #                                                                 preds_08_bo_p$site == 'MC_2021',], 
    #                                             aes(x = bo_total_wk, y = value, color = group, fill = group, linetype = group)) +
    #   geom_line() +
    #   geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #   # ylim(0,1) +
    #   ylab('Probability of detection (\u00B1 95 CI)') +
    #   xlab('Barred owls (calls/wk)') +
    #   ggtitle(paste('MC_2021\n', 'p ', as.character(mod08_formula)[2], sep = '')) +
    #   facet_grid(~dist) +
    #   theme_bw() + theme(#legend.position = 'none',
    #     strip.text = element_text(size = 12))
    # plot08_p_bo_mf_mc21_alldist_a
    # 
    # #two distances together (colors)
    # plot08_p_bo_mf_mc21_alldist_b <- ggplot(data = preds_08_bo_p[preds_08_bo_p$variable == 'det' &
    #                                                                 preds_08_bo_p$group == 'any' &
    #                                                                 preds_08_bo_p$site == 'MC_2021' &
    #                                                                 preds_08_bo_p$dist %in% c(0,3000),], #maybe only 0km and 3km? v busy otherwise with overlapping CI
    #                                             aes(x = bo_total_wk, y = value, group = as.factor(dist/1000))) +
    #   geom_line(aes(color = as.factor(dist/1000), linetype = as.factor(dist/1000)), lwd = 0.8) +
    #   geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = as.factor(dist/1000)), alpha = 0.3, color = NA) +
    #   scale_color_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
    #   scale_fill_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
    #   scale_linetype_manual(values = c('0' = 'dashed', '3' = 'solid'), name = 'Distance (km)') +
    #   # ylim(0,1) +
    #   ylab('Probability of detection (\u00B1 95 CI)') +
    #   xlab('Barred owls (calls/wk)') +
    #   ggtitle(paste('MC_2021\n', 'p ', as.character(mod08_formula)[2], sep = '')) +
    #   # facet_grid(~dist) +
    #   theme_bw() + theme(legend.position = 'inside',
    #                      legend.background = element_rect(fill = 'transparent'),
    #                      legend.title = element_text(size = 12, face = 'bold'),
    #                      legend.justification = c(0.9, 0.9),
    #                      axis.text = element_text(size = 12),
    #                      axis.title = element_text(size = 14),
    #                      legend.text = element_text(size = 12))
    # plot08_p_bo_mf_mc21_alldist_b
    
    #now for one site only (MC_2021) -- mean distance
    dd = covar_means[covar_means$covar == 'dist',]$value
    plot08_p_bo_mf_mc21_mean <- ggplot(data = preds_08_bo_p[preds_08_bo_p$variable == 'det' &
                                                              preds_08_bo_p$group != 'any' &
                                                              preds_08_bo_p$dist == dd &
                                                              preds_08_bo_p$site == 'MC_2021',], 
                                       aes(x = bo_total_wk, y = value, color = group, fill = group, linetype = group)) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.4, color = NA) +
      geom_line(lwd = 0.8) +
      scale_color_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_fill_manual(values = c('male' = '#7fadf4', 'female' = '#d3886b')) +
      scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid')) +
      # scale_color_manual(values = c('male' = '#0072B2', 'female' = '#E69F00'), name = 'Distance (km)') +
      # scale_fill_manual(values = c('male' = '#0072B2', 'female' = '#E69F00'), name = 'Distance (km)') +
      # scale_linetype_manual(values = c('male' = 'dashed', 'female' = 'solid'), name = 'Distance (km)') +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owl detections (calls/wk)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', round(dd), ' m', 
                    # '\np ', as.character(mod08_formula)[2], 
                    sep = '')) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_blank(),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot08_p_bo_mf_mc21_mean
    
    
      #save
      plot08_p_bo_mf_mean
        ggsave('plots/mod08_p_bo_mf.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
      # plot08_p_bo_mf_mc21_alldist_a
      #   ggsave('plots/mod09_p_bo_mf_mc21_panel.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
      # plot08_p_bo_mf_mc21_alldist_b
      #   ggsave('plots/mod09_p_bo_mf_mc21_0_3km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')

      plot08_p_bo_mf_mc21_mean
        ggsave('plots/mod09_p_bo_mf_mc21.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
        
        
  ## Plot p~dist, psi~dist, and p~BO together --------------------------------
    
    #layout 1 
    p_psi_dist_BO_a <- ((plot08_psi_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1)) + theme(legend.position = 'none')) + plot_spacer())/ 
                        ((plot08_p_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1)) + theme(legend.position = 'none')) +
                          (plot08_p_bo_mf_mc21_mean + labs(title = NULL) + ylim(c(0,1)))) +
                            plot_annotation(title = 'MC_2021')
    p_psi_dist_BO_a
    ggsave('plots/mod08_mf_panel_mc21a.png', units = 'in', width = 10, height = 5, dpi = 600, bg = 'transparent')
      
    #layout 2
    p_psi_dist_BO_b <- (plot08_psi_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1)) + theme(legend.position = 'none') +
                           annotate('text', label = 'italic(\u03C8)', parse = TRUE, x = 2.75, y = 0.95, size = 6)) +
                            (plot08_p_dist_mf_mc21b + labs(title = NULL) + ylim(c(0,1)) + theme(legend.position = 'none') +
                             annotate('text', label = 'italic(p)', parse = TRUE, x = 2.75, y = 0.95, size = 6)) +
                                (plot08_p_bo_mf_mc21_mean + labs(title = NULL) + ylim(c(0,1)) +
                                annotate('text', label = 'italic(p)', parse = TRUE, x = 550, y = 0.95, size = 6) +
                                theme(legend.justification = c(0.8, 0.6))) +
                        plot_annotation(title = 'MC_2021', tag_levels = 'A')
    p_psi_dist_BO_b
    ggsave('plots/mod08_mf_panel_mc21b.png', units = 'in', width = 10, height = 5, dpi = 600, bg = 'transparent')
    
      

## Model 09 (any) #### ---------------------------------------------------------
    
    (mod09_formula <- mod_results$input_any$model09@formula)
    #p ~ dist + site + noise + log(effort) + bo_total_wk
    #psi ~ dist_actual + site_year + bo_total
    
    ## Predict p ~ distance and psi ~ distance
    
    #these covariate grids get huge, so hold all other covariates to one or a few values
    preds_09_dist <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_09_dist_group <- occPred(mod_results[[group]]$model09, 
                                     nickname = group,
                                     new_data = expand_grid('dist' = covariates$dist_grid,
                                                            'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                            'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                            'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                            'site' = covariates$site_grid, #for p ... double check site = site_year
                                                            'dist_actual' = covariates$dist_grid,  #psi
                                                            'site_year' = covariates$site_grid, #psi
                                                            'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value) #psi
      )
      preds_09_dist <- rbind(preds_09_dist, preds_09_dist_group)
    }
    
    #keep only where site = site_year (covar has different names for p and psi but don't need combos)
    #and where dist = dist_actual
    preds_09_dist <- preds_09_dist[preds_09_dist$site == preds_09_dist$site_year &
                                     preds_09_dist$dist == preds_09_dist$dist_actual,]
    
    
    ## Plot p ~ distance  --------------------------------------------------------
    
    #all sites/groups
    plot09_p_dist_withSite <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'det',], 
                                     aes(x = dist/1000, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle(paste('p ', as.character(mod09_formula)[2], sep = '')) +
      geom_rug(data = preds_09_dist %>% filter(group == .data$group), #doesn't work to filter by group... need dataframe with female/male yes/no for each ARU 
               aes(x = dist/1000), inherit.aes = FALSE) +
      # geom_rug(data = data.frame('dist' = covariates$dist_grid), 
      #          aes(x = dist/1000), inherit.aes = FALSE) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_dist_withSite
    
    #now for 'any' only
    plot09_p_dist_withSite_any <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'det' &
                                                                preds_09_dist$group == 'input_any',], 
                                         aes(x = dist/1000, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle(paste('p ', as.character(mod09_formula)[2], sep = '')) +
      geom_rug(data = preds_09_dist %>% filter(group == .data$group), #doesn't work to filter by group... need dataframe with female/male yes/no for each ARU 
               aes(x = dist/1000), inherit.aes = FALSE) +
      # geom_rug(data = data.frame('dist' = covariates$dist_grid), 
      #          aes(x = dist/1000), inherit.aes = FALSE) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_dist_withSite_any
    
    #now for one site only (MC_2021)
    plot09_p_dist_withSite_any_mc21 <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'det' &
                                                                     preds_09_dist$group == 'input_any' &
                                                                     preds_09_dist$site == 'MC_2021',], 
                                              aes(x = dist/1000, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Distance (km)') + 
      ggtitle(paste('p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_dist_withSite_any_mc21
    
    #save
    plot09_p_dist_withSite
    ggsave('plots/mod09_p_dist_all.png', units="in", width=10, height=8, dpi=600, bg = 'transparent')
    plot09_p_dist_withSite_any
    ggsave('plots/mod09_p_dist_any.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_dist_withSite_any_mc21
    ggsave('plots/mod09_p_dist_any_mc21.png', units="in", width = 5, height = 5, dpi = 600, bg = 'transparent')
    
    
    ## Plot psi ~ distance -------------------------------------------------------
    
    #all sites/groups
    plot09_psi_dist_withSite <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'occ',], 
                                       aes(x = dist/1000, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle(paste('psi ~', as.character(mod09_formula)[3], sep = '')) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_dist_withSite
    
    #now for 'any' only
    plot09_psi_dist_withSite_any <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'occ' &
                                                                  preds_09_dist$group == 'input_any',], 
                                           aes(x = dist/1000, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle(paste('psi ~', as.character(mod09_formula)[3], sep = '')) +
      geom_rug(data = preds_09_dist %>% filter(group == .data$group), #doesn't work to filter by group... need dataframe with female/male yes/no for each ARU 
               aes(x = dist/1000), inherit.aes = FALSE) +
      # geom_rug(data = data.frame('dist' = covariates$dist_grid), 
      #          aes(x = dist/1000), inherit.aes = FALSE) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_dist_withSite_any
    
    #now for one site only (MC_2021)
    plot09_psi_dist_withSite_any_mc21 <- ggplot(data = preds_09_dist[preds_09_dist$variable == 'occ' &
                                                                       preds_09_dist$group == 'input_any' &
                                                                       preds_09_dist$site == 'MC_2021',], 
                                                aes(x = dist/1000, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle(paste('MC_2021', '\npsi ~', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_dist_withSite_any_mc21
    
    #save
    plot09_psi_dist_withSite
    ggsave('plots/mod09_psi_dist_all.png', units="in", width=10, height=8, dpi=600, bg = 'transparent')
    plot09_psi_dist_withSite_any
    ggsave('plots/mod09_psi_dist_any.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_psi_dist_withSite_any_mc21
    ggsave('plots/mod09_psi_dist_any_mc21.png', units="in", width = 5, height = 5, dpi = 600, bg = 'transparent')
    
    #plot p and psi together
    p_psi_dist <- ((plot09_p_dist_withSite_any_mc21 + labs(title = NULL) + ylim(c(0,1))) | 
                     (plot09_psi_dist_withSite_any_mc21 + labs(title = NULL))) + plot_annotation(title = 'MC_2021')
    p_psi_dist
    ggsave('plots/mod09_p_psi_dist_any_mc21.png', units = 'in', width = 10, height = 5, dpi = 600, bg = 'transparent')
    
    
    
    ## Predict p ~ noise ---------------------------------------------------------
    
    preds_09_noise <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_09_noise_group <- occPred(mod_results[[group]]$model09, 
                                      nickname = group,
                                      new_data = expand_grid('dist' = c(0,1000,2000,3000), #a few values (instad of mean)
                                                             'noise' = covariates$noise_grid,
                                                             'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                             'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                             'site' = covariates$site_grid, #for p ... double check site = site_year
                                                             'dist_actual' = c(0,1000,2000,3000),  #psi
                                                             'site_year' = covariates$site_grid, #psi
                                                             'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value) #psi
      )
      preds_09_noise <- rbind(preds_09_noise, preds_09_noise_group)
    }
    
    ## Predict p ~ effort --------------------------------------------------------
    preds_09_effort <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_09_effort_group <- occPred(mod_results[[group]]$model09, 
                                       nickname = group,
                                       new_data = expand_grid('dist' = c(0,1000,2000,3000), #a few values (instad of mean)
                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                              'effort' = covariates$effort_grid,
                                                              'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                              'site' = covariates$site_grid, #for p ... double check site = site_year
                                                              'dist_actual' = c(0,1000,2000,3000),  #psi
                                                              'site_year' = covariates$site_grid, #psi
                                                              'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value) #psi
      )
      preds_09_effort <- rbind(preds_09_effort, preds_09_effort_group)
    }
    
    ## Predict p ~ barred owl  ---------------------------------------------------
    preds_09_bo_p <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_09_bo_p_group <- occPred(mod_results[[group]]$model09, 
                                     nickname = group,
                                     new_data = expand_grid('dist' = c(0,1000,2000,3000), #a few values (instad of mean)
                                                            'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                            'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                            'bo_total_wk' = covariates$bo_total_wk_grid, 
                                                            'site' = covariates$site_grid, #for p ... double check site = site_year
                                                            'dist_actual' = c(0,1000,2000,3000),  #psi
                                                            'site_year' = covariates$site_grid, #psi
                                                            'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value) #psi
      )
      preds_09_bo_p <- rbind(preds_09_bo_p, preds_09_bo_p_group)
    }
    
    ## Predict psi ~ barred owl  -------------------------------------------------
    preds_09_bo_psi <- NULL
    for (group in c('input_any','input_female','input_male')){
      preds_09_bo_psi_group <- occPred(mod_results[[group]]$model09, 
                                       nickname = group,
                                       new_data = expand_grid('dist' = c(0,1000,2000,3000), #a few values (instad of mean)
                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
                                                              'effort' = covar_means[covar_means$covar == 'effort',]$value,
                                                              'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                                                              'site' = covariates$site_grid, #for p ... double check site = site_year
                                                              'dist_actual' = c(0,1000,2000,3000),  #psi
                                                              'site_year' = covariates$site_grid, #psi
                                                              'bo_total' = covariates$bo_total_site_grid) #psi
      )
      preds_09_bo_psi <- rbind(preds_09_bo_psi, preds_09_bo_psi_group)
    }
    
    #keep only where site = site_year and where dist = dist_actual
    preds_09_noise <- preds_09_noise[preds_09_noise$site == preds_09_noise$site_year &
                                       preds_09_noise$dist == preds_09_noise$dist_actual,]
    preds_09_effort <- preds_09_effort[preds_09_effort$site == preds_09_effort$site_year &
                                         preds_09_effort$dist == preds_09_effort$dist_actual,]
    preds_09_bo_p <- preds_09_bo_p[preds_09_bo_p$site == preds_09_bo_p$site_year &
                                     preds_09_bo_p$dist == preds_09_bo_p$dist_actual,]
    preds_09_bo_psi <- preds_09_bo_psi[preds_09_bo_psi$site == preds_09_bo_psi$site_year &
                                         preds_09_bo_psi$dist == preds_09_bo_psi$dist_actual,]
    
    
    ## Plot p ~ noise  --------------------------------------------------------
    
    #all sites/groups (dist = 0 km) *add rug?*
    dd = 0
    plot09_p_noise_0 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                       preds_09_noise$dist == dd,], 
                               aes(x = noise, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_0
    
    #1km
    dd = 1000
    plot09_p_noise_1 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                       preds_09_noise$dist == dd,], 
                               aes(x = noise, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_1
    
    #2km
    dd = 2000
    plot09_p_noise_2 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                       preds_09_noise$dist == dd,], 
                               aes(x = noise, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_2
    
    #3km
    dd = 3000
    plot09_p_noise_3 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                       preds_09_noise$dist == dd,], 
                               aes(x = noise, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(group), rows = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_3
    
    #now for 'any' only
    #0km
    dd=0
    plot09_p_noise_any_0 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                           preds_09_noise$group == 'input_any' &
                                                           preds_09_noise$dist == dd,], 
                                   aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_0
    
    #1km
    dd=1000
    plot09_p_noise_any_1 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                           preds_09_noise$group == 'input_any' &
                                                           preds_09_noise$dist == dd,], 
                                   aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_1
    
    #2km
    dd=2000
    plot09_p_noise_any_2 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                           preds_09_noise$group == 'input_any' &
                                                           preds_09_noise$dist == dd,], 
                                   aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_2
    
    #3km
    dd=3000
    plot09_p_noise_any_3 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                           preds_09_noise$group == 'input_any' &
                                                           preds_09_noise$dist == dd,], 
                                   aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_3
    
    
    #now for one site only (MC_2021)
    #0km
    dd=0
    plot09_p_noise_any_mc21_0 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                preds_09_noise$group == 'input_any' &
                                                                preds_09_noise$dist == dd &
                                                                preds_09_noise$site == 'MC_2021',], 
                                        aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_mc21_0
    
    #1km
    dd=1000
    plot09_p_noise_any_mc21_1 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                preds_09_noise$group == 'input_any' &
                                                                preds_09_noise$dist == dd &
                                                                preds_09_noise$site == 'MC_2021',], 
                                        aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_mc21_1
    
    #2km
    dd=2000
    plot09_p_noise_any_mc21_2 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                preds_09_noise$group == 'input_any' &
                                                                preds_09_noise$dist == dd &
                                                                preds_09_noise$site == 'MC_2021',], 
                                        aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_mc21_2
    
    #3km
    dd=3000
    plot09_p_noise_any_mc21_3 <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                preds_09_noise$group == 'input_any' &
                                                                preds_09_noise$dist == dd &
                                                                preds_09_noise$site == 'MC_2021',], 
                                        aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_mc21_3
    
    #all distances together (facet)
    plot09_p_noise_any_mc21_alldist_a <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                        preds_09_noise$group == 'input_any' &
                                                                        preds_09_noise$site == 'MC_2021',], 
                                                aes(x = noise, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(~dist) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_noise_any_mc21_alldist_a
    
    #all distances together (colors)
    plot09_p_noise_any_mc21_alldist_b <- ggplot(data = preds_09_noise[preds_09_noise$variable == 'det' &
                                                                        preds_09_noise$group == 'input_any' &
                                                                        preds_09_noise$site == 'MC_2021' &
                                                                        preds_09_noise$dist %in% c(0,3000),], #maybe only 0km and 3km? v busy otherwise with overlapping CI
                                                aes(x = noise, y = value, group = as.factor(dist/1000))) +
      geom_line(aes(color = as.factor(dist/1000), linetype = as.factor(dist/1000)), lwd = 0.8) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = as.factor(dist/1000)), alpha = 0.3, color = NA) +
      scale_color_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_fill_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_linetype_manual(values = c('0' = 'dashed', '3' = 'solid'), name = 'Distance (km)') +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Noise (dBFS)') +
      ggtitle(paste('MC_2021\n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      # facet_grid(~dist) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_text(size = 12, face = 'bold'),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot09_p_noise_any_mc21_alldist_b
    
    
    #save
    #don't really need the ones with any/female/male
    plot09_p_noise_any_0
    ggsave('plots/mod09_p_noise_any_0km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_1
    ggsave('plots/mod09_p_noise_any_1km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_2
    ggsave('plots/mod09_p_noise_any_2km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_3
    ggsave('plots/mod09_p_noise_any_3km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    
    plot09_p_noise_any_mc21_0
    ggsave('plots/mod09_p_noise_any_mc21_0km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_mc21_1
    ggsave('plots/mod09_p_noise_any_mc21_1km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_mc21_2
    ggsave('plots/mod09_p_noise_any_mc21_2km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_noise_any_mc21_3
    ggsave('plots/mod09_p_noise_any_mc21_3km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    plot09_p_noise_any_mc21_alldist_b
    ggsave('plots/mod09_p_noise_any_mc21_allKm.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    
    ## Plot p ~ effort (*HOW DO I DEAL WITH LOG? DO I PREDICT ON LOG(EFFORT) AND/OR PLOT LOG(EFORT) ON X AXIS?*) -------------------------------
    
    #for 'any' only
    #0km
    dd=0
    plot09_p_effort_any_0 <- ggplot(data = preds_09_effort[preds_09_effort$variable == 'det' &
                                                             preds_09_effort$group == 'input_any' &
                                                             preds_09_effort$dist == dd,], 
                                    aes(x = effort, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Effort (min)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_effort_any_0
    
    #1km
    #2km
    #3km
    
    
    #now for one site only (MC_2021)
    
    #1km
    #
    #
    
    
    #save
    
    
    
    ## Plot p ~ bo ---------------------------------------------------------------
    
    #for 'any' only
    #0km
    dd=0
    plot09_p_bo_any_0 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                       preds_09_bo_p$group == 'input_any' &
                                                       preds_09_bo_p$dist == dd,], 
                                aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_0
    
    #1km
    dd=1000
    plot09_p_bo_any_1 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                       preds_09_bo_p$group == 'input_any' &
                                                       preds_09_bo_p$dist == dd,], 
                                aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_1
    
    #2km
    dd=2000
    plot09_p_bo_any_2 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                       preds_09_bo_p$group == 'input_any' &
                                                       preds_09_bo_p$dist == dd,], 
                                aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_2
    
    #3km
    dd=3000
    plot09_p_bo_any_3 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                       preds_09_bo_p$group == 'input_any' &
                                                       preds_09_bo_p$dist == dd,], 
                                aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_3
    
    
    #now for one site only (MC_2021)
    #0km
    dd=0
    plot09_p_bo_any_mc21_0 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                            preds_09_bo_p$group == 'input_any' &
                                                            preds_09_bo_p$dist == dd &
                                                            preds_09_bo_p$site == 'MC_2021',], 
                                     aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('MC_2021\n','Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_mc21_0   
    
    #1km
    dd=1000
    plot09_p_bo_any_mc21_1 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                            preds_09_bo_p$group == 'input_any' &
                                                            preds_09_bo_p$dist == dd &
                                                            preds_09_bo_p$site == 'MC_2021',], 
                                     aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('MC_2021\n','Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_mc21_1
    
    #2km
    dd=2000
    plot09_p_bo_any_mc21_2 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                            preds_09_bo_p$group == 'input_any' &
                                                            preds_09_bo_p$dist == dd &
                                                            preds_09_bo_p$site == 'MC_2021',], 
                                     aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('MC_2021\n','Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_mc21_2
    
    #3km
    dd=3000
    plot09_p_bo_any_mc21_3 <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                            preds_09_bo_p$group == 'input_any' &
                                                            preds_09_bo_p$dist == dd &
                                                            preds_09_bo_p$site == 'MC_2021',], 
                                     aes(x = bo_total_wk, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('MC_2021\n','Dist = ', dd, ' m \n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_p_bo_any_mc21_3
    
    #all distances together
    plot09_p_bo_any_mc21_alldist <- ggplot(data = preds_09_bo_p[preds_09_bo_p$variable == 'det' &
                                                                  preds_09_bo_p$group == 'input_any' &
                                                                  preds_09_bo_p$dist %in% c(0,3000) &
                                                                  preds_09_bo_p$site == 'MC_2021',], 
                                           aes(x = bo_total_wk, y = value, group = as.factor(dist/1000))) +
      geom_line(aes(color = as.factor(dist/1000), linetype = as.factor(dist/1000)), lwd = 0.8) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = as.factor(dist/1000)), alpha = 0.3, color = NA) +
      scale_color_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_fill_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_linetype_manual(values = c('0' = 'dashed', '3' = 'solid'), name = 'Distance (km)') +
      # ylim(0,1) +
      ylab('Probability of detection (\u00B1 95 CI)') +
      xlab('Barred owls (calls/wk)') +
      ggtitle(paste('MC_2021\n', 'p ', as.character(mod09_formula)[2], sep = '')) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_text(size = 12, face = 'bold'),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot09_p_bo_any_mc21_alldist
    
    
    #save
    plot09_p_bo_any_0
    ggsave('plots/mod09_p_bo_any_0km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_1
    ggsave('plots/mod09_p_bo_any_1km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_2
    ggsave('plots/mod09_p_bo_any_2km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_3
    ggsave('plots/mod09_p_bo_any_3km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    
    plot09_p_bo_any_mc21_0
    ggsave('plots/mod09_p_bo_any_mc21_0km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_mc21_1
    ggsave('plots/mod09_p_bo_any_mc21_1km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_mc21_2
    ggsave('plots/mod09_p_bo_any_mc21_2km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_p_bo_any_mc21_3
    ggsave('plots/mod09_p_bo_any_mc21_3km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    plot09_p_bo_any_mc21_alldist
    ggsave('plots/mod09_p_bo_any_mc21_allKm.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    
    
    ## Plot psi ~ bo -------------------------------------------------------------
    
    #for 'any' only
    #0km
    dd=0
    plot09_psi_bo_any_0 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                           preds_09_bo_psi$group == 'input_any' &
                                                           preds_09_bo_psi$dist == dd,], 
                                  aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_0   
    
    #1km
    dd=1000
    plot09_psi_bo_any_1 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                           preds_09_bo_psi$group == 'input_any' &
                                                           preds_09_bo_psi$dist == dd,], 
                                  aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_1
    
    #2km
    dd=2000
    plot09_psi_bo_any_2 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                           preds_09_bo_psi$group == 'input_any' &
                                                           preds_09_bo_psi$dist == dd,], 
                                  aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_2 
    
    #3km
    dd=3000
    plot09_psi_bo_any_3 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                           preds_09_bo_psi$group == 'input_any' &
                                                           preds_09_bo_psi$dist == dd,], 
                                  aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      facet_grid(cols = vars(site)) + 
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_3
    
    
    #for one site only (MC_2021)
    #0km
    dd=0
    plot09_psi_bo_any_mc21_0 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                                preds_09_bo_psi$group == 'input_any' &
                                                                preds_09_bo_psi$dist == dd &
                                                                preds_09_bo_psi$site == 'MC_2021',], 
                                       aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_mc21_0 
    
    #1km
    dd=1000
    plot09_psi_bo_any_mc21_1 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                                preds_09_bo_psi$group == 'input_any' &
                                                                preds_09_bo_psi$dist == dd &
                                                                preds_09_bo_psi$site == 'MC_2021',], 
                                       aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_mc21_1 
    
    #2km
    dd=2000
    plot09_psi_bo_any_mc21_2 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                                preds_09_bo_psi$group == 'input_any' &
                                                                preds_09_bo_psi$dist == dd &
                                                                preds_09_bo_psi$site == 'MC_2021',], 
                                       aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_mc21_2 
    
    #3km
    dd=3000
    plot09_psi_bo_any_mc21_3 <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                                preds_09_bo_psi$group == 'input_any' &
                                                                preds_09_bo_psi$dist == dd &
                                                                preds_09_bo_psi$site == 'MC_2021',], 
                                       aes(x = bo_total, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('MC_2021\n', 'Dist = ', dd, ' m \n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'none',
                         strip.text = element_text(size = 12))
    plot09_psi_bo_any_mc21_3
    
    #for all dist combined
    plot09_psi_bo_any_mc21_alldist <- ggplot(data = preds_09_bo_psi[preds_09_bo_psi$variable == 'occ' &
                                                                      preds_09_bo_psi$group == 'input_any' &
                                                                      preds_09_bo_psi$dist %in% c(0,3000) &
                                                                      preds_09_bo_psi$site == 'MC_2021',], 
                                             aes(x = bo_total, y = value, group = as.factor(dist/1000))) +
      geom_line(aes(color = as.factor(dist/1000), linetype = as.factor(dist/1000)), lwd = 0.8) +
      geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = as.factor(dist/1000)), alpha = 0.3, color = NA) +
      scale_color_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_fill_manual(values = c('0' = '#0072B2', '3' = '#E69F00'), name = 'Distance (km)') +
      scale_linetype_manual(values = c('0' = 'dashed', '3' = 'solid'), name = 'Distance (km)') +
      # ylim(0,1) +
      ylab('Probability of use (\u00B1 95 CI)') +
      xlab('Barred owls (total calls)') +
      ggtitle(paste('MC_2021\n', 'psi ~ ', as.character(mod09_formula)[3], sep = '')) +
      theme_bw() + theme(legend.position = 'inside',
                         legend.background = element_rect(fill = 'transparent'),
                         legend.title = element_text(size = 12, face = 'bold'),
                         legend.justification = c(0.9, 0.9),
                         axis.text = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
    plot09_psi_bo_any_mc21_alldist
    
    
    #save
    plot09_psi_bo_any_0
    ggsave('plots/mod09_psi_bo_any_0km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_1
    ggsave('plots/mod09_psi_bo_any_1km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_2
    ggsave('plots/mod09_psi_bo_any_2km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_3
    ggsave('plots/mod09_psi_bo_any_3km.png', units="in", width=10, height=5, dpi=600, bg = 'transparent')
    
    plot09_psi_bo_any_mc21_0
    ggsave('plots/mod09_psi_bo_any_mc21_0km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_mc21_1
    ggsave('plots/mod09_psi_bo_any_mc21_1km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_mc21_2
    ggsave('plots/mod09_psi_bo_any_mc21_2km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    plot09_psi_bo_any_mc21_3
    ggsave('plots/mod09_psi_bo_any_mc21_3km.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    plot09_psi_bo_any_mc21_alldist
    ggsave('plots/mod09_psi_bo_any_mc21_allKm.png', units="in", width=5, height=5, dpi=600, bg = 'transparent')
    
    
    
      ##

    
    ## Model 09 (no site) #### -----------------------------------------------------
    # (mod09_noSiteP_formula <- mod_results$input_any$model09_noSiteP@formula)
    # 
    # ## Predict p ~ distance and psi ~ distance
    # 
    #   #these covariate grids get huge, so hold all other covariates to one or a few values
    #   pred_09_p_dist <- NULL
    #   for (group in c('input_any','input_female','input_male')){
    #     pred_09_p_dist_group <- occPred(mod_results[[group]]$model09_noSiteP, nickname = group,
    #                                       new_data = expand_grid('dist' = covariates$dist_grid,
    #                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
    #                                                              'effort' = covar_means[covar_means$covar == 'effort',]$value,
    #                                                              'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
    #                                                          'dist_actual' = covariates$dist_grid, 
    #                                                          'site_year' = covariates$site_grid,
    #                                                          'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value)
    #                                     )
    #     pred_09_p_dist <- rbind(pred_09_p_dist, pred_09_p_dist_group)
    #   }
    #   
    #   #plot p ~ distance
    #   plot09_p_dist <- ggplot(data = pred_09_p_dist[pred_09_p_dist$dist == pred_09_p_dist$dist_actual &
    #                                                      pred_09_p_dist$variable == 'det',], 
    #                            aes(x = dist/1000, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     # ylim(0,1) +
    #     # ylab('Probability of detection (\u00B1 95 CI)') +
    #     xlab('Distance (km)') +
    #     ggtitle(paste('model09_noSiteP',
    #                   paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     geom_rug(data = pred_09_p_dist %>% filter(group == .data$group), #doesn't work to filter by group... need dataframe with female/male yes/no for each ARU 
    #              aes(x = dist/1000), inherit.aes = FALSE) +
    #     # geom_rug(data = data.frame('dist' = covariates$dist_grid), 
    #     #          aes(x = dist/1000), inherit.aes = FALSE) +
    #     facet_grid(cols = vars(group)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #                        strip.text = element_text(size = 12))
    #   
    #   #plot psi ~ distance (can use same predictions dataframe as above)
    #   plot09_psi_dist <- ggplot(data = pred_09_p_dist[pred_09_p_dist$dist == pred_09_p_dist$dist_actual &
    #                                                     pred_09_p_dist$variable == 'occ',], 
    #                            aes(x = dist/1000, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     ylim(0,1) +
    #     ylab('Probability of use (\u00B1 95 CI)') +
    #     xlab('Distance (km)') +
    #     ggtitle(paste('model09_noSiteP',
    #                   # paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     geom_rug(data = pred_09_p_dist %>% filter(group == .data$group), #doesn't work to filter by group... need dataframe with female/male yes/no for each ARU 
    #              aes(x = dist/1000), inherit.aes = FALSE) +
    #     # geom_rug(data = data.frame('dist' = covariates$dist_grid), 
    #     #          aes(x = dist/1000), inherit.aes = FALSE) +
    #     facet_grid(cols = vars(group), rows = vars(site_year)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #       strip.text = element_text(size = 12))
    #   
    # #View and save  
    #   plot09_p_dist
    #   ggsave('plots/mod09_noSiteP_p_dist.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    #   
    #   plot09_psi_dist
    #   ggsave('plots/mod09_noSiteP_psi_dist.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    #   
    # 
    #   
    # ## Predict psi ~ barred_owl_proportion
    #   
    #   #these covariate grids get huge, so hold all other covariates to one or a few values
    #   pred_09_psi_bo <- NULL
    #   for (group in c('input_any','input_female','input_male')){
    #     pred_09_psi_bo_group <- occPred(mod_results[[group]]$model09_noSiteP, nickname = group,
    #                                       new_data = expand_grid('dist' = c(0,1000,2000,3000),
    #                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
    #                                                              'effort' = covar_means[covar_means$covar == 'effort',]$value,
    #                                                              'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
    #                                                              'dist_actual' = c(0,1000,2000,3000), 
    #                                                              'site_year' = covariates$site_grid,
    #                                                              'bo_total' = (covariates$bo_total_site_grid))
    #                                     )
    #     pred_09_psi_bo <- rbind(pred_09_psi_bo, pred_09_psi_bo_group)
    #   }
    #   
    #   #plot psi ~ bo_total
    #   dist = 0 #0 km
    #   plot09_psi_bo_0km <- ggplot(data = pred_09_psi_bo[pred_09_psi_bo$dist == pred_09_psi_bo$dist_actual &
    #                                                       pred_09_psi_bo$variable == 'occ' &
    #                                                       pred_09_psi_bo$dist == dist,], 
    #                                aes(x = bo_total, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     ylim(0,1) +
    #     ylab('Probability of use (\u00B1 95 CI)') +
    #     xlab('Total number of barred owl detections') +
    #     ggtitle(paste(paste('model09_noSiteP (distance = ', dist/1000, ' km)', sep = ''),
    #                   # paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     facet_grid(cols = vars(group), rows = vars(site_year)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #                        strip.text = element_text(size = 12))
    #   
    #   dist = 2000 #2 km
    #   plot09_psi_bo_2km <- ggplot(data = pred_09_psi_bo[pred_09_psi_bo$dist == pred_09_psi_bo$dist_actual &
    #                                                       pred_09_psi_bo$variable == 'occ' &
    #                                                       pred_09_psi_bo$dist == dist,], 
    #                               aes(x = bo_total, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     ylim(0,1) +
    #     ylab('Probability of use (\u00B1 95 CI)') +
    #     xlab('Total number of barred owl detections') +
    #     ggtitle(paste(paste('model09_noSiteP (distance = ', dist/1000, ' km)', sep = ''),
    #                   # paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     facet_grid(cols = vars(group), rows = vars(site_year)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #                        strip.text = element_text(size = 12))
    #   
    # #View and save  
    #   plot09_psi_bo_0km
    #   ggsave('plots/mod09_psi_bo_0km.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    #   
    #   plot09_psi_bo_2km
    #   ggsave('plots/mod09_psi_bo_2km.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    #   
    #   
    # ## Predict p ~ barred_owl_proportion
    #   
    #   #these covariate grids get huge, so hold all other covariates to one or a few values
    #   pred_09_p_bo <- NULL
    #   for (group in c('input_any','input_female','input_male')){
    #     pred_09_p_bo_group <- occPred(mod_results[[group]]$model09_noSiteP, nickname = group,
    #                                       new_data = expand_grid('dist' = c(0,1000,2000,3000),
    #                                                              'noise' = covar_means[covar_means$covar == 'noise',]$value,
    #                                                              'effort' = covar_means[covar_means$covar == 'effort',]$value,
    #                                                              'bo_total_wk' = (covariates$bo_total_wk_grid), 
    #                                                              'dist_actual' = c(0,1000,2000,3000), 
    #                                                              'site_year' = covariates$site_grid,
    #                                                              'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value)
    #     )
    #     pred_09_p_bo <- rbind(pred_09_p_bo, pred_09_p_bo_group)
    #   }
    #   
    #   #plot p ~ bo_proportion
    #   dist = 0 #0 km
    #   plot09_p_bo_0km <- ggplot(data = pred_09_p_bo[pred_09_p_bo$dist == pred_09_p_bo$dist_actual &
    #                                                   pred_09_p_bo$variable == 'det' &
    #                                                   pred_09_p_bo$dist == dist,], 
    #                                aes(x = bo_total_wk, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     ylim(0,1) +
    #     ylab('Probability of detection (\u00B1 95 CI)') +
    #     xlab('Total number of owl detections per week') +
    #     ggtitle(paste(paste('model09_noSiteP (distance = ', dist/1000, ' km)', sep = ''),
    #                   paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     facet_grid(cols = vars(group)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #                        strip.text = element_text(size = 12))
    #   
    #   dist = 2000 #2km
    #   plot09_p_bo_2km <- ggplot(data = pred_09_p_bo[pred_09_p_bo$dist == pred_09_p_bo$dist_actual &
    #                                                   pred_09_p_bo$variable == 'det' &
    #                                                   pred_09_p_bo$dist == dist,], 
    #                             aes(x = bo_total_wk, y = value, color = group, fill = group)) +
    #     geom_line() +
    #     geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    #     ylim(0,1) +
    #     ylab('Probability of detection (\u00B1 95 CI)') +
    #     xlab('Total number of owl detections per week') +
    #     ggtitle(paste(paste('model09_noSiteP (distance = ', dist/1000, ' km)', sep = ''),
    #                   paste('p ', as.character(mod09_noSiteP_formula)[2], '\npsi ~', as.character(mod09_noSiteP_formula)[3], sep = ''), 
    #                   sep = '\n')) +
    #     facet_grid(cols = vars(group)) + 
    #     theme_bw() + theme(legend.position = 'none',
    #                        strip.text = element_text(size = 12))
    #   
    # #View and save
    #   plot09_p_bo_0km
    #   ggsave('plots/mod09_p_bo_0km.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    #   
    #   plot09_p_bo_2km    
    #   ggsave('plots/mod09_p_bo_2km.png', 
    #          units="in", width=10, height=8, dpi=600, bg = 'transparent')
    
    
          
