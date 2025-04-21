## Single-season (stacked) occupancy models -- making predictions / marginal plots

library(data.table)
library(unmarked)
library(tidyverse)


## Read in model inputs and outputs --------------------------------------------

  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  final_models <- readRDS('ss_occ/model_objects_final.RDS')
  
  #Source function 'occPred' I wrote for easy predictions from package 'unmarked'
  source('ss_occ/occ_mod_functions.R')
  
  
## Format covariates for plotting ----------------------------------------------
  
  ## SITE-LEVEL
    site_covars_summary <- fread('data/cleaned/covariates/covar_summmaries.csv')
    (site_covar_names <- site_covars_summary$variable[!grepl('5x5|Afromontane|Water|Dambo|Rock|Cleared|fence', site_covars_summary$variable, ignore.case = FALSE)])
    site_covars_summary <- site_covars_summary %>% filter(variable %in% site_covar_names)
    
    #generate values to predict on (select 50 values between min-max, including the mean)
    n = 50
    predict_values <- list()
    for(aa in unique(site_covars_summary$variable)){
      covar_aa <- site_covars_summary[site_covars_summary$variable == aa,]
      predict_values[[aa]] <- sort(c(covar_aa$mean, seq(covar_aa$min, covar_aa$ max, length = n-1)))
    }
    lapply(predict_values, length)
    
    
  ## SURVEY-LEVEL 
    predict_values[['effort']] <- seq(0, 7)
    predict_values[['season']] <- factor(c('wet','cool','dry'), levels = c('wet','cool','dry'))
    
    
  ## STRUCTURAL
    predict_values[['releases']] <- c('Before','After')
    predict_values[['period']] <- c('Fence','NoFence')
    predict_values[['fence']] <- c(0,1)
    
    #save
    saveRDS(predict_values, 'data/cleaned/covariates/covars_for_plotting.RDS')
  
  
  
## Create new data grids -------------------------------------------------------
  
  ## Actually I don't think I can really do these ahead of time.....
  
  new_data_detection <- expand_grid('effort' = predict_values$effort,
                                    'season' = predict_values$season)
  
  new_data_occupancy <- expand_grid('')  
    
    
    new_data = expand_grid('dist' = c(1500, covar_means[covar_means$covar == 'dist',]$value, covariates$dist_grid),
                           'noise' = covar_means[covar_means$covar == 'noise',]$value,
                           'effort' = covar_means[covar_means$covar == 'effort',]$value,
                           'bo_total_wk' = covar_means[covar_means$covar == 'bo_total_wk',]$value, 
                           'site' = covariates$site_grid, #for p ... double check site = site_year
                           'dist_actual' = c(1500, covar_means[covar_means$covar == 'dist',]$value, covariates$dist_grid),  #psi
                           'site_year' = covariates$site_grid, #psi
                           'bo_total' = covar_means[covar_means$covar == 'bo_total_site',]$value)
  
  
## Predict from top model for each species -------------------------------------
    
  names(final_models)
    
  ### Buffalo -------------------------------------------------------------------  
  
  #choose top model  
  names(final_models$buffalo)
  top_mod_buffalo <- final_models$buffalo$`~effort + season ~ elev_sd_3x3 + pct_slope_sd_3x3 + releases + fence * period`
    
  #predict
  preds_buffalo <- occPred(top_mod_buffalo, nickname = 'buffalo', 
                           new_data = expand_grid('effort' = predict_values$effort,
                                                  'season' = predict_values$season,
                                                  'elev_sd_3x3' = predict_values$elev_sd_3x3,
                                                  'pct_slope_sd_3x3' = predict_values$pct_slope_sd_3x3,
                                                  'releases' = predict_values$releases,
                                                  'fence' = predict_values$fence,
                                                  'period' = predict_values$period))
  
  #order factor levels for plotting
  preds_buffalo$season <- factor(preds_buffalo$season, levels = c('wet','cool','dry'))
  preds_buffalo$releases <- factor(preds_buffalo$releases, levels = c('Before','After'))
  preds_buffalo$period <- factor(preds_buffalo$period, levels = c('Fence','NoFence'))
  preds_buffalo$fence2 <- factor(ifelse(preds_buffalo$fence == 0, 'North', 'South'), levels = c('South','North'))
  
  #plot p~EFFORT
  plot_buffalo_p_effort <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'det',], 
                           aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
                       legend.position = 'none',
                       legend.background = element_rect('transparent'),
                       legend.justification = c(0.99,0.95),
                       strip.text = element_text(size = 12),
                       axis.text.x = element_text(size = 12),
                       axis.text.y = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12),
                       legend.title = element_blank())
  plot_buffalo_p_effort
  
  #plot p~SEASON
  plot_buffalo_p_season <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'det' & 
                                                         preds_buffalo$effort == 7,], 
                           aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  plot_buffalo_p_season
  
  #plot psi~elev_sd_3x3
  plot_buffalo_psi_elevSD <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'occ' &
                                                         # preds_buffalo$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3']$mean &
                                                         preds_buffalo$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3']$mean &
                                                         preds_buffalo$releases == 'Before' &
                                                         preds_buffalo$fence == 0 &
                                                         preds_buffalo$period == 'Fence',], 
                                  aes(x = elev_sd_3x3, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation SD (m)') +
    # facet_grid(~effort) +
    # ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  plot_buffalo_psi_elevSD
  
  #plot psi~pct_slope_sd_3x3
  plot_buffalo_psi_slopeSD <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'occ' &
                                                           preds_buffalo$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3']$mean
                                                           # preds_buffalo$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3']$mean &
                                                           # preds_buffalo$releases == 'Before' &
                                                           # preds_buffalo$fence == 0 &
                                                           # preds_buffalo$period == 'Fence'
                                                            ,], 
                                    aes(x = pct_slope_sd_3x3, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Slope SD (%)') +
    facet_grid(~releases ~ fence ~ period) +
    # ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  plot_buffalo_psi_slopeSD
  
  
  ## WHY DON'T I JUST PLOT PSI ~ ENVIR RELATIONSIHPS FROM THOSE TOP MODELS (W/O STRUCTURAL VARIABLES)!
  ## FOCUS ON PLOTTING THE STRUCTURAL AND INTERACTIONS HERE...
  
  #plot psi~RELEASES
  plot_buffalo_psi_releases <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'occ' &
                                                          preds_buffalo$season == 'wet' & preds_buffalo$effort == 7 & #these don't matter here
                                                          preds_buffalo$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3']$mean &
                                                          preds_buffalo$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3']$mean
                                                          # preds_buffalo$releases == 'Before'
                                                          # preds_buffalo$fence == 0 &
                                                          # preds_buffalo$period == 'Fence'
                                                          ,], 
                                     aes(x = releases, y = value, color = season, fill = season)) +
    # geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2)) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Releases') +
    facet_grid(~fence2 ~period) +
    ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  plot_buffalo_psi_releases
  
  #plot psi~PERIOD*FENCE
  plot_buffalo_psi_periodfence <- ggplot(data = preds_buffalo[preds_buffalo$variable == 'occ' &
                                                             preds_buffalo$season == 'wet' & preds_buffalo$effort == 7 & #these don't matter here
                                                             preds_buffalo$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3']$mean &
                                                             preds_buffalo$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3']$mean
                                                           # preds_buffalo$releases == 'Before'
                                                           # preds_buffalo$fence == 0 &
                                                           # preds_buffalo$period == 'Fence'
                                                           ,], 
                                      aes(x = period, y = value, color = as.factor(fence2))) +
    # geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2)) +
    # geom_line() +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Period') +
    facet_grid(~releases) +
    # ylim(0,0.2) +
    ggtitle('Buffalo') +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  
  plot_buffalo_psi_periodfence
  
  #save
  ggsave(paste('ss_occ/figures/single_season/', 'p_season_buffalo', '.png', sep = ''), plot_buffalo_p_season)
  ggsave(paste('ss_occ/figures/single_season/', 'p_effort_buffalo', '.png', sep = ''), plot_buffalo_p_effort)
  
  
  
  
    
  ggsave(paste('ss_occ/figures/single_season/', Sys.Date(), '/detection_fire_', ss, '.png', sep = ''), det_fire_ss)
  
####
  
  #predict occupancy (season)
  predict_occ_season <- cbind(predict(global_mod_season, 
                                      newdata = expand.grid(list(fence = unique(site_covar$fence), season = unique(site_covar$season))), type = 'state'), 
                              expand.grid(list(fence = unique(site_covar$fence), season = unique(site_covar$season))))
  head(predict_occ_season)
  
  (use_season_ss <- ggplot(data = predict_occ_season, aes(x = season, y = Predicted)) +
      geom_pointrange(aes(ymin = lower, ymax = upper), fill = "skyblue") +
      facet_wrap(~fence) +
      ylim(c(0,1)) +
      # stat_smooth(method = "loess", col = "black", se = FALSE) +
      labs(x = "Season", y = "Predicted use", title = ss) +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)))
  
  ggsave(paste('ss_occ/figures/single_season/', Sys.Date(), '/use_season_', ss, '.png', sep = ''), use_season_ss)
  
  #predict occupancy (calendar week)
  predict_occ_week <- cbind(predict(global_mod_week, 
                                    newdata = expand.grid(list(fence = unique(site_covar$fence), calendar_week = unique(site_covar$calendar_week))), type = 'state'), 
                            expand.grid(list(fence = unique(site_covar$fence), calendar_week = unique(site_covar$calendar_week))))
  head(predict_occ_week)
  
  (use_week_ss <- ggplot(data = predict_occ_week, aes(x = calendar_week, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue") +
      stat_smooth(method = "loess", col = "black", se = FALSE) +
      facet_wrap(~fence) +
      ylim(c(0,1)) +
      labs(x = "Calendar week", y = "Predicted occupancy", title = ss) +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)))
  
  ggsave(paste('ss_occ/figures/single_season/', Sys.Date(), '/use_week_', ss, '.png', sep = ''), use_week_ss)
  
  
  
  