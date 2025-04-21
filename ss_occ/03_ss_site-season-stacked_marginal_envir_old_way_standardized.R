## Make predictions / marginal plots (ENVIRONMENTAL COVARIATES ONLY)

library(data.table)
library(unmarked)
library(tidyverse)
library(patchwork)


## Read in model inputs and outputs --------------------------------------------

  occu_inputs <- readRDS('ss_occ/outputs/occu_inputs.RDS')
  p_models <- readRDS('ss_occ/outputs/model_objects_p.RDS')
  envir_models_univar <- readRDS('ss_occ/outputs/model_objects_psi_univariate.RDS')
  envir_models_multivar <- readRDS('ss_occ/outputs/model_objects_psi_multivariate.RDS')
  #final_models <- readRDS('ss_occ/outputs/model_objects_final.RDS')
  
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

  
## Predict (DETECTION) ---------------------------------------------------------
  names(p_models)
  
  #read in if necessary
  # predict_values <- readRDS('data/cleaned/covariates/covars_for_plotting.RDS')
  
  ### Buffalo ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$buffalo)
  top_mod_buffalo_p <- p_models$buffalo$`~effort + season ~ 1`
  
  #predict
  preds_buffalo_p <- occPred(top_mod_buffalo_p, nickname = 'buffalo_p', 
                             new_data = expand_grid('effort' = predict_values$effort,
                                                  'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_buffalo_p_effort <- ggplot(data = preds_buffalo_p[preds_buffalo_p$variable == 'det',], 
                                  aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('buffalo') +
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
  plot_buffalo_p_season <- ggplot(data = preds_buffalo_p[preds_buffalo_p$variable == 'det' & 
                                                           preds_buffalo_p$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('buffalo') +
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

  #save
  ggsave(paste('ss_occ/figures/detection/', 'buffalo_p_effort', '.png', sep = ''), plot_buffalo_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'buffalo_p_season', '.png', sep = ''), plot_buffalo_p_season, width = 6, height = 4, dpi = 300)
  
  
  ### Bushbuck ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$bushbuck)
  top_mod_bushbuck_p <- p_models$bushbuck$`~effort + season ~ 1`
  
  #predict
  preds_bushbuck_p <- occPred(top_mod_bushbuck_p, nickname = 'bushbuck_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_bushbuck_p_effort <- ggplot(data = preds_bushbuck_p[preds_bushbuck_p$variable == 'det',], 
                                  aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('bushbuck') +
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
  plot_bushbuck_p_effort
  
  #plot p~SEASON
  plot_bushbuck_p_season <- ggplot(data = preds_bushbuck_p[preds_bushbuck_p$variable == 'det' & 
                                                           preds_bushbuck_p$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('bushbuck') +
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
  plot_bushbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'bushbuck_p_effort', '.png', sep = ''), plot_bushbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'bushbuck_p_season', '.png', sep = ''), plot_bushbuck_p_season, width = 6, height = 4, dpi = 300)
  
  
  ### Bushpig ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$bushpig)
  top_mod_bushpig_p <- p_models$bushpig$`~effort + season ~ 1`
  
  #predict
  preds_bushpig_p <- occPred(top_mod_bushpig_p, nickname = 'bushpig_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_bushpig_p_effort <- ggplot(data = preds_bushpig_p[preds_bushpig_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('bushpig') +
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
  plot_bushpig_p_effort
  
  #plot p~SEASON
  plot_bushpig_p_season <- ggplot(data = preds_bushpig_p[preds_bushpig_p$variable == 'det' & 
                                                             preds_bushpig_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('bushpig') +
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
  plot_bushpig_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'bushpig_p_effort', '.png', sep = ''), plot_bushpig_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'bushpig_p_season', '.png', sep = ''), plot_bushpig_p_season, width = 6, height = 4, dpi = 300)  
  
  
  ### Eland ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$eland)
  top_mod_eland_p <- p_models$eland$`~effort + season ~ 1`
  
  #predict
  preds_eland_p <- occPred(top_mod_eland_p, nickname = 'eland_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_eland_p_effort <- ggplot(data = preds_eland_p[preds_eland_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('eland') +
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
  plot_eland_p_effort
  
  #plot p~SEASON
  plot_eland_p_season <- ggplot(data = preds_eland_p[preds_eland_p$variable == 'det' & 
                                                             preds_eland_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('eland') +
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
  plot_eland_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'eland_p_effort', '.png', sep = ''), plot_eland_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'eland_p_season', '.png', sep = ''), plot_eland_p_season, width = 6, height = 4, dpi = 300)  
  
  
  ### Elephant -----------------------------------------------------------------
  
  #which one was top model?
  names(p_models$elephant)
  top_mod_elephant_p <- p_models$elephant$`~effort + season ~ 1`
  
  #predict
  preds_elephant_p <- occPred(top_mod_elephant_p, nickname = 'elephant_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_elephant_p_effort <- ggplot(data = preds_elephant_p[preds_elephant_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('elephant') +
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
  plot_elephant_p_effort
  
  #plot p~SEASON
  plot_elephant_p_season <- ggplot(data = preds_elephant_p[preds_elephant_p$variable == 'det' & 
                                                             preds_elephant_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('elephant') +
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
  plot_elephant_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'elephant_p_effort', '.png', sep = ''), plot_elephant_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'elephant_p_season', '.png', sep = ''), plot_elephant_p_season, width = 6, height = 4, dpi = 300)  
  
  
  ### Impala -------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$impala)
  top_mod_impala_p <- p_models$impala$`~effort + season ~ 1`
  
  #predict
  preds_impala_p <- occPred(top_mod_impala_p, nickname = 'impala_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_impala_p_effort <- ggplot(data = preds_impala_p[preds_impala_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('impala') +
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
  plot_impala_p_effort
  
  #plot p~SEASON
  plot_impala_p_season <- ggplot(data = preds_impala_p[preds_impala_p$variable == 'det' & 
                                                             preds_impala_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('impala') +
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
  plot_impala_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'impala_p_effort', '.png', sep = ''), plot_impala_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'impala_p_season', '.png', sep = ''), plot_impala_p_season, width = 6, height = 4, dpi = 300)  

  
  ### Kudu ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$kudu)
  top_mod_kudu_p <- p_models$kudu$`~effort + season ~ 1`
  
  #predict
  preds_kudu_p <- occPred(top_mod_kudu_p, nickname = 'kudu_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_kudu_p_effort <- ggplot(data = preds_kudu_p[preds_kudu_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('kudu') +
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
  plot_kudu_p_effort
  
  #plot p~SEASON
  plot_kudu_p_season <- ggplot(data = preds_kudu_p[preds_kudu_p$variable == 'det' & 
                                                             preds_kudu_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('kudu') +
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
  plot_kudu_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'kudu_p_effort', '.png', sep = ''), plot_kudu_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'kudu_p_season', '.png', sep = ''), plot_kudu_p_season, width = 6, height = 4, dpi = 300)  
  
    
  ### Reeedbuck ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$reedbuck)
  top_mod_reedbuck_p <- p_models$reedbuck$`~effort + season ~ 1`
  
  #predict
  preds_reedbuck_p <- occPred(top_mod_reedbuck_p, nickname = 'reedbuck_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_reedbuck_p_effort <- ggplot(data = preds_reedbuck_p[preds_reedbuck_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('reedbuck') +
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
  plot_reedbuck_p_effort
  
  #plot p~SEASON
  plot_reedbuck_p_season <- ggplot(data = preds_reedbuck_p[preds_reedbuck_p$variable == 'det' & 
                                                             preds_reedbuck_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('reedbuck') +
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
  plot_reedbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'reedbuck_p_effort', '.png', sep = ''), plot_reedbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'reedbuck_p_season', '.png', sep = ''), plot_reedbuck_p_season, width = 6, height = 4, dpi = 300)  

  
  ### Roan ---------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$roan)
  top_mod_roan_p <- p_models$roan$`~effort + season ~ 1`
  
  #predict
  preds_roan_p <- occPred(top_mod_roan_p, nickname = 'roan_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_roan_p_effort <- ggplot(data = preds_roan_p[preds_roan_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('roan') +
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
  plot_roan_p_effort
  
  #plot p~SEASON
  plot_roan_p_season <- ggplot(data = preds_roan_p[preds_roan_p$variable == 'det' & 
                                                             preds_roan_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('roan') +
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
  plot_roan_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'roan_p_effort', '.png', sep = ''), plot_roan_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'roan_p_season', '.png', sep = ''), plot_roan_p_season, width = 6, height = 4, dpi = 300)  
  
    
  ### Sable ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$sable)
  top_mod_sable_p <- p_models$sable$`~effort + season ~ 1`
  
  #predict
  preds_sable_p <- occPred(top_mod_sable_p, nickname = 'sable_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_sable_p_effort <- ggplot(data = preds_sable_p[preds_sable_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('sable') +
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
  plot_sable_p_effort
  
  #plot p~SEASON
  plot_sable_p_season <- ggplot(data = preds_sable_p[preds_sable_p$variable == 'det' & 
                                                             preds_sable_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('sable') +
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
  plot_sable_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'sable_p_effort', '.png', sep = ''), plot_sable_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'sable_p_season', '.png', sep = ''), plot_sable_p_season, width = 6, height = 4, dpi = 300)  
  
  
  ### Warthog ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$warthog)
  top_mod_warthog_p <- p_models$warthog$`~effort + season ~ 1`
  
  #predict
  preds_warthog_p <- occPred(top_mod_warthog_p, nickname = 'warthog_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_warthog_p_effort <- ggplot(data = preds_warthog_p[preds_warthog_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('warthog') +
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
  plot_warthog_p_effort
  
  #plot p~SEASON
  plot_warthog_p_season <- ggplot(data = preds_warthog_p[preds_warthog_p$variable == 'det' & 
                                                             preds_warthog_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('warthog') +
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
  plot_warthog_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'warthog_p_effort', '.png', sep = ''), plot_warthog_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'warthog_p_season', '.png', sep = ''), plot_warthog_p_season, width = 6, height = 4, dpi = 300)  
  

  ### Waterbuck ----------------------------------------------------------------
  
  #which one was top model?
  names(p_models$waterbuck)
  top_mod_waterbuck_p <- p_models$waterbuck$`~effort + season ~ 1`
  
  #predict
  preds_waterbuck_p <- occPred(top_mod_waterbuck_p, nickname = 'waterbuck_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_waterbuck_p_effort <- ggplot(data = preds_waterbuck_p[preds_waterbuck_p$variable == 'det',], 
                                   aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('waterbuck') +
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
  plot_waterbuck_p_effort
  
  #plot p~SEASON
  plot_waterbuck_p_season <- ggplot(data = preds_waterbuck_p[preds_waterbuck_p$variable == 'det' & 
                                                             preds_waterbuck_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('waterbuck') +
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
  plot_waterbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'waterbuck_p_effort', '.png', sep = ''), plot_waterbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'waterbuck_p_season', '.png', sep = ''), plot_waterbuck_p_season, width = 6, height = 4, dpi = 300)  

  
  ### Zebra ------------------------------------------------------------------
  
  #which one was top model?
  names(p_models$zebra)
  top_mod_zebra_p <- p_models$zebra$`~effort + season ~ 1`
  
  #predict
  preds_zebra_p <- occPred(top_mod_zebra_p, nickname = 'zebra_p', 
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_zebra_p_effort <- ggplot(data = preds_zebra_p[preds_zebra_p$variable == 'det',], 
                                  aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.3) +
    ggtitle('zebra') +
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
  plot_zebra_p_effort
  
  #plot p~SEASON
  plot_zebra_p_season <- ggplot(data = preds_zebra_p[preds_zebra_p$variable == 'det' & 
                                                           preds_zebra_p$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly probability of detection (\u00B1 95 CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('zebra') +
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
  plot_zebra_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'zebra_p_effort', '.png', sep = ''), plot_zebra_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'zebra_p_season', '.png', sep = ''), plot_zebra_p_season, width = 6, height = 4, dpi = 300)

    
  
## PRINT ALL TOGETHER (shd reduce y axes here)
  
  fig6 <- ((mod1_plot_prop + theme(plot.title = element_blank())) | mod2_plot_a) + plot_layout(widths = c(1, 2))
  fig6
  
  (p_effort_figs <- (plot_buffalo_p_effort | plot_bushbuck_p_effort | plot_bushpig_p_effort | plot_eland_p_effort | plot_elephant_p_effort) /
                    (plot_impala_p_effort | plot_kudu_p_effort | plot_reedbuck_p_effort | plot_roan_p_effort | plot_sable_p_effort) /
                    (plot_warthog_p_effort | plot_waterbuck_p_effort | plot_zebra_p_effort | plot_spacer() | plot_spacer()))

  ggsave('ss_occ/figures/detection/all_p_effort.png', p_effort_figs, width = 15, height = 8, dpi = 300)
  
  (p_season_figs <- (plot_buffalo_p_season | plot_bushbuck_p_season | plot_bushpig_p_season | plot_eland_p_season | plot_elephant_p_season) /
      (plot_impala_p_season | plot_kudu_p_season | plot_reedbuck_p_season | plot_roan_p_season | plot_sable_p_season) /
      (plot_warthog_p_season | plot_waterbuck_p_season | plot_zebra_p_season | plot_spacer() | plot_spacer()))
  
  ggsave('ss_occ/figures/detection/all_p_season.png', p_season_figs, width = 15, height = 8, dpi = 300)
  
  
  
## Predict (OCCUPANCY - ENVIR ONLY) --------------------------------------------
  names(envir_models_univar) #bushbuck, bushpig, eland, elephant, kudu, reedbuck, sable, waterbuck
  names(envir_models_multivar) #buffalo, impala, roan, warthog, zebra
  
  #read in if necessary
  # predict_values <- readRDS('data/cleaned/covariates/covars_for_plotting.RDS')
  

  ## WAIT... DO I NEED TO DO SOMETHING TO UN-STANDARDIZE COVS HERE?  
  
  
  ### Buffalo ------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_multivar$buffalo)
  top_mod_buffalo_psi <- envir_models_multivar$buffalo$`~1 ~ elev_sd_3x3 + pct_slope_sd_3x3`
  
  #predict
  preds_buffalo_psi <- occPred(top_mod_buffalo_psi, nickname = 'buffalo_psi', 
                               new_data = expand_grid('elev_sd_3x3' = predict_values$elev_sd_3x3,
                                                      'pct_slope_sd_3x3' = predict_values$pct_slope_sd_3x3))
  
  #plot psi~ELEV
  plot_buffalo_psi_elev <- ggplot(data = preds_buffalo_psi[preds_buffalo_psi$variable == 'occ' & 
                                                             preds_buffalo_psi$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3',]$mean,], 
                                  aes(x = elev_sd_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation SD (m)') +
    # facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('buffalo') +
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
  plot_buffalo_psi_elev
  
  #plot psi~SLOPE
  plot_buffalo_psi_slope <- ggplot(data = preds_buffalo_psi[preds_buffalo_psi$variable == 'occ' & 
                                                             preds_buffalo_psi$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3',]$mean,], 
                                  aes(x = pct_slope_sd_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Slope SD') +
    # facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('buffalo') +
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
  plot_buffalo_psi_slope
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_elevSD', '.png', sep = ''), plot_buffalo_psi_elev, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_slopeSD', '.png', sep = ''), plot_buffalo_psi_slope, width = 6, height = 4, dpi = 300)
  
  
  ### Bushbuck -----------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$bushbuck)
  top_mod_bushbuck_psi <- envir_models_univar$bushbuck$`~1 ~ tree_vol_mean_3x3`
  
  #predict
  preds_bushbuck_psi <- occPred(top_mod_bushbuck_psi, nickname = 'bushbuck_psi', 
                                new_data = expand_grid('tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  #plot psi~TREE_VOL
  plot_bushbuck_psi_treevol <- ggplot(data = preds_bushbuck_psi[preds_bushbuck_psi$variable == 'occ',], 
                                  aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Tree volume (mean m3/ha)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('bushbuck') +
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
  plot_bushbuck_psi_treevol
  
  ## doesn't look right... plot rug values to see what's going on here
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushbuck_psi_treevol', '.png', sep = ''), plot_bushbuck_psi_treevol, width = 6, height = 4, dpi = 300)

  
  ### Bushpig -----------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$bushpig)
  top_mod_bushpig_psi <- envir_models_univar$bushpig$`~1 ~ tree_vol_mean_3x3`
  
  #predict
  preds_bushpig_psi <- occPred(top_mod_bushpig_psi, nickname = 'bushpig_psi', 
                                new_data = expand_grid('tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  #plot psi~TREE_VOL
  plot_bushpig_psi_treevol <- ggplot(data = preds_bushpig_psi[preds_bushpig_psi$variable == 'occ',], 
                                      aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Tree volume (mean m3/ha)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('bushpig') +
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
  plot_bushpig_psi_treevol
  
  ## doesn't look right... plot rug values to see what's going on here
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushpig_psi_treevol', '.png', sep = ''), plot_bushpig_psi_treevol, width = 6, height = 4, dpi = 300)

  
  ### Eland --------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$eland)
  top_mod_eland_psi <- envir_models_univar$eland$`~1 ~ dist_cleared_sd_3x3`
  
  #predict
  preds_eland_psi <- occPred(top_mod_eland_psi, nickname = 'eland_psi', 
                               new_data = expand_grid('dist_cleared_sd_3x3' = predict_values$dist_cleared_sd_3x3))
  
  #plot psi~TREE_VOL
  plot_eland_psi_distcleared <- ggplot(data = preds_eland_psi[preds_eland_psi$variable == 'occ',], 
                                     aes(x = dist_cleared_sd_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Dist to cleared area SD (m)') +
    # facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('eland') +
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
  plot_eland_psi_distcleared
  
  ## doesn't look right... plot rug values to see what's going on here
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'eland_psi_distcleared', '.png', sep = ''), plot_eland_psi_distcleared, width = 6, height = 4, dpi = 300)
  
  
  
  ### Elephant --------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$elephant)
  top_mod_elephant_psi <- envir_models_univar$elephant$`~1 ~ prop_M3_3x3`
  
  #predict
  preds_elephant_psi <- occPred(top_mod_elephant_psi, nickname = 'elephant_psi', 
                             new_data = expand_grid('prop_M3_3x3' = predict_values$prop_M3_3x3))
  
  #plot psi~M3
  plot_elephant_psi_propM3 <- ggplot(data = preds_elephant_psi[preds_elephant_psi$variable == 'occ',], 
                                       aes(x = prop_M3_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Proportion M3') +
    # facet_grid(~season) +
    ylim(0,0.5) +
    ggtitle('elephant') +
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
  plot_elephant_psi_propM3

  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'elephant_psi_propM3', '.png', sep = ''), plot_elephant_psi_propM3, width = 6, height = 4, dpi = 300)
  
  
  ### Impala ------------------------------------------------------------------
  
  #which one was top model? we had a couple....
  names(envir_models_multivar$impala)
  top_mod_impala_psi1 <- envir_models_multivar$impala$`~1 ~ pct_slope_mean_3x3`
  top_mod_impala_psi2 <- envir_models_multivar$impala$`~1 ~ dist_cleared_mean_3x3`
  top_mod_impala_psi3 <- envir_models_multivar$impala$`~1 ~ elev_sd_3x3`

  #predict
  preds_impala_psi1 <- occPred(top_mod_impala_psi1, nickname = 'impala_psi1', 
                               new_data = expand_grid('pct_slope_mean_3x3' = predict_values$pct_slope_mean_3x3))
  preds_impala_psi2 <- occPred(top_mod_impala_psi2, nickname = 'impala_psi2', 
                               new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3))
  preds_impala_psi3 <- occPred(top_mod_impala_psi3, nickname = 'impala_psi3', 
                               new_data = expand_grid('elev_sd_3x3' = predict_values$elev_sd_3x3))
 
  #plot psi~SLOPE
  plot_impala_psi_slope <- ggplot(data = preds_impala_psi1[preds_impala_psi1$variable == 'occ',], 
                                   aes(x = pct_slope_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Slope (mean)') +
    # facet_grid(~season) +
    # ylim(0,0.5) +
    ggtitle('impala') +
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
  plot_impala_psi_slope
  
  #plot psi~DIST_CLEARED
  plot_impala_psi_cleared <- ggplot(data = preds_impala_psi2[preds_impala_psi2$variable == 'occ',], 
                                  aes(x = dist_cleared_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance to cleared area (m)') +
    # facet_grid(~season) +
    # ylim(0,0.5) +
    ggtitle('impala') +
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
  plot_impala_psi_cleared

  #plot psi~ELEV_SD
  plot_impala_psi_elevSD <- ggplot(data = preds_impala_psi3[preds_impala_psi3$variable == 'occ',], 
                                 aes(x = elev_sd_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation SD (m)') +
    # facet_grid(~season) +
    # ylim(0,0.5) +
    ggtitle('impala') +
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
  plot_impala_psi_elevSD
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'impala_psi_slope', '.png', sep = ''), plot_impala_psi_slope, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'impala_psi_cleared', '.png', sep = ''), plot_impala_psi_cleared, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'impala_psi_elevSD', '.png', sep = ''), plot_impala_psi_elevSD, width = 6, height = 4, dpi = 300)
  
  
  ### Kudu ---------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$kudu)
  top_mod_kudu_psi <- envir_models_univar$kudu$`~1 ~ elev_mean_3x3`
  
  #predict
  preds_kudu_psi <- occPred(top_mod_kudu_psi, nickname = 'kudu_psi', 
                             new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  #plot psi~ELEV_MEAN
  plot_kudu_psi_elev <- ggplot(data = preds_kudu_psi[preds_kudu_psi$variable == 'occ',], 
                               aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation mean (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('kudu') +
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
  plot_kudu_psi_elev
  
  #why all 0s? this was the top model... just a v weak effect?? I'm confused
  envir_models_univar$kudu$`~1 ~ elev_mean_3x3`
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'kudu_psi_elev', '.png', sep = ''), plot_kudu_psi_elev, width = 6, height = 4, dpi = 300)
  
  
  ### reedbuck ---------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$reedbuck)
  top_mod_reedbuck_psi <- envir_models_univar$reedbuck$`~1 ~ dist_cleared_mean_3x3`
  
  #predict
  preds_reedbuck_psi <- occPred(top_mod_reedbuck_psi, nickname = 'reedbuck_psi', 
                            new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3))
  
  #plot psi~CLEARED
  plot_reedbuck_psi_cleared <- ggplot(data = preds_reedbuck_psi[preds_reedbuck_psi$variable == 'occ',], 
                                      aes(x = dist_cleared_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance to cleared area (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('reedbuck') +
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
  plot_reedbuck_psi_cleared
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'reedbuck_psi_cleared', '.png', sep = ''), plot_reedbuck_psi_cleared, width = 6, height = 4, dpi = 300)
  
  
  ### roan ------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_multivar$roan) #actually just had 2 top univar models (they're correlated)
  top_mod_roan_psi1 <- envir_models_multivar$roan$`~1 ~ prop_M3_3x3`
  top_mod_roan_psi2 <- envir_models_multivar$roan$`~1 ~ tree_vol_mean_3x3`
  
  #predict
  preds_roan_psi1 <- occPred(top_mod_roan_psi1, nickname = 'roan_psi1', 
                             new_data = expand_grid('prop_M3_3x3' = predict_values$prop_M3_3x3))
  preds_roan_psi2 <- occPred(top_mod_roan_psi2, nickname = 'roan_psi2', 
                             new_data = expand_grid('tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))

  #plot psi~M3
  plot_roan_psi_M3 <- ggplot(data = preds_roan_psi1[preds_roan_psi1$variable == 'occ',], 
                                  aes(x = prop_M3_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Proportion M3') +
    # facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('roan') +
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
  plot_roan_psi_M3
  
  #plot psi~TREEVOL
  plot_roan_psi_treevol <- ggplot(data = preds_roan_psi2[preds_roan_psi2$variable == 'occ',], 
                                  aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Tree volume (mean m3/ha)') +
    # facet_grid(~season) +
    ylim(0,0.2) +
    ggtitle('roan') +
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
  plot_roan_psi_treevol
  
  #treevol doesn't look right; plot rug values here

  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'roan_psi_M3', '.png', sep = ''), plot_roan_psi_M3, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'roan_psi_treevol', '.png', sep = ''), plot_roan_psi_treevol, width = 6, height = 4, dpi = 300)
  
  
  ### sable -----------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$sable)
  top_mod_sable_psi <- envir_models_univar$sable$`~1 ~ elev_mean_3x3`
  
  #predict
  preds_sable_psi <- occPred(top_mod_sable_psi, nickname = 'sable_psi', 
                                new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  #plot psi~ELEV
  plot_sable_psi_elev <- ggplot(data = preds_sable_psi[preds_sable_psi$variable == 'occ',], 
                                      aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation mean (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('sable') +
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
  plot_sable_psi_elev
  
  ## doesn't look right... plot rug values to see what's going on here
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'sable_psi_elev', '.png', sep = ''), plot_sable_psi_elev, width = 6, height = 4, dpi = 300)
  
  
  
  ### warthog -----------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_multivar$warthog)
  top_mod_warthog_psi1 <- envir_models_multivar$warthog$`~1 ~ elev_sd_3x3`
  top_mod_warthog_psi2 <- envir_models_multivar$warthog$`~1 ~ pct_slope_mean_3x3`
  
  #predict
  preds_warthog_psi1 <- occPred(top_mod_warthog_psi1, nickname = 'warthog_psi1', 
                             new_data = expand_grid('elev_sd_3x3' = predict_values$elev_sd_3x3))
  preds_warthog_psi2 <- occPred(top_mod_warthog_psi2, nickname = 'warthog_psi2', 
                               new_data = expand_grid('pct_slope_mean_3x3' = predict_values$pct_slope_mean_3x3))
  
  #plot psi~ELEV
  plot_warthog_psi_elev <- ggplot(data = preds_warthog_psi1[preds_warthog_psi1$variable == 'occ',], 
                                aes(x = elev_sd_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation SD (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('warthog') +
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
  plot_warthog_psi_elev
  
  #plot psi~SLOPE
  plot_warthog_psi_slope <- ggplot(data = preds_warthog_psi2[preds_warthog_psi2$variable == 'occ',], 
                                  aes(x = pct_slope_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Slope mean') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('warthog') +
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
  plot_warthog_psi_slope
  
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'warthog_psi_elev', '.png', sep = ''), plot_warthog_psi_elev, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'warthog_psi_slope', '.png', sep = ''), plot_warthog_psi_slope, width = 6, height = 4, dpi = 300)
  
  
  
  ### waterbuck -----------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_univar$waterbuck)
  top_mod_waterbuck_psi <- envir_models_univar$waterbuck$`~1 ~ elev_mean_3x3`
  
  #predict
  preds_waterbuck_psi <- occPred(top_mod_waterbuck_psi, nickname = 'waterbuck_psi', 
                                new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  #plot psi~TREE_VOL
  plot_waterbuck_psi_elev <- ggplot(data = preds_waterbuck_psi[preds_waterbuck_psi$variable == 'occ',], 
                                      aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Elevation mean (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('waterbuck') +
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
  plot_waterbuck_psi_elev
  
  ## doesn't look right... plot rug values to see what's going on here
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'waterbuck_psi_elev', '.png', sep = ''), plot_waterbuck_psi_elev, width = 6, height = 4, dpi = 300)
  
  
  ### zebra ------------------------------------------------------------------
  
  #which one was top model?
  names(envir_models_multivar$zebra)
  top_mod_zebra_psi1 <- envir_models_multivar$zebra$`~1 ~ dist_cleared_mean_3x3 + prop_M1_3x3`
  top_mod_zebra_psi2 <- envir_models_multivar$zebra$`~1 ~ dist_cleared_mean_3x3 + tree_vol_mean_3x3`
  
  #predict
  preds_zebra_psi1 <- occPred(top_mod_zebra_psi1, nickname = 'zebra_psi1', 
                               new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3,
                                                      'prop_M1_3x3' = predict_values$prop_M1_3x3))
  preds_zebra_psi2 <- occPred(top_mod_zebra_psi2, nickname = 'zebra_psi2', 
                              new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3,
                                                     'tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  #plot psi~CLEARED (1st mod)
  plot_zebra_psi_cleared <- ggplot(data = preds_zebra_psi1[preds_zebra_psi1$variable == 'occ' & 
                                                          preds_zebra_psi1$prop_M1_3x3 == site_covars_summary[site_covars_summary$variable == 'prop_M1_3x3',]$mean,], 
                                  aes(x = dist_cleared_mean_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Distance to cleared area (m)') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('zebra') +
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
  plot_zebra_psi_cleared
  
  #plot psi~M1 (1st mod)
  plot_zebra_psi_M1 <- ggplot(data = preds_zebra_psi1[preds_zebra_psi1$variable == 'occ' & 
                                                             preds_zebra_psi1$dist_cleared_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_mean_3x3',]$min,], 
                                   aes(x = prop_M1_3x3, y = value)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    # scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    # scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Probability of use (\u00B1 95 CI)') +
    xlab('Proportion M1') +
    # facet_grid(~season) +
    # ylim(0,0.2) +
    ggtitle('zebra -- at min dist_cleared') +
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
  plot_zebra_psi_M1
  
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'zebra_psi_cleared', '.png', sep = ''), plot_zebra_psi_cleared, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'zebra_psi_M1', '.png', sep = ''), plot_zebra_psi_M1, width = 6, height = 4, dpi = 300)
  
  