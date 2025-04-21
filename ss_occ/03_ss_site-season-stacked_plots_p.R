## Make predictions / marginal plots (DETECTION)

library(data.table)
library(unmarked)
library(tidyverse)
library(patchwork)


## Read in model inputs and outputs --------------------------------------------

  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  p_models <- readRDS('ss_occ/outputs/model_objects_p.RDS')

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
  predict_values[['releases']] <- factor(c('Before','After'), levels = c('Before','After'))
  predict_values[['period']] <- factor(c('Fence','NoFence'), levels = c('Fence','NoFence'))
  predict_values[['fence']] <- factor(c('South','North'), levels = c('South','North'))
  predict_values[['season']] <- factor(c('wet','cool','dry'), levels = c('wet','cool','dry'))
  
  #save
  saveRDS(predict_values, 'data/cleaned/covariates/covars_for_plotting.RDS')

  
## Predict (DETECTION) ---------------------------------------------------------
  names(p_models)
  
  #read in if necessary
  # predict_values <- readRDS('data/cleaned/covariates/covars_for_plotting.RDS')
  p_top_mods <- readRDS('ss_occ/outputs/top_models_p.RDS')
  
  
### Buffalo ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$buffalo
  top_mod_buffalo_p1 <- p_models$buffalo$`~scale(effort) ~ 1`
  top_mod_buffalo_p2 <- p_models$buffalo$`~scale(effort) + season ~ 1`
  
  #predict
  preds_buffalo_p1 <- occPred(top_mod_buffalo_p1, nickname = 'buffalo_p1',
                              new_data = expand_grid('effort' = predict_values$effort))
  preds_buffalo_p2 <- occPred(top_mod_buffalo_p2, nickname = 'buffalo_p2',
                              new_data = expand_grid('effort' = predict_values$effort,
                                                     'season' = predict_values$season))
  
  #plot p~EFFORT
  plot_buffalo_p_effort <- ggplot(data = preds_buffalo_p2[preds_buffalo_p2$variable == 'det',], 
                                  aes(x = effort, y = value, color = season, fill = season)) +
    geom_line(lwd = 0.8) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    ylim(0,0.4) +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_buffalo_p_effort
  
  #plot p~SEASON
  plot_buffalo_p_season <- ggplot(data = preds_buffalo_p2[preds_buffalo_p2$variable == 'det' & 
                                                           preds_buffalo_p2$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.4) +
    ggtitle('buffalo (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_buffalo_p_season

  #save
  ggsave(paste('ss_occ/figures/detection/', 'buffalo_p_effort', '.png', sep = ''), plot_buffalo_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'buffalo_p_season', '.png', sep = ''), plot_buffalo_p_season, width = 6, height = 4, dpi = 300)
  
  
### Bushbuck -------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$bushbuck
  top_mod_bushbuck_p <- p_models$bushbuck$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushbuck_p_effort
  
  #plot p~SEASON
  plot_bushbuck_p_season <- ggplot(data = preds_bushbuck_p[preds_bushbuck_p$variable == 'det' & 
                                                           preds_bushbuck_p$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('bushbuck (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'bushbuck_p_effort', '.png', sep = ''), plot_bushbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'bushbuck_p_season', '.png', sep = ''), plot_bushbuck_p_season, width = 6, height = 4, dpi = 300)
  
  
### Bushpig --------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$bushpig
  top_mod_bushpig_p <- p_models$bushpig$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushpig_p_effort
  
  #plot p~SEASON
  plot_bushpig_p_season <- ggplot(data = preds_bushpig_p[preds_bushpig_p$variable == 'det' & 
                                                             preds_bushpig_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('bushpig (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushpig_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'bushpig_p_effort', '.png', sep = ''), plot_bushpig_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'bushpig_p_season', '.png', sep = ''), plot_bushpig_p_season, width = 6, height = 4, dpi = 300)  
  
  
### Eland ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$eland
  top_mod_eland_p <- p_models$eland$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_eland_p_effort
  
  #plot p~SEASON
  plot_eland_p_season <- ggplot(data = preds_eland_p[preds_eland_p$variable == 'det' & 
                                                       preds_eland_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('eland (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_eland_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'eland_p_effort', '.png', sep = ''), plot_eland_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'eland_p_season', '.png', sep = ''), plot_eland_p_season, width = 6, height = 4, dpi = 300)  
  
  
### Elephant -----------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$elephant
  top_mod_elephant_p <- p_models$elephant$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_elephant_p_effort
  
  #plot p~SEASON
  plot_elephant_p_season <- ggplot(data = preds_elephant_p[preds_elephant_p$variable == 'det' & 
                                                             preds_elephant_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('elephant (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_elephant_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'elephant_p_effort', '.png', sep = ''), plot_elephant_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'elephant_p_season', '.png', sep = ''), plot_elephant_p_season, width = 6, height = 4, dpi = 300)  
  
  
### Impala -------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$impala
  top_mod_impala_p <- p_models$impala$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    facet_grid(~season) +
    # ylim(0,0.3) +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_impala_p_effort
  
  #plot p~SEASON
  plot_impala_p_season <- ggplot(data = preds_impala_p[preds_impala_p$variable == 'det' & 
                                                             preds_impala_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('impala (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_impala_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'impala_p_effort', '.png', sep = ''), plot_impala_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'impala_p_season', '.png', sep = ''), plot_impala_p_season, width = 6, height = 4, dpi = 300)  

  
### Kudu ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$kudu
  top_mod_kudu_p <- p_models$kudu$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_kudu_p_effort
  
  #plot p~SEASON
  plot_kudu_p_season <- ggplot(data = preds_kudu_p[preds_kudu_p$variable == 'det' & 
                                                             preds_kudu_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('kudu (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_kudu_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'kudu_p_effort', '.png', sep = ''), plot_kudu_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'kudu_p_season', '.png', sep = ''), plot_kudu_p_season, width = 6, height = 4, dpi = 300)  
  
    
### Reeedbuck ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$reedbuck
  top_mod_reedbuck_p <- p_models$reedbuck$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_reedbuck_p_effort
  
  #plot p~SEASON
  plot_reedbuck_p_season <- ggplot(data = preds_reedbuck_p[preds_reedbuck_p$variable == 'det' & 
                                                             preds_reedbuck_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('reedbuck (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_reedbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'reedbuck_p_effort', '.png', sep = ''), plot_reedbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'reedbuck_p_season', '.png', sep = ''), plot_reedbuck_p_season, width = 6, height = 4, dpi = 300)  

  
### Roan ---------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$roan
  top_mod_roan_p <- p_models$roan$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_roan_p_effort
  
  #plot p~SEASON
  plot_roan_p_season <- ggplot(data = preds_roan_p[preds_roan_p$variable == 'det' & 
                                                             preds_roan_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('roan (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_roan_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'roan_p_effort', '.png', sep = ''), plot_roan_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'roan_p_season', '.png', sep = ''), plot_roan_p_season, width = 6, height = 4, dpi = 300)  
  
    
### Sable ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$sable
  top_mod_sable_p <- p_models$sable$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_sable_p_effort
  
  #plot p~SEASON
  plot_sable_p_season <- ggplot(data = preds_sable_p[preds_sable_p$variable == 'det' & 
                                                             preds_sable_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    #optionally, could show values with effort = 1 day (or effort at its mean):
    # geom_pointrange(data = preds_sable_p[preds_sable_p$variable == 'det' & 
    #                                        preds_sable_p$effort == 1,], aes(ymin = LCI, ymax = UCI), shape = 17) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('sable (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_sable_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'sable_p_effort', '.png', sep = ''), plot_sable_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'sable_p_season', '.png', sep = ''), plot_sable_p_season, width = 6, height = 4, dpi = 300)  
  
  
### Warthog ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$warthog
  top_mod_warthog_p <- p_models$warthog$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_warthog_p_effort
  
  #plot p~SEASON
  plot_warthog_p_season <- ggplot(data = preds_warthog_p[preds_warthog_p$variable == 'det' & 
                                                             preds_warthog_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('warthog (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_warthog_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'warthog_p_effort', '.png', sep = ''), plot_warthog_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'warthog_p_season', '.png', sep = ''), plot_warthog_p_season, width = 6, height = 4, dpi = 300)  
  

### Waterbuck ----------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$waterbuck
  top_mod_waterbuck_p <- p_models$waterbuck$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_waterbuck_p_effort
  
  #plot p~SEASON
  plot_waterbuck_p_season <- ggplot(data = preds_waterbuck_p[preds_waterbuck_p$variable == 'det' & 
                                                             preds_waterbuck_p$effort == 7,], 
                                   aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('waterbuck (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_waterbuck_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'waterbuck_p_effort', '.png', sep = ''), plot_waterbuck_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'waterbuck_p_season', '.png', sep = ''), plot_waterbuck_p_season, width = 6, height = 4, dpi = 300)  

  
### Zebra ------------------------------------------------------------------
  
  #which one was top model?
  p_top_mods$zebra
  top_mod_zebra_p <- p_models$zebra$`~scale(effort) + season ~ 1`
  
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
    ylab('Weekly p (\u00B1 95% CI)') +
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
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_zebra_p_effort
  
  #plot p~SEASON
  plot_zebra_p_season <- ggplot(data = preds_zebra_p[preds_zebra_p$variable == 'det' & 
                                                           preds_zebra_p$effort == 7,], 
                                  aes(x = season, y = value, color = season, fill = season)) +
    geom_point() +
    geom_pointrange(aes(ymin = LCI, ymax = UCI)) +
    scale_color_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    scale_fill_manual(values = c('wet' = '#7fadf4', 'cool' = '#6bbf88', 'dry' = '#d3886b')) +
    ylab('Weekly p (\u00B1 95% CI)') +
    xlab('Effort (survey days per week)') +
    # facet_grid(~effort) +
    ylim(0,0.3) +
    ggtitle('zebra (at effort = 7 days)') +
    theme_bw() + theme(
      legend.position = 'none',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_zebra_p_season
  
  #save
  ggsave(paste('ss_occ/figures/detection/', 'zebra_p_effort', '.png', sep = ''), plot_zebra_p_effort, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/detection/', 'zebra_p_season', '.png', sep = ''), plot_zebra_p_season, width = 6, height = 4, dpi = 300)

    
  
## PRINT ALL TOGETHER (p) ------------------------------------------------------
  
  (p_effort_figs <- (plot_buffalo_p_effort | plot_bushbuck_p_effort | plot_bushpig_p_effort | plot_eland_p_effort | plot_elephant_p_effort) /
                    (plot_impala_p_effort | plot_kudu_p_effort | plot_reedbuck_p_effort | plot_roan_p_effort | plot_sable_p_effort) /
                    (plot_warthog_p_effort | plot_waterbuck_p_effort | plot_zebra_p_effort | plot_spacer() | plot_spacer()))

  ggsave('ss_occ/figures/detection/all_p_effort.png', p_effort_figs, width = 15, height = 8, dpi = 300)
  
  (p_season_figs <- (plot_buffalo_p_season | plot_bushbuck_p_season | plot_bushpig_p_season | plot_eland_p_season | plot_elephant_p_season) /
      (plot_impala_p_season | plot_kudu_p_season | plot_reedbuck_p_season | plot_roan_p_season | plot_sable_p_season) /
      (plot_warthog_p_season | plot_waterbuck_p_season | plot_zebra_p_season | plot_spacer() | plot_spacer()))
  
  ggsave('ss_occ/figures/detection/all_p_season.png', p_season_figs, width = 15, height = 8, dpi = 300)
  
  
    