## Make predictions / marginal plots (OCCUPANCY - STRUCTURAL)

library(data.table)
library(unmarked)
library(tidyverse)
library(patchwork)


## Read in model inputs and outputs --------------------------------------------

  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  final_models <- readRDS('ss_occ/outputs/model_objects_final.RDS')
  predict_values <- readRDS('data/cleaned/covariates/covars_for_plotting.RDS')
  
  #Source function 'occPred' I wrote for easy predictions from package 'unmarked'
  source('ss_occ/occ_mod_functions.R')


## Predict (OCCUPANCY - STRUCTURAL) --------------------------------------------
  names(final_models) 
  

### buffalo (psi) --------------------------------------------------------------

  #which one was top model? (reference AIC tables for this)
  top_mod_buffalo1 <- final_models$buffalo$`~scale(effort) ~ scale(elev_sd_3x3) + scale(pct_slope_sd_3x3) + fence * releases`
  top_mod_buffalo2 <- final_models$buffalo$`~scale(effort) ~ scale(elev_sd_3x3) + scale(pct_slope_sd_3x3) + season + fence * period`
  
  #predict
  preds_buffalo_psi1 <- occPred(top_mod_buffalo1, nickname = 'buffalo_psi1', 
                                new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                       'elev_sd_3x3' = predict_values$elev_sd_3x3,
                                                       'pct_slope_sd_3x3' = predict_values$pct_slope_sd_3x3,
                                                       'releases' = predict_values$releases,
                                                       'fence' = predict_values$fence))
  
  preds_buffalo_psi2 <- occPred(top_mod_buffalo2, nickname = 'buffalo_psi2', 
                                new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                       'elev_sd_3x3' = predict_values$elev_sd_3x3,
                                                       'pct_slope_sd_3x3' = predict_values$pct_slope_sd_3x3,
                                                       'fence' = predict_values$fence,
                                                       'period' = predict_values$period,
                                                       'season' = predict_values$season))

  #plot psi~FENCE*RELEASES
  plot_buffalo_psi_fenceRelease <- ggplot(data = preds_buffalo_psi1[preds_buffalo_psi1$variable == 'occ' & 
                                                                     preds_buffalo_psi1$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3',]$mean &
                                                                     preds_buffalo_psi1$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3',]$mean,], 
                                   aes(x = releases, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(releases), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Releases') +
    ylim(0,1) +
    ggtitle('buffalo') +
    # facet_grid(~releases) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_buffalo_psi_fenceRelease
  
  #plot psi~FENCE*PERIOD
  plot_buffalo_psi_fencePeriod <- ggplot(data = preds_buffalo_psi2[preds_buffalo_psi2$variable == 'occ' & 
                                                                      preds_buffalo_psi2$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3',]$mean &
                                                                      preds_buffalo_psi2$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3',]$mean,], 
                                          aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('buffalo') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_buffalo_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_fence-release', '.png', sep = ''), plot_buffalo_psi_fenceRelease, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_fence-period-season', '.png', sep = ''), plot_buffalo_psi_fencePeriod, width = 6, height = 4, dpi = 300)
  

### bushbuck (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_bushbuck <- final_models$bushbuck$`~scale(effort) ~ scale(tree_vol_mean_3x3) + season + fence * period`
  
  #predict
  preds_bushbuck_psi <- occPred(top_mod_bushbuck, nickname = 'bushbuck_psi', 
                                new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                       'tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3,
                                                       'season' = predict_values$season,
                                                       'fence' = predict_values$fence,
                                                       'period' = predict_values$period))
  
  #plot psi~PERIOD*FENCE
  plot_bushbuck_psi_fencePeriod <- ggplot(data = preds_bushbuck_psi[preds_bushbuck_psi$variable == 'occ' & 
                                                                    preds_bushbuck_psi$tree_vol_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'tree_vol_mean_3x3',]$mean,], 
                                         aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('bushbuck') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushbuck_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushbuck_psi_fence-period', '.png', sep = ''), plot_bushbuck_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
  
### bushpig (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_bushpig <- final_models$bushpig$`~scale(effort) + season ~ scale(tree_vol_mean_3x3) + season + fence * period`
  
  #predict
  preds_bushpig_psi <- occPred(top_mod_bushpig, nickname = 'bushpig_psi', 
                                new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                       # 'season' = 'wet', #won't matter for psi variables
                                                       'tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3,
                                                       'fence' = predict_values$fence,
                                                       'season' = predict_values$season,
                                                       'period' = predict_values$period))
  
  #plot psi~PERIOD*FENCE
  plot_bushpig_psi_fencePeriod <- ggplot(data = preds_bushpig_psi[preds_bushpig_psi$variable == 'occ' & 
                                                                      preds_bushpig_psi$tree_vol_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'tree_vol_mean_3x3',]$mean,], 
                                          aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('bushpig') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_bushpig_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushpig_psi_fence-period', '.png', sep = ''), plot_bushpig_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
### eland (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_eland1 <- final_models$eland$`~scale(effort) + season ~ scale(dist_cleared_sd_3x3) + fence * releases`
  top_mod_eland2 <- final_models$eland$`~1 ~ scale(dist_cleared_sd_3x3) + season + fence * period`
  
  #predict
  preds_eland_psi1 <- occPred(top_mod_eland1, nickname = 'eland_psi1', 
                              new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                      'season' = 'wet', #won't matter for psi variables
                                                      'dist_cleared_sd_3x3' = predict_values$dist_cleared_sd_3x3,
                                                      'releases' = predict_values$releases,
                                                      'fence' = predict_values$fence))
  preds_eland_psi2 <- occPred(top_mod_eland2, nickname = 'eland_psi2', 
                              new_data = expand_grid('dist_cleared_sd_3x3' = predict_values$dist_cleared_sd_3x3,
                                                     'season' = predict_values$season,
                                                     'fence' = predict_values$fence,
                                                     'period' = predict_values$period))
  
  #plot psi~FENCE*RELEASES
  plot_eland_psi_fenceRelease <- ggplot(data = preds_eland_psi1[preds_eland_psi1$variable == 'occ' & 
                                                                  preds_eland_psi1$dist_cleared_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_sd_3x3',]$mean,], 
                                         aes(x = releases, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(releases), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Releases') +
    ylim(0,1) +
    ggtitle('eland') +
    # facet_grid(~releases) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_eland_psi_fenceRelease
  
  #plot psi~FENCE*RELEASES
  plot_eland_psi_fencePeriod <- ggplot(data = preds_eland_psi2[preds_eland_psi2$variable == 'occ' & 
                                                                 preds_eland_psi2$dist_cleared_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_sd_3x3',]$mean,], 
                                        aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('eland') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_eland_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'eland_psi_fence-releases', '.png', sep = ''), plot_eland_psi_fenceRelease, width = 4, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'eland_psi_fence-period', '.png', sep = ''), plot_eland_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
### elephant (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_elephant <- final_models$elephant$`~scale(effort) + season ~ scale(prop_M3_3x3) + season + fence * period`
  
  #predict
  preds_elephant_psi <- occPred(top_mod_elephant, nickname = 'elephant_psi', 
                             new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                    # 'season' = 'wet', #won't matter for psi variables
                                                    'prop_M3_3x3' = predict_values$prop_M3_3x3,
                                                    'season' = predict_values$season,
                                                    'fence' = predict_values$fence,
                                                    'period' = predict_values$period))
  
  #plot psi~PERIOD*FENCE
  plot_elephant_psi_fencePeriod <- ggplot(data = preds_elephant_psi[preds_elephant_psi$variable == 'occ' & 
                                                                preds_elephant_psi$prop_M3_3x3 == site_covars_summary[site_covars_summary$variable == 'prop_M3_3x3',]$mean,], 
                                       aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('elephant') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_elephant_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'elephant_psi_fence-period', '.png', sep = ''), plot_elephant_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
### impala (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_impala1 <- final_models$impala$`~1 ~ scale(elev_mean_3x3) + season + fence * releases`
  top_mod_impala2 <- final_models$impala$`~1 ~ scale(elev_mean_3x3) + fence * period`
  
  #predict
  preds_impala_psi1 <- occPred(top_mod_impala1, nickname = 'impala_psi1', 
                                new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3,
                                                       'releases' = predict_values$releases,
                                                       'fence' = predict_values$fence,
                                                       'season' = predict_values$season))
  preds_impala_psi2 <- occPred(top_mod_impala2, nickname = 'impala_psi2', 
                               new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3,
                                                      'period' = predict_values$period,
                                                      'fence' = predict_values$fence))
  
  #plot psi~FENCE*RELEASES
  plot_impala_psi_fenceReleases <- ggplot(data = preds_impala_psi1[preds_impala_psi1$variable == 'occ' & 
                                                                     preds_impala_psi1$elev_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_mean_3x3',]$mean,], 
                                          aes(x = releases, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = releases), position = position_dodge(width = 0.2)) +
    # geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('impala') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_impala_psi_fenceReleases
  
  #plot psi~FENCE*PERIOD
  plot_impala_psi_fencePeriod <- ggplot(data = preds_impala_psi2[preds_impala_psi2$variable == 'occ' & 
                                                                   preds_impala_psi2$elev_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_mean_3x3',]$mean,], 
                                          aes(x = period, y = value, group = fence)) +
    # geom_line(lwd = 0.8, color = 'darkblue') +
    # geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = period), position = position_dodge(width = 0.2)) +
    # geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('impala') +
    # facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_impala_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'impala_psi_fence-releases-season', '.png', sep = ''), plot_impala_psi_fenceReleases, width = 4, height = 4, dpi = 300)
  
  
### kudu (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_kudu <- final_models$kudu$`~scale(effort) ~ scale(elev_mean_3x3) + season + fence * releases`
  
  #predict
  preds_kudu_psi <- occPred(top_mod_kudu, nickname = 'kudu_psi', 
                              new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                     'elev_mean_3x3' = predict_values$elev_mean_3x3,
                                                     'releases' = predict_values$releases,
                                                     'fence' = predict_values$fence,
                                                     'season' = predict_values$season))
  
  #plot psi~PERIOD*FENCE
  plot_kudu_psi_fencePeriod <- ggplot(data = preds_kudu_psi[preds_kudu_psi$variable == 'occ' & 
                                                                  preds_kudu_psi$elev_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_mean_3x3',]$mean,], 
                                        aes(x = releases, y = value, group = fence)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(releases), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('kudu') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_kudu_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'kudu_psi_fence-period', '.png', sep = ''), plot_kudu_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
### reedbuck (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_reedbuck <- final_models$reedbuck$`~scale(effort) + season ~ scale(dist_cleared_mean_3x3) + season + fence * period`
  
  #predict
  preds_reedbuck_psi <- occPred(top_mod_reedbuck, nickname = 'reedbuck_psi', 
                            new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                   # 'season' = 'wet', #won't matter for psi variables
                                                   'dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3,
                                                   'fence' = predict_values$fence,
                                                   'period' = predict_values$period,
                                                   'season' = predict_values$season))
  
  #plot psi~PERIOD*FENCE
  plot_reedbuck_psi_fencePeriod <- ggplot(data = preds_reedbuck_psi[preds_reedbuck_psi$variable == 'occ' & 
                                                              preds_reedbuck_psi$dist_cleared_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_mean_3x3',]$mean,], 
                                      aes(x = period, y = value, group = fence)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('reedbuck') +
    facet_grid(~season) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_reedbuck_psi_fencePeriod
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'reedbuck_psi_fence-period', '.png', sep = ''), plot_reedbuck_psi_fencePeriod, width = 4, height = 4, dpi = 300)
  
  
### roan (psi) --------------------------------------------------------------
  
  ## LEFT OFF HERE......
  
  #which one was top model? (reference AIC tables for this)
  top_mod_roan <- final_models$roan$`~1 ~ scale(prop_M3_3x3) + fence * period`
  
  #predict
  preds_roan_psi <- occPred(top_mod_roan, nickname = 'roan_psi', 
                            new_data = expand_grid('prop_M3_3x3' = predict_values$prop_M3_3x3,
                                                   'fence' = predict_values$fence,
                                                   'period' = predict_values$period))
  
  #plot psi~PERIOD*FENCE
  plot_roan_psi_periodFence <- ggplot(data = preds_roan_psi[preds_roan_psi$variable == 'occ' & 
                                                                      preds_roan_psi$prop_M3_3x3 == site_covars_summary[site_covars_summary$variable == 'prop_M3_3x3',]$mean,], 
                                          aes(x = period, y = value, group = fence)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('roan') +
    # facet_grid(~releases) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_roan_psi_periodFence
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'roan_psi_period-fence', '.png', sep = ''), plot_roan_psi_periodFence, width = 4, height = 4, dpi = 300)
  
  
  ### sable (psi) --------------------------------------------------------------
  
  #which one was top model? (reference AIC tables for this)
  top_mod_sable <- final_models$sable$`~scale(effort) + season ~ scale(elev_mean_3x3) + releases + fence * period`
  
  #predict
  preds_sable_psi <- occPred(top_mod_sable, nickname = 'sable_psi', 
                            new_data = expand_grid('effort' = 7, #won't matter for psi variables
                                                   'season' = 'wet', #won't matter for psi variables
                                                   'elev_mean_3x3' = predict_values$elev_mean_3x3,
                                                   'releases' = predict_values$releases,
                                                   'fence' = predict_values$fence,
                                                   'period' = predict_values$period))
  
  #plot psi~PERIOD*FENCE
  plot_sable_psi_periodFence <- ggplot(data = preds_sable_psi[preds_sable_psi$variable == 'occ' & 
                                                              preds_sable_psi$elev_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_mean_3x3',]$mean,], 
                                      aes(x = period, y = value, group = fence)) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI, color = fence), position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), aes(x = as.numeric(period), color = fence, linetype = fence)) +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Period') +
    ylim(0,1) +
    ggtitle('sable') +
    facet_grid(~releases) +
    theme_bw() + theme(
      legend.position = 'inside',
      legend.background = element_rect('transparent'),
      legend.justification = c(0.99,0.95),
      strip.text = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      plot.title = element_text(face = 'bold'))
  plot_sable_psi_periodFence
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'sable_psi_period-fence', '.png', sep = ''), plot_sable_psi_periodFence, width = 4, height = 4, dpi = 300)
  
  
  
  
  
  