## Make predictions / marginal plots (OCCUPANCY - ENVIR)

library(data.table)
library(unmarked)
library(tidyverse)
library(patchwork)
library(rphylopic)

## Read in model inputs and outputs --------------------------------------------

  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  envir_models_univar <- readRDS('ss_occ/outputs/model_objects_psi_univariate.RDS')
  envir_models_multivar <- readRDS('ss_occ/outputs/model_objects_psi_multivariate.RDS')
  predict_values <- readRDS('data/cleaned/covariates/covars_for_plotting.RDS')
  site_covars_summary <- fread('data/cleaned/covariates/covar_summmaries.csv')
  
  #Source function 'occPred' I wrote for easy predictions from package 'unmarked'
  source('ss_occ/occ_mod_functions.R')


## Read in phylopic IDs --------------------------------------------------------
  
  phylopic_ids <- fread('ss_occ/phylopic_ids.csv'); phylopic_ids <- phylopic_ids[,-'V1']
  
  #create x,y,height columns? or do as needed below
  
  geom_phylopic(aes(x = x, y = y, uuid = id, height = height), fill = 'black', color = NA) +
    
  
  
## Predict (OCCUPANCY - ENVIR ONLY) --------------------------------------------
  names(envir_models_univar) #bushbuck, bushpig, eland, elephant, kudu, reedbuck, sable, waterbuck
  names(envir_models_multivar) #buffalo, impala, roan, warthog, zebra
  

### buffalo (psi) --------------------------------------------------------------

  #which one was top model? (reference AIC tables for this)
  top_mod_buffalo_psi <- envir_models_multivar$buffalo$`~1 ~ scale(elev_sd_3x3) + scale(pct_slope_sd_3x3)`
  
  #predict
  preds_buffalo_psi <- occPred(top_mod_buffalo_psi, nickname = 'buffalo_psi', 
                               new_data = expand_grid('elev_sd_3x3' = predict_values$elev_sd_3x3,
                                                      'pct_slope_sd_3x3' = predict_values$pct_slope_sd_3x3))
  
  #et values for rug
  sp_input <- occu_inputs$buffalo
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  
  #get phylopic ID
  phy_id <- phylopic_ids[phylopic_ids$species_cleaned == 'buffalo',]$id
  
  #sites with buffalo > 0
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_buffalo_psi_elev <- ggplot(data = preds_buffalo_psi[preds_buffalo_psi$variable == 'occ' & 
                                                             preds_buffalo_psi$pct_slope_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'pct_slope_sd_3x3',]$mean,], 
                                  aes(x = elev_sd_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Elevation SD (m)') +
    ylim(0,1) +
    ggtitle('buffalo') +
    geom_phylopic(x = 10, y = 0.75, uuid = phy_id, height = 0.3, fill = 'black', color = NA) +
    # geom_phylopic(aes(x = 0.6, y = 0.2, uuid = id, height = size_scaling), fill = 'black', color = NA) +
    geom_rug(data = sp_siteCovs %>% select(elev_sd_3x3), 
             mapping = aes(x = elev_sd_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_sd_3x3), 
             mapping = aes(x = elev_sd_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_buffalo_psi_elev
  
  #plot psi~SLOPE
  plot_buffalo_psi_slope <- ggplot(data = preds_buffalo_psi[preds_buffalo_psi$variable == 'occ' & 
                                                              preds_buffalo_psi$elev_sd_3x3 == site_covars_summary[site_covars_summary$variable == 'elev_sd_3x3',]$mean,], 
                                   aes(x = pct_slope_sd_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkblue') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkblue') +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Slope SD') +
    ylim(0,1) +
    ggtitle('buffalo') +
    geom_rug(data = sp_siteCovs %>% select(pct_slope_sd_3x3), 
             mapping = aes(x = pct_slope_sd_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(pct_slope_sd_3x3), 
             mapping = aes(x = pct_slope_sd_3x3), inherit.aes = FALSE, color = 'darkblue', length = unit(0.05, 'npc')) +
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
  plot_buffalo_psi_slope
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_elevSD', '.png', sep = ''), plot_buffalo_psi_elev, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'buffalo_psi_slopeSD', '.png', sep = ''), plot_buffalo_psi_slope, width = 6, height = 4, dpi = 300)
  


### bushbuck (psi) -------------------------------------------------------------
  
  #which one was top model?
  top_mod_bushbuck_psi <- envir_models_univar$bushbuck$`~1 ~ scale(tree_vol_mean_3x3)`
  
  #predict
  preds_bushbuck_psi <- occPred(top_mod_bushbuck_psi, nickname = 'bushbuck_psi', 
                                new_data = expand_grid('tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$bushbuck
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  
  #sites with bushbuck > 0
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~TREEVOL
  plot_bushbuck_psi_treevol <- ggplot(data = preds_bushbuck_psi[preds_bushbuck_psi$variable == 'occ',], 
                                      aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkgreen') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkgreen') +
    ylab('ψ (\u00B1 95% CI)') +
    xlab('Tree volume (mean m3/ha)') +
    ylim(0,1) +
    ggtitle('bushbuck') +
    geom_rug(data = sp_siteCovs %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3), inherit.aes = FALSE, color = 'darkgreen', length = unit(0.05, 'npc')) +
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
  plot_bushbuck_psi_treevol
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushbuck_psi_treevol', '.png', sep = ''), plot_bushbuck_psi_treevol, width = 6, height = 4, dpi = 300)
  
  
### bushpig (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_bushpig_psi <- envir_models_univar$bushpig$`~1 ~ scale(tree_vol_mean_3x3)`
  
  #predict
  preds_bushpig_psi <- occPred(top_mod_bushpig_psi, nickname = 'bushpig_psi', 
                               new_data = expand_grid('tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$bushpig
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  
  #sites with bushpig > 0
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~TREEVOL
  plot_bushpig_psi_treevol <- ggplot(data = preds_bushpig_psi[preds_bushpig_psi$variable == 'occ',], 
                                     aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkgreen') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkgreen') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Tree volume (mean m3/ha)') +
    ylim(0,1) +
    ggtitle('bushpig') +
    geom_rug(data = sp_siteCovs %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3), inherit.aes = FALSE, color = 'darkgreen', length = unit(0.05, 'npc')) +
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
  plot_bushpig_psi_treevol
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'bushpig_psi_treevol', '.png', sep = ''), plot_bushpig_psi_treevol, width = 6, height = 4, dpi = 300)
  
  
  
### eland (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_eland_psi <- envir_models_univar$eland$`~1 ~ scale(dist_cleared_sd_3x3)`
  
  #predict
  preds_eland_psi <- occPred(top_mod_eland_psi, nickname = 'eland_psi', 
                             new_data = expand_grid('dist_cleared_sd_3x3' = predict_values$dist_cleared_sd_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$eland
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~DIST_CLEARED
  plot_eland_psi_cleared <- ggplot(data = preds_eland_psi[preds_eland_psi$variable == 'occ',], 
                                   aes(x = dist_cleared_sd_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'tan') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'tan') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Dist to cleared SD (m)') +
    ylim(0,1) +
    ggtitle('eland') +
    geom_rug(data = sp_siteCovs %>% select(dist_cleared_sd_3x3), 
             mapping = aes(x = dist_cleared_sd_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(dist_cleared_sd_3x3), 
             mapping = aes(x = dist_cleared_sd_3x3), inherit.aes = FALSE, color = 'tan', length = unit(0.05, 'npc')) +
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
  plot_eland_psi_cleared
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'eland_psi_cleared', '.png', sep = ''), plot_eland_psi_cleared, width = 6, height = 4, dpi = 300)
  
  
### elephant (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_elephant_psi <- envir_models_univar$elephant$`~1 ~ scale(prop_M3_3x3)`
  
  #predict
  preds_elephant_psi <- occPred(top_mod_elephant_psi, nickname = 'elephant_psi', 
                                new_data = expand_grid('prop_M3_3x3' = predict_values$prop_M3_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$elephant
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~M3
  plot_elephant_psi_m3 <- ggplot(data = preds_elephant_psi[preds_elephant_psi$variable == 'occ',], 
                                 aes(x = prop_M3_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'yellowgreen') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'yellowgreen') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Prop M3') +
    ylim(0,1) +
    ggtitle('elephant') +
    geom_rug(data = sp_siteCovs %>% select(prop_M3_3x3), 
             mapping = aes(x = prop_M3_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(prop_M3_3x3), 
             mapping = aes(x = prop_M3_3x3), inherit.aes = FALSE, color = 'yellowgreen', length = unit(0.05, 'npc')) +
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
  plot_elephant_psi_m3
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'elephant_psi_m3', '.png', sep = ''), plot_elephant_psi_m3, width = 6, height = 4, dpi = 300)
  
  
  
### impala (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_impala_psi <- envir_models_univar$impala$`~1 ~ scale(elev_mean_3x3)`
  
  #predict
  preds_impala_psi <- occPred(top_mod_impala_psi, nickname = 'impala_psi', 
                              new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$impala
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_impala_psi_elev <- ggplot(data = preds_impala_psi[preds_impala_psi$variable == 'occ',], 
                                 aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Elevation mean (m)') +
    ylim(0,1) +
    ggtitle('impala') +
    geom_rug(data = sp_siteCovs %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_impala_psi_elev
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'impala_psi_elev', '.png', sep = ''), plot_impala_psi_elev, width = 6, height = 4, dpi = 300)
  
  
  
### kudu (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_kudu_psi <- envir_models_univar$kudu$`~1 ~ scale(elev_mean_3x3)`
  
  #predict
  preds_kudu_psi <- occPred(top_mod_kudu_psi, nickname = 'kudu_psi', 
                            new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$kudu
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_kudu_psi_elev <- ggplot(data = preds_kudu_psi[preds_kudu_psi$variable == 'occ',], 
                               aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Elevation mean (m)') +
    ylim(0,1) +
    ggtitle('kudu') +
    geom_rug(data = sp_siteCovs %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_kudu_psi_elev
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'kudu_psi_elev', '.png', sep = ''), plot_kudu_psi_elev, width = 6, height = 4, dpi = 300)
  
  
### reedbuck (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_reedbuck_psi <- envir_models_univar$reedbuck$`~1 ~ scale(dist_cleared_mean_3x3)`
  
  #predict
  preds_reedbuck_psi <- occPred(top_mod_reedbuck_psi, nickname = 'reedbuck_psi', 
                                new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$reedbuck
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~CLEARED
  plot_reedbuck_psi_cleared <- ggplot(data = preds_reedbuck_psi[preds_reedbuck_psi$variable == 'occ',], 
                                      aes(x = dist_cleared_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'tan') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'tan') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Dist to cleared mean (m)') +
    ylim(0,1) +
    ggtitle('reedbuck') +
    geom_rug(data = sp_siteCovs %>% select(dist_cleared_mean_3x3), 
             mapping = aes(x = dist_cleared_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(dist_cleared_mean_3x3), 
             mapping = aes(x = dist_cleared_mean_3x3), inherit.aes = FALSE, color = 'tan', length = unit(0.05, 'npc')) +
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
  plot_reedbuck_psi_cleared
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'reedbuck_psi_cleared', '.png', sep = ''), plot_reedbuck_psi_cleared, width = 6, height = 4, dpi = 300)
  
  
### roan (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_roan_psi <- envir_models_univar$roan$`~1 ~ scale(prop_M3_3x3)`
  
  #predict
  preds_roan_psi <- occPred(top_mod_roan_psi, nickname = 'roan_psi', 
                            new_data = expand_grid('prop_M3_3x3' = predict_values$prop_M3_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$roan
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~M3
  plot_roan_psi_m3 <- ggplot(data = preds_roan_psi[preds_roan_psi$variable == 'occ',], 
                             aes(x = prop_M3_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'yellowgreen') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'yellowgreen') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Prop M3') +
    ylim(0,1) +
    ggtitle('roan') +
    geom_rug(data = sp_siteCovs %>% select(prop_M3_3x3), 
             mapping = aes(x = prop_M3_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(prop_M3_3x3), 
             mapping = aes(x = prop_M3_3x3), inherit.aes = FALSE, color = 'yellowgreen', length = unit(0.05, 'npc')) +
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
  plot_roan_psi_m3
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'roan_psi_m3', '.png', sep = ''), plot_roan_psi_m3, width = 6, height = 4, dpi = 300)
  
  
  
### sable (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_sable_psi <- envir_models_univar$sable$`~1 ~ scale(elev_mean_3x3)`
  
  #predict
  preds_sable_psi <- occPred(top_mod_sable_psi, nickname = 'sable_psi', 
                             new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$sable
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_sable_psi_elev <- ggplot(data = preds_sable_psi[preds_sable_psi$variable == 'occ',], 
                                aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Elevation mean (m)') +
    ylim(0,1) +
    ggtitle('sable') +
    geom_rug(data = sp_siteCovs %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_sable_psi_elev
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'sable_psi_elev', '.png', sep = ''), plot_sable_psi_elev, width = 6, height = 4, dpi = 300)
  
  
### warthog (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_warthog_psi <- envir_models_univar$warthog$`~1 ~ scale(elev_sd_3x3)`
  
  #predict
  preds_warthog_psi <- occPred(top_mod_warthog_psi, nickname = 'warthog_psi', 
                               new_data = expand_grid('elev_sd_3x3' = predict_values$elev_sd_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$warthog
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_warthog_psi_elev <- ggplot(data = preds_warthog_psi[preds_warthog_psi$variable == 'occ',], 
                                  aes(x = elev_sd_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Elevation SD (m)') +
    ylim(0,1) +
    ggtitle('warthog') +
    geom_rug(data = sp_siteCovs %>% select(elev_sd_3x3), 
             mapping = aes(x = elev_sd_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_sd_3x3), 
             mapping = aes(x = elev_sd_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_warthog_psi_elev
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'warthog_psi_elev', '.png', sep = ''), plot_warthog_psi_elev, width = 6, height = 4, dpi = 300)
  
  
### waterbuck (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_waterbuck_psi <- envir_models_univar$waterbuck$`~1 ~ scale(elev_mean_3x3)`
  
  #predict
  preds_waterbuck_psi <- occPred(top_mod_waterbuck_psi, nickname = 'waterbuck_psi', 
                                 new_data = expand_grid('elev_mean_3x3' = predict_values$elev_mean_3x3))
  
  # #get values for rug
  sp_input <- occu_inputs$waterbuck
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~ELEV
  plot_waterbuck_psi_elev <- ggplot(data = preds_waterbuck_psi[preds_waterbuck_psi$variable == 'occ',], 
                                    aes(x = elev_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkorange') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkorange') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Elevation mean (m)') +
    ylim(0,1) +
    ggtitle('waterbuck') +
    geom_rug(data = sp_siteCovs %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(elev_mean_3x3), 
             mapping = aes(x = elev_mean_3x3), inherit.aes = FALSE, color = 'darkorange', length = unit(0.05, 'npc')) +
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
  plot_waterbuck_psi_elev
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'waterbuck_psi_elev', '.png', sep = ''), plot_waterbuck_psi_elev, width = 6, height = 4, dpi = 300)
  
  
### zebra (psi) ------------------------------------------------------------
  
  #which one was top model?
  top_mod_zebra_psi1 <- envir_models_multivar$zebra$`~1 ~ scale(dist_cleared_mean_3x3) + scale(prop_M1_3x3)`
  top_mod_zebra_psi2 <- envir_models_multivar$zebra$`~1 ~ scale(dist_cleared_mean_3x3) + scale(tree_vol_mean_3x3)`
  
  #predict
  preds_zebra_psi1 <- occPred(top_mod_zebra_psi1, nickname = 'zebra_psi1', 
                              new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3,
                                                     'prop_M1_3x3' = predict_values$prop_M1_3x3))
  preds_zebra_psi2 <- occPred(top_mod_zebra_psi2, nickname = 'zebra_psi2', 
                              new_data = expand_grid('dist_cleared_mean_3x3' = predict_values$dist_cleared_mean_3x3,
                                                     'tree_vol_mean_3x3' = predict_values$tree_vol_mean_3x3))
  
  #get values for rug
  sp_input <- occu_inputs$zebra
  sp_y <- data.frame(sp_input@y)
  sp_siteCovs <- as.data.frame(sp_input@siteCovs)
  sp_y$total <- rowSums(sp_y, na.rm = TRUE)
  sp_sites <- sp_y %>% rownames_to_column(var = 'site_season') %>% filter(total > 0) %>% pull(site_season)
  
  #plot psi~CLEARED
  plot_zebra_psi_cleared <- ggplot(data = preds_zebra_psi1[preds_zebra_psi1$variable == 'occ' &
                                                             preds_zebra_psi1$prop_M1_3x3 == site_covars_summary[site_covars_summary$variable == 'prop_M1_3x3',]$mean,], 
                                   aes(x = dist_cleared_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'tan') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'tan') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Dist to cleared mean (m)') +
    ylim(0,1) +
    ggtitle('zebra') +
    geom_rug(data = sp_siteCovs %>% select(dist_cleared_mean_3x3), 
             mapping = aes(x = dist_cleared_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(dist_cleared_mean_3x3), 
             mapping = aes(x = dist_cleared_mean_3x3), inherit.aes = FALSE, color = 'tan', length = unit(0.05, 'npc')) +
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
  plot_zebra_psi_cleared
  
  #plot psi~M1
  plot_zebra_psi_m1 <- ggplot(data = preds_zebra_psi1[preds_zebra_psi1$variable == 'occ' &
                                                        preds_zebra_psi1$dist_cleared_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_mean_3x3',]$mean,], 
                              aes(x = prop_M1_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'chartreuse3') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'chartreuse3') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Prop M1') +
    ylim(0,1) +
    ggtitle('zebra') +
    geom_rug(data = sp_siteCovs %>% select(prop_M1_3x3), 
             mapping = aes(x = prop_M1_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(prop_M1_3x3), 
             mapping = aes(x = prop_M1_3x3), inherit.aes = FALSE, color = 'chartreuse3', length = unit(0.05, 'npc')) +
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
  plot_zebra_psi_m1
  
  #plot psi~treevol
  plot_zebra_psi_treevol <- ggplot(data = preds_zebra_psi2[preds_zebra_psi2$variable == 'occ' &
                                                             preds_zebra_psi2$dist_cleared_mean_3x3 == site_covars_summary[site_covars_summary$variable == 'dist_cleared_mean_3x3',]$mean,], 
                                   aes(x = tree_vol_mean_3x3, y = value)) +
    geom_line(lwd = 0.8, color = 'darkgreen') +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA, fill = 'darkgreen') +
    ylab('ψ \u00B1 95% CI') +
    xlab('Tree volume (mean m3/ha)') +
    ylim(0,1) +
    ggtitle('zebra') +
    geom_rug(data = sp_siteCovs %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3 + runif(nrow(sp_siteCovs), -0.01, 0.01)), inherit.aes = FALSE, color = 'gray70') +
    geom_rug(data = sp_siteCovs %>% filter(site_season %in% sp_sites) %>% select(tree_vol_mean_3x3), 
             mapping = aes(x = tree_vol_mean_3x3), inherit.aes = FALSE, color = 'darkgreen', length = unit(0.05, 'npc')) +
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
  plot_zebra_psi_treevol
  
  #save
  ggsave(paste('ss_occ/figures/occupancy/', 'zebra_psi_cleared', '.png', sep = ''), plot_zebra_psi_cleared, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'zebra_psi_m1', '.png', sep = ''), plot_zebra_psi_m1, width = 6, height = 4, dpi = 300)
  ggsave(paste('ss_occ/figures/occupancy/', 'zebra_psi_treevol', '.png', sep = ''), plot_zebra_psi_treevol, width = 6, height = 4, dpi = 300)
  
  
  
## PRINT ALL TOGETHER ----------------------------------------------------------
  
  plot_buffalo_psi_elev
  plot_buffalo_psi_slope
  plot_bushbuck_psi_treevol
  plot_bushpig_psi_treevol
  plot_eland_psi_cleared
  plot_elephant_psi_m3
  plot_kudu_psi_elev
  plot_impala_psi_elev
  plot_reedbuck_psi_cleared
  plot_roan_psi_m3
  plot_sable_psi_elev
  plot_warthog_psi_elev
  plot_waterbuck_psi_elev
  plot_zebra_psi_cleared
  plot_zebra_psi_m1
  plot_zebra_psi_treevol
  
  #elevation
  (psi_elevationSD_figs <- (plot_buffalo_psi_elev | plot_warthog_psi_elev))
  
  (psi_elevationMEAN_figs <- (plot_kudu_psi_elev | plot_impala_psi_elev) / 
      (plot_sable_psi_elev | plot_waterbuck_psi_elev))
  
  ggsave('ss_occ/figures/occupancy/all_psi_elevSD.png', psi_elevationSD_figs, width = 12, height = 6, dpi = 300)
  ggsave('ss_occ/figures/occupancy/all_psi_elevMEAN.png', psi_elevationMEAN_figs, width = 12, height = 12, dpi = 300)
  
  #slope
  (psi_slope_figs <- plot_buffalo_psi_slope)
  
  ggsave('ss_occ/figures/occupancy/all_psi_slope.png', psi_slope_figs, width = 6, height = 6, dpi = 300)
  
  #tree volume
  (psi_treevol_figs <- (plot_bushbuck_psi_treevol | plot_bushpig_psi_treevol) / 
      (plot_zebra_psi_treevol | plot_spacer()))
  
  ggsave('ss_occ/figures/occupancy/all_psi_treevol.png', psi_treevol_figs, width = 6, height = 6, dpi = 300)
  
  #cleared
  (psi_clearedMEAN_figs <- (plot_reedbuck_psi_cleared | plot_zebra_psi_cleared)) 
  (psi_clearedSD_figs <- (plot_eland_psi_cleared))
  
  ggsave('ss_occ/figures/occupancy/all_psi_clearedMEAN.png', psi_clearedMEAN_figs, width = 12, height = 6, dpi = 300)
  ggsave('ss_occ/figures/occupancy/all_psi_clearedSD.png', psi_clearedSD_figs, width = 6, height = 6, dpi = 300)
  
  #m3
  (psi_M3_figs <- (plot_elephant_psi_m3 | plot_roan_psi_m3))
  ggsave('ss_occ/figures/occupancy/all_psi_m3.png', psi_M3_figs, width = 6, height = 6, dpi = 300)
  
  #m1
  (psi_M1_figs <- (plot_zebra_psi_m1))
  ggsave('ss_occ/figures/occupancy/all_psi_m1.png', psi_M1_figs, width = 6, height = 6, dpi = 300)
  
  ## OR...
  
  (psi_figs_all <- (plot_buffalo_psi_elev + plot_warthog_psi_elev + plot_layout(ncol = 3, widths = c(1, 1, 1))) / 
      (plot_buffalo_psi_slope + plot_layout(ncol = 3, widths = c(1, 1, 1))) / 
      (plot_bushbuck_psi_treevol + plot_bushpig_psi_treevol + plot_zebra_psi_treevol + plot_layout(ncol = 3, widths = c(1, 1, 1))) /
      (plot_reedbuck_psi_cleared + plot_zebra_psi_cleared + plot_layout(ncol = 3, widths = c(1, 1, 1))) /
      (plot_eland_psi_cleared + plot_layout(ncol = 3, widths = c(1, 1, 1))) /
      (plot_elephant_psi_m3 + plot_roan_psi_m3 + plot_layout(ncol = 3, widths = c(1, 1, 1))) /
      (plot_zebra_psi_m1 + plot_layout(ncol = 3, widths = c(1, 1, 1))))
  
  ggsave('ss_occ/figures/occupancy/all_psi.png', psi_figs_all, width = 8, height = 14, dpi = 300)
  ggsave('ss_occ/figures/occupancy/all_psi3.png', psi_figs_all, width = 8, height = 12, dpi = 300)
  
  
  ## OR ...
  
  psi_figs_all_wide <- (plot_buffalo_psi_elev / plot_warthog_psi_elev + plot_annotation('Elevation') + plot_layout(nrow = 3, heights = c(1,1,1))) | 
    (plot_buffalo_psi_slope / plot_spacer() + plot_annotation('Slope') + plot_layout(nrow = 3, heights = c(1,1,1))) |
    (plot_reedbuck_psi_cleared / plot_zebra_psi_cleared + plot_annotation('Cleared (mean)') + plot_layout(nrow = 3, heights = c(1,1,1))) |
    (plot_eland_psi_cleared / plot_spacer() + plot_annotation('Cleared (SD)') + plot_layout(nrow = 3, heights = c(1,1,1))) |
    (plot_bushbuck_psi_treevol / plot_bushpig_psi_treevol / plot_zebra_psi_treevol + plot_annotation('Tree vol') + plot_layout(nrow = 3, heights = c(1,1,1))) |
    (plot_elephant_psi_m3 / plot_roan_psi_m3 / plot_spacer() + plot_annotation('Prop M3') + plot_layout(nrow = 3, heights = c(1,1,1))) |
    (plot_zebra_psi_m1 / plot_spacer() / plot_spacer() + plot_annotation('Prop M1') + plot_layout(nrow = 3, heights = c(1,1,1)))
  psi_figs_all_wide
  
  ggsave('ss_occ/figures/occupancy/all_psi_wide.png', psi_figs_all_wide, width = 14, height = 6, dpi = 300)
