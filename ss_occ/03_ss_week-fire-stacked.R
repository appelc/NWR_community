## Single-season (stacked) occupancy models -- each week w/ fire

library(data.table)
library(unmarked)
library(dplyr)
library(ggplot2)


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

ifelse(!dir.exists(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 'Folder exists already')
ifelse(!dir.exists(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 'Folder exists already')


## CREATE MODEL INPUTS ---------------------------------------------------------

  ## Read in detection data
  dh_stacked <- readRDS('data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')
  dh_stacked_before <- readRDS('data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked_before.RDS')
  dh_stacked_after <- readRDS('data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked_after.RDS')
  
    #sort by hex and remove that column?

  ## Read in survey-level covariate data
  fire_covar <- read.csv('data/cleaned/covariates/fire_1_7_stacked.csv', header = TRUE)
  rownames(fire_covar) <- fire_covar$site_primary
  fire_covar <- subset(fire_covar, select = -c(X, site_primary))
    
    #sort by site and remove that column?

  ## Create site covariate dataframe (pull from 1st species; should all be the same)
  dh_stacked_templ <- tibble::as_tibble(dh_stacked[[1]])
  site_covar <- dh_stacked_templ %>% dplyr::select(site_primary, fence, calendar_week, season)

    #QC:
    length(unique(dh_stacked_templ$site_primary)) #good, same -- I have all sites in det history now
    length(unique(site_covar$site_primary)) #good, same
    
    #any to standardize?
  

    # ## Create site covariate dataframe
    # siteCovar <- data.frame(hex_data[hex_data$hex_padded %in% rownames(dh_combined[[1]]), #keep only the 69 hexagons in det histories
    #                                  c('M1_prop_z','M2_prop_z','M3_prop_z','TREEVOL_z','SLOPE_MEAN_z','SLOPE_SD_z',
    #                                    'DAMBO_DIST_z','FIRE_YRS_z','ELEV_MEAN_z','ELEV_SD_z','COST_ELEV_z','BuaRiver_indicator',
    #                                    'hex_padded')]) #use these columns

  
  ## Create list of univariate models
  (univar_psi <- lapply(c(1, colnames(site_covar)), function(x) as.formula(paste('psi ~', x)))) #include 1 for dot model too
  
  ## Set up dataframe for model outputs
  mod_results <- structure(list(model = character(), species = character(), aic = numeric()), class = 'data.frame')
  
  ## Initialize list to store results
  univar_models = list()
  
  
## TEST FOR CORRELATIONS (DO THIS BEFORE THIS SCRIPT)
  
  # cov_corr <- cor(site_covar, method = 'spearman')
  #no continuous covar right now (well, just calendar_week)
    
    
## RUN UNIVARIATE MODELS -------------------------------------------------------
  
  ## Loop through all univariate models
  
  #for each species:
  for (ss in names(dh_stacked)){
    
    names(dh_stacked)
    #ss='buffalo'
    
    #extract detection history
    dh_ss <- data.frame(dh_stacked[[ss]])
    rownames(dh_ss) <- dh_ss$site_primary
    dh_ss <- dh_ss %>% dplyr::select(starts_with('X')) #keep only week columns
    
    #convert to 0/1 first or does it matter?
    
    #create input
    occu_input_ss <- unmarkedFrameOccu(y = as.matrix(dh_ss),
                                       obsCovs = list(fire = fire_covar),
                                       siteCovs = site_covar)
      summary(occu_input_ss)
      
    (naive_occ <- sum(ifelse(rowSums(dh_ss, na.rm=T)>0,1,0))/nrow(dh_ss))

    #null model
    null_mod <- occu(formula = ~1 ~1, data = occu_input_ss)
      
      # #back-transform
      # p_ss <- backTransform(null_mod, type = 'det')
      # psi_ss <- backTransform(null_mod, type = 'state')
    
    #fire-only det model
    fire_mod <- occu(formula = ~fire ~1, data = occu_input_ss)
    
    #quadratic fire-only det model
    #fire_mod_quad <- occu(formula = ~fire^2 ~1, data = occu_input_ss)
    
    #run full model (season)
    global_mod_season <- occu(formula = ~fire ~fence + season, data = occu_input_ss)
      
    #run full model (calendar week)
    global_mod_week <- occu(formula = ~fire ~fence + calendar_week, data = occu_input_ss)

    #run fence model
    fence_mod <- occu(formula = ~fire ~fence, data = occu_input_ss)

    #run season model
    season_mod <- occu(formula = ~fire ~season, data = occu_input_ss)
    
    #run week model
    week_mod <- occu(formula = ~fire ~calendar_week, data = occu_input_ss)
    
    #rank
    aic_table <- data.frame('model' = c('null',
                                        'p(fire)',
                                        # 'p(fire^2)',
                                        'p(fire) psi(fence+season)',
                                        'p(fire) psi(fence+week)',
                                        'p(fire) psi(fence)',
                                        'p(fire) psi(season)',
                                        'p(fire) psi(season)'),
                            'AIC' = c(null_mod@AIC, 
                                      fire_mod@AIC, 
                                      # fire_mod_quad@AIC,
                                      global_mod_season@AIC, 
                                      global_mod_week@AIC,
                                      fence_mod@AIC, 
                                      season_mod@AIC, 
                                      week_mod@AIC))
    aic_table[order(aic_table$AIC, decreasing = FALSE),]
    
    #Predict from top model(s)
      
    #predict fire (detection)
    predict_fire <- cbind(predict(global_mod_season, 
                                  newdata = data.frame(fire = seq(min(fire_covar, na.rm = TRUE), max(fire_covar, na.rm = TRUE), by = 1)
                                                       # fence = 'Inside',
                                                       # season = 'dry',
                                                       # calendar_week = mean(site_covar$calendar_week)
                                                       ), type = 'det'),
                          data.frame(fire = seq(min(fire_covar, na.rm = TRUE), max(fire_covar, na.rm = TRUE), by = 1)))
    head(predict_fire)
    
    (det_fire_ss <- ggplot(data = predict_fire, aes(x = fire, y = Predicted)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue") +
      stat_smooth(method = "loess", col = "black", se = FALSE) +
      ylim(c(0,1)) +
      labs(x = "Days since fire", y = "Detection probability", title = ss) +
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)))
    
      ggsave(paste('ss_occ/figures/single_season/', Sys.Date(), '/detection_fire_', ss, '.png', sep = ''), det_fire_ss)
    
    
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

    
        
######      
    #for each univariate structure:
    univar_models_ss <- list()  
    for (uu in univar_psi){
      
      #construct model name
      modname_uu = paste('Malawi/outputs/models/', Sys.Date(), '/', ss, '_pDOT', gsub(' ~ ', '', as.character(c(uu))), sep = '')
      
      #run model
      model_uu <- occMod(model = list(uu, p~1), data = input_ss, type = 'so', modname = modname_uu, outfile = 'modname_uu')
      
      #store results
      univar_models_ss[[as.character(c(uu))]] <- model_uu
      modResults[nrow(modResults)+1,] <- c(paste('pDOT', gsub(' ~ ', '', as.character(c(uu))), sep = ''), ss, model_uu$aic)
      # modResults[nrow(modResults)+1,] <- c(paste('pDOT', gsub(' ~ ', '', as.character(c(uu))), sep = ''), ss, 
      #                                      gsub('psi ~ ', '', as.character(c(uu))), #cov
      #                                      model_uu$real$psi$est[1], model_uu$real$psi$se[1], #psi_est and psi_se
      #                                      model_uu$real$p$est[1], model_uu$real$p$se[1], model_uu$aic) #p_est and p_se and AIC
    } #end univar loop
    
    univar_models[[ss]] <- univar_models_ss
    
  } #end species loop
  
  #inspect
  names(univar_models)
  names(univar_models$elephant)
  
  head(modResults)
  tail(modResults)
  