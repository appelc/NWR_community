## Single-season (stacked) occupancy models -- site_seasons as primary periods, weeks as secondary periods

library(data.table)
library(unmarked)
library(dplyr)
library(ggplot2)
library(purrr)
library(knitr)
library(flextable) #to save tables to Word
library(officer) #to save tables to Word
library(stringr)


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

ifelse(!dir.exists(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 'Folder exists already')
ifelse(!dir.exists(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 'Folder exists already')


## READ IN DETECTION DATA ------------------------------------------------------

## Read in detection data
dh_stacked <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')
dh_stacked_before <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked_before.RDS')
dh_stacked_after <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked_after.RDS')

  #Remove site P23-C1. Not sure why there was a second camera here but it was only deployed for 10 wks in 2019
  # and we don't have covariate data for it.
  # Looks like mostly baboon, warthog, small antelope.
  dh_stacked <- lapply(dh_stacked, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  dh_stacked_before <- lapply(dh_stacked_before, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  dh_stacked_after <- lapply(dh_stacked_after, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  
  #QC
  names(dh_stacked)
  head(dh_stacked$elephant) #example
  names(dh_stacked$elephant) #18 weeks per season
  dim(dh_stacked$elephant) #3040/16 seasons = 190 sites
  dim(dh_stacked_before$elephant) #1520/8 seasons = 190 sites
  dim(dh_stacked_after$elephant) #1520/8 seasons = 190 sites
  
    
## READ IN SITE-LEVEL COVARIATES -----------------------------------------------  

  site_covar <- fread('data/cleaned/covariates/cleaned_covariates_scaled.csv')
    names(site_covar)
    (covar_names <- names(site_covar)[grepl('Site|3x3|dist|fence', names(site_covar))])
    head(site_covar[,..covar_names]) #these are standardized 
  
  #convert to same structure as det hist (site x season as rows)
  site_season_names <- data.frame('site_season' = dh_stacked$elephant$site_primary)
  site_season_names$Site <- sapply(strsplit(site_season_names$site_season, '_'), '[', 1)
    table(site_season_names$Site, useNA = 'a') #all have 16 seasons

  site_covar_expanded <- site_covar %>% select(all_of(covar_names)) %>% right_join(site_season_names, by = 'Site')      
    nrow(site_covar_expanded); nrow(site_season_names) #should match
    length(unique(site_covar_expanded$Site)) #should be 190 sites

  #before
  site_season_names_before <- data.frame('site_season' = dh_stacked_before$elephant$site_primary)
  site_season_names_before$Site <- sapply(strsplit(site_season_names_before$site_season, '_'), '[', 1)
    table(site_season_names_before$Site, useNA = 'a') #all have 8 seasons
    
  site_covar_expanded_before <- site_covar %>% select(all_of(covar_names)) %>% right_join(site_season_names_before, by = 'Site')      
    nrow(site_covar_expanded_before); nrow(site_season_names_before) #should match
    length(unique(site_covar_expanded_before$Site)) #should be 190 sites    
    
  #after
  site_season_names_after <- data.frame('site_season' = dh_stacked_after$elephant$site_primary)
  site_season_names_after$Site <- sapply(strsplit(site_season_names_after$site_season, '_'), '[', 1)
    table(site_season_names_after$Site, useNA = 'a') #all have 8 seasons
  
  site_covar_expanded_after <- site_covar %>% select(all_of(covar_names)) %>% right_join(site_season_names_after, by = 'Site')      
    nrow(site_covar_expanded_after); nrow(site_season_names_after) #should match
    length(unique(site_covar_expanded_after$Site)) #should be 190 sites    
    
    
  #list covar I want to use (3x3 FOR NOW)
    (site_covar_names <- names(site_covar_expanded)[!grepl('Site|5x5|Afromontane|Water|Dambo|Rock|Human|site_season', names(site_covar_expanded), ignore.case = FALSE)])
    
    
## MAKE DETECTION COVARIATE FOR SEASON -----------------------------------------
    
  #create an indicator for dry, wet, cool season so that we can estimate different 'p'
  head(site_season_names)
  
  #extract dry/wet/cool part
  site_season_names$season <- as.factor(sapply(strsplit(site_season_names$site_season, '_'), '[', 3))
    table(site_season_names$season, useNA = 'a')
    
  #make new field (0=wet, 1=cool, 2=dry)
  site_season_names$season_indicator <- ifelse(site_season_names$season == 'wet', 0, 
                                           ifelse(site_season_names$season == 'cool', 1, 2))
    table(site_season_names$season_indicator, site_season_names$season, useNA = 'a')
    
  #now replicate across columns so it varies by occasion
  n <- 18 #number of times to replicate (# occasions)
  season_covar <- site_season_names %>% 
    bind_cols(set_names(replicate(n, site_season_names$season, simplify = FALSE), sprintf("%02d", 1:n))) %>%
    select(-c(site_season, Site, season, season_indicator))

  #do for before
  site_season_names_before$season <- sapply(strsplit(site_season_names_before$site_season, '_'), '[', 3)
  site_season_names_before$season_indicator <- ifelse(site_season_names_before$season == 'wet', 0,
                                                  ifelse(site_season_names_before$season == 'cool', 1, 2))
  season_covar_before <- site_season_names_before %>% 
    bind_cols(set_names(replicate(n, site_season_names_before$season, simplify = FALSE), sprintf("%02d", 1:n))) %>%
    select(-c(site_season, Site, season, season_indicator))
  
  #do for after
  site_season_names_after$season <- sapply(strsplit(site_season_names_after$site_season, '_'), '[', 3)
  site_season_names_after$season_indicator <- ifelse(site_season_names_after$season == 'wet', 0, 
                                                 ifelse(site_season_names_after$season == 'cool', 1, 2))
  season_covar_after <- site_season_names_after %>% 
    bind_cols(set_names(replicate(n, site_season_names_after$season, simplify = FALSE), sprintf("%02d", 1:n))) %>%
    select(-c(site_season, Site, season, season_indicator))
  
  
## READ IN SURVEY-LEVEL EFFORT COVARIATE ---------------------------------------

  #effort (number days camera was active per week)
  effort_covar <- fread('data/cleaned/covariates/effort/effort_stacked_3-18.csv', header = TRUE)
  effort_covar_before <- fread('data/cleaned/covariates/effort/effort_stacked_3-18_before.csv', header = TRUE)
  effort_covar_after <- fread('data/cleaned/covariates/effort/effort_stacked_3-18_after.csv', header = TRUE)
    
  #remove site P23-C1 (see above)
  effort_covar <- effort_covar %>% filter(!grepl('P23-C1', site_primary))
  effort_covar_before <- effort_covar_before %>% filter(!grepl('P23-C1', site_primary))
  effort_covar_after <- effort_covar_after %>% filter(!grepl('P23-C1', site_primary))
  
  #QC
  head(effort_covar)
  dim(effort_covar); dim(dh_stacked$elephant) #good, same number of sites (rows)
  dim(effort_covar_before); dim(dh_stacked_before$elephant)
  dim(effort_covar_after); dim(dh_stacked_after$elephant)
  
  #format
  effort_covar <- subset(effort_covar, select = -c(V1, site_primary, fence, class))
  effort_covar_before <- subset(effort_covar_before, select = -c(V1, site_primary, fence, class))
  effort_covar_after <- subset(effort_covar_after, select = -c(V1, site_primary, fence, class))
  
  
## CREATE MODEL INPUTS ---------------------------------------------------------
  
  #create object to store
  occu_inputs <- list()
  naive_occ <- NULL
  
  #loop thru species
  for (ss in names(dh_stacked)){
    
    #extract detection history
    dh_ss <- data.frame(dh_stacked[[ss]])
    rownames(dh_ss) <- dh_ss$site_primary
    dh_ss <- dh_ss %>% dplyr::select(starts_with('X')) #keep only week columns
    
    #convert to 0/1 first or does it matter?
    
    #create input
    occu_input_ss <- unmarkedFrameOccu(y = as.matrix(dh_ss),
                                       obsCovs = list(effort = effort_covar, season = season_covar),
                                       siteCovs = site_covar_expanded)
    
    #calculate naive occ  
    naive_occ_ss <- sum(ifelse(rowSums(dh_ss, na.rm=T)>0,1,0))/nrow(dh_ss)
    naive_occ <- rbind(naive_occ, data.frame('species' = ss, 'naive_occ' = naive_occ_ss))
    
    #store
    occu_inputs[[ss]] <- occu_input_ss
  }
  
  naive_occ
  
  
## MAKE LIST OF UNIVARIATE MODELS ----------------------------------------------
  

## LIST FOCAL SPECIES TO REPORT ------------------------------------------------    

  names(dh_stacked)
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala','kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')

      
## RUN DETECTION MODELS --------------------------------------------------------
  
  #make objects to store results
  p_models <- list()
  p_mod_results <- list()
  p_aic_tables <- list()
  
  #loop thru species
  for (pp in names(dh_stacked)){
    
    print(pp) #to capture species associated with any printouts
    
    #extract input
    occu_input_pp <- occu_inputs[[pp]]

    #run detection models:
    p_null <- occu(formula = ~1 ~1, data = occu_input_pp)
    p_effort <- occu(formula = ~effort ~1, data = occu_input_pp)
    p_season <- occu(formula = ~season ~1, data = occu_input_pp)
    p_effort_season <- occu(formula = ~effort + season ~1, data = occu_input_pp)
    
    #store
    p_models[[pp]] <- list(p_null, p_effort, p_season, p_effort_season)
    names(p_models[[pp]]) <- c(as.character(c(p_null@formula)), as.character(c(p_effort@formula)), 
                               as.character(c(p_season@formula)), as.character(c(p_effort_season@formula)))
    
    #extract model selection results
    p_model_list <- fitList(fits = p_models[[pp]])
    p_mod_sel <- as(modSel(p_model_list, nullmod = '~1 ~ 1'), 'data.frame')
    
    #save full coef estimates and SE
    p_mod_results[[pp]] <- p_mod_sel
    
    #save formatted AIC table
    p_aic_tables[[pp]] <- p_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
                                mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
                                flextable() %>% 
                                colformat_num(big.mark = '') %>%
                                bg(i = ~ model == as.character(c(p_null@formula)), bg = "lightgray") %>%
                                autofit() %>%
                                set_caption(pp)
  }
  
  #non-convergence for: blue_monkey, domestic_cattle, domestic_dog, goat, scrub_hare, side-striped_jackal
  #(no surprises)

  #print AIC tables to Word
  doc <- read_docx()
  for (name in names(p_aic_tables[focal_species])) {
    doc <- doc %>%
      # body_add_par(value = name, style = "heading 2") %>%
      body_add_flextable(p_aic_tables[[name]]) %>%
      body_add_par(value = "")
  }
  print(doc, target = paste('ss_occ/outputs/single_season', Sys.Date(), 'aic_tables_p.docx', sep = '/'))

    #Null model is within 10 AIC of top for:
      #buffalo
      #eland
      #impala
      #kudu
      #reedbuck
      #roan
      #zebra
  
    #Otherwise, top models are:
      #bushbuck: p(effort) and p(effort + season)
      #bushpig: p(effort + season)
      #elephant: p(effort + season) and p(effort)
      #sable: p(effort + season) and p(effort)
      #warthog: p(effort) p(effort + season)
      #waterbuck: p(effort) and p(effort + season)
  
    #Actually, p(effort + season) is within 10 AIC for all species, so go ahead with that?
  
  
  
## RUN UNIVARIATE PSI MODELS ---------------------------------------------------
  
  #create list of univariate models
  (univar_psi <- lapply(c(1, site_covar_names), function(x) as.formula(paste0('~effort + season ~', x)))) #include 1 for dot model too
  names(univar_psi) <- c('null', site_covar_names)

  #make objects to store results
  psi_univar_models <- list()
  psi_univar_mod_results <- list()
  psi_univar_aic_tables <- list()
  
  #loop thru species
  for (nn in focal_species){
    
    print(nn) #to capture species associated with any printouts
    
    #extract input
    occu_input_nn <- occu_inputs[[nn]]
    
    #loop thru psi covariates and run univariate models
    univar_models_nn <- list()
    for (uu in names(univar_psi)){
      mod_str_uu <- univar_psi[[uu]]
      mod_uu <- occu(formula = mod_str_uu, data = occu_input_nn)
      univar_models_nn[[as.character(c(mod_str_uu))]] <- mod_uu
      # aic_table_uu <- rbind(aic_table_uu, data.frame('model' = as.character(c(mod_str_uu)), 'AIC' = mod_uu@AIC))
    }
    
    #extract model selection results
    univar_model_list <- fitList(fits = univar_models_nn)
    univar_mod_sel <- as(modSel(univar_model_list, nullmod = '~effort + season ~ 1'), 'data.frame')
    
    #save full coef estimates and SE
    psi_univar_mod_results[[nn]] <- univar_mod_sel
    
    #save formatted AIC table
    psi_univar_aic_tables[[nn]] <- univar_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
          mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
          flextable() %>% 
          colformat_num(big.mark = '') %>%
          bg(i = ~ model == as.character('~effort + season ~ 1'), bg = "lightgray") %>%
          autofit() %>%
          set_caption(nn)
    
    #store models
    psi_univar_models[[nn]] <- univar_models_nn
  }
  
  #print tables to Word
  doc2 <- read_docx()
  for (name in names(psi_univar_aic_tables[focal_species])) {
    doc2 <- doc2 %>%
      # body_add_par(value = name, style = "heading 2") %>%
      body_add_flextable(psi_univar_aic_tables[[name]]) %>%
      body_add_par(value = "")
  }
  print(doc2, target = paste('ss_occ/outputs/single_season', Sys.Date(), 'aic_tables_psi_univariate.docx', sep = '/'))
  
  
## CONSTRUCT MULTIVARIATE PSI MODELS -------------------------------------------
  
  #Construct model sets for species with multiple variables within 10 AIC
  # (except impala and roan, for which ALL univariate models were within 10 AIC, including null)
  
  #REMEMBER correlated pairs of covariates:
  # M1 & M3
  # M1 & tree_vol_mean
  # M3 & tree_vol_mean
  # slope_mean & elev_sd
  
  site_covar_names
  
  #make list to store top covar for each species
  multivar_list <- list()
  
  #view AIC tables for each:
  format_table(aic_tables_psi_univar$buffalo, 'buffalo')
  multivar_list[['buffalo']] <- c('dist_human_sd_3x3','tree_vol_mean_3x3','prop_M3_3x3','pct_slope_sd_3x3','fence')
    #removed prop_M1
  
  format_table(aic_tables_psi_univar$eland, 'eland')
  multivar_list[['eland']] <- c('dist_human_sd_3x3','prop_M2_3x3','dist_dambo_sd_3x3')
  
  format_table(aic_tables_psi_univar$elephant, 'elephant')
  multivar_list[['elephant']] <- c('tree_vol_mean_3x3','pct_slope_mean_3x3')
    #removed pct_slope_sd
  
  format_table(aic_tables_psi_univar$waterbuck, 'waterbuck')
  multivar_list[['waterbuck']] <- c('pct_slope_mean_3x3','elev_sd_3x3','dist_human_mean_3x3','dist_dambo_mean_3x3',
                     'prop_M3_3x3','fence','dist_river','tree_vol_mean_3x3')
    #removed prop_M1, dist_dambo_sd_3x3, dist_human_sd_3x3,tree_vol_sd_3x3
    #also include null!
  
  format_table(aic_tables_psi_univar$zebra, 'zebra')
  multivar_list[['zebra']] <- c('dist_dambo_mean_3x3','prop_M1_3x3','dist_river','prop_M2_3x3','fence')

  #make list of all combinations
  multivar_combos <- list()
  for (gg in names(multivar_list)){
    
    #find all combos
    covars_gg <- multivar_list[[gg]]
    covar_combos <- lapply(1:length(covars_gg), function(x) combn(covars_gg, x, simplify = FALSE))
    covar_combos <- unlist(covar_combos, recursive = FALSE)
    
    #turn into formulas
    covar_combo_forms <- lapply(covar_combos, function(covars_gg) {
      as.formula(paste0('~effort + season ~', paste(covars_gg, collapse = " + ")))
    })
    
    #store
    multivar_combos[[gg]] <- covar_combo_forms
  }

  multivar_combos$zebra
  
  
## RUN MULTIVARIATE PSI MODELS -------------------------------------------------
  
  #make objects to store results
  psi_multivar_models <- list()
  psi_multivar_mod_results <- list()
  psi_multivar_aic_tables <- list()
  
  #loop thru species
  for (dd in names(multivar_list)){
    
    print(dd) #print species name to capture any associated printouts
    
    #extract input and model list
    occu_input_dd <- occu_inputs[[dd]]
    model_list_dd <- multivar_combos[[dd]] #names will be generic numbers now

    #loop thru psi covariates and run multivariate models
    multivar_models_dd <- list()
    for (mm in 1:length(model_list_dd)){
      mod_str_mm <- model_list_dd[[mm]]
      mod_mm <- occu(formula = mod_str_mm, data = occu_input_dd)
      multivar_models_dd[[as.character(c(mod_str_mm))]] <- mod_mm
      # aic_table_dd <- rbind(aic_table_dd, data.frame('model' = as.character(c(mod_str_mm)), 'AIC' = mod_mm@AIC))
    }
    
    #append null model
    null_mod_dd <- occu(formula = ~effort + season ~1, data = occu_input_dd)
    # aic_table_dd <- rbind(aic_table_dd, data.frame('model' = as.character(c(null_mod_dd@formula)), 'AIC' = null_mod_dd@AIC))
    multivar_models_dd[[as.character(c(null_mod_dd@formula))]] <- null_mod_dd
    
    #extract model selection results
    multivar_model_list <- fitList(fits = multivar_models_dd)
    multivar_mod_sel <- as(modSel(multivar_model_list), 'data.frame')
    
    #save full coef estimates and SE
    psi_multivar_mod_results[[dd]] <- multivar_mod_sel
    
    #save formatted AIC table
    psi_multivar_aic_tables[[dd]] <- multivar_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
                                              mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
                                              flextable() %>% 
                                              colformat_num(big.mark = '') %>%
                                              bg(i = ~ model == as.character('~effort + season ~ 1'), bg = "lightgray") %>%
                                              bg(which.max(multivar_mod_sel$nPars), bg = "lightblue") %>% #shade global model here too
                                              autofit() %>%
                                              set_caption(dd)
    
    #store models
    psi_multivar_models[[dd]] <- multivar_models_dd
  }
  
  #print tables to Word
  doc3 <- read_docx()
  for (name in names(psi_multivar_aic_tables)) {
    doc3 <- doc3 %>%
      # body_add_par(value = name, style = "heading 1") %>%
      body_add_flextable(psi_multivar_aic_tables[[name]]) %>%
      body_add_par(value = "")
  }
  print(doc3, target = paste('ss_occ/outputs/single_season', Sys.Date(), 'aic_tables_psi_multivariate2.docx', sep = '/'))
  
  
  
## -------------------------------------------------
  
## PLOT MODEL COEFFICIENTS -----------------------------------------------------
  
  #source functions to process model outputs
  source('ss_occ/occ_mod_functions.R')
  
  #plot coefficient estimates (p)
  param_tables_p <- data.frame()
  for (ee in focal_species){
    model_names <- ls(p_models[[ee]])
    for (model_name in model_names){
      model_object <- p_models[[ee]][[model_name]]
      model_table <- param_table(model_object, 0.95, model_name)
      model_table$species <- ee
      param_tables_p <- rbind(param_tables_p, model_table)
    }
  }
  param_tables_p$param <- ifelse(grepl('psi', param_tables_p$covar), 'psi','p')
  write.csv(param_tables_p, paste('ss_occ/outputs/single_season', Sys.Date(), 'p_model_parameters.csv', sep = '/'))
  
  #plot coefficient estimates (psi - univariate)
  param_tables_psi_univariate <- data.frame()
  for (ff in focal_species){
    model_names <- ls(psi_univar_models[[ff]])
    for (model_name in model_names){
      model_object <- psi_univar_models[[ff]][[model_name]]
      model_table <- param_table(model_object, 0.95, model_name)
      model_table$species <- ff
      param_tables_psi_univariate <- rbind(param_tables_psi_univariate, model_table)
    }
  }
  param_tables_psi_univariate$param <- ifelse(grepl('psi', param_tables_psi_univariate$covar), 'psi','p')
  #save
  write.csv(param_tables_psi_univariate, paste('ss_occ/outputs/single_season', Sys.Date(), 'psi_univariate_model_parameters.csv', sep = '/'))
  
  #plot coefficient estimates (psi - multivariate)
  param_tables_psi_multivariate <- data.frame()
  for (gg in names(psi_multivar_models)){
    model_names <- ls(psi_multivar_models[[gg]])
    for (model_name in model_names){
      model_object <- psi_multivar_models[[gg]][[model_name]]
      model_table <- param_table(model_object, 0.95, model_name)
      model_table$species <- gg
      param_tables_psi_multivariate <- rbind(param_tables_psi_multivariate, model_table)
    }
  }
  param_tables_psi_multivariate$param <- ifelse(grepl('psi', param_tables_psi_multivariate$covar), 'psi','p')
  #save
  write.csv(param_tables_psi_multivariate, paste('ss_occ/outputs/single_season', Sys.Date(), 'psi_multivariate_model_parameters.csv', sep = '/'))
  

  #plot (p)
  sort(unique(param_tables_p$mod_name))
  model_to_plot <- '~effort + season ~ 1' 

  p1 <- ggplot(param_tables_p[param_tables_p$mod_name %in% model_to_plot,],
               aes(x = covar, y = beta)) + #, color = group
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbar(aes(ymin = LCI_95, ymax = UCI_95), width = 0.2,
                  position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_wrap(~species, nrow = 3, ncol = 5, scales = 'free_x') +
    # ylim(c(-2.5,2.5)) +
    theme_bw() +
    labs(title = model_to_plot) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  p1
  
  #plot (psi - univariate)
  param_tables_psi_univariate <- param_tables_psi_univariate %>% mutate(covar_name = str_extract(covar, "(?<=\\().+?(?=\\))"))
    
  sort(unique(param_tables_psi_univariate$mod_name))
  model_to_plot <- '~effort + season ~ 1' 
  
  psi1 <- param_tables_psi_univariate %>% filter(param == 'psi') %>%
    ggplot(aes(x = covar_name, y = beta))

  psi1 + #ggplot(param_tables_psi_univariate, aes(x = covar, y = beta)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbar(aes(ymin = LCI_95, ymax = UCI_95), width = 0.2,
                  position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_wrap(~species, nrow = 3, ncol = 5, scales = 'free_x') +
    ylim(c(-2.5,2.5)) +
    theme_bw() +
    labs(title = model_to_plot) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  psi1
  
  #plot (psi - multivariate)
  
  
## PREDICTIONS AND MARGINAL PLOTS -----------------------------------------------

  ## Start with NULL models (impala, roan, waterbuck)
  
  #need covarss for plotting and covar means?
  
  ## And univariate psi models (bushbuck, bushpig, kudu, reedbuck, sable, warthog)
  
  
  
  
  
  
  
  