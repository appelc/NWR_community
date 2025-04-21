## Single-season (stacked) occupancy models -- site_seasons as primary periods, weeks as secondary periods

library(data.table)
library(unmarked)
library(tidyverse)
library(knitr)
library(rlang)
library(flextable) #to save tables to Word
library(officer) #to save tables to Word


## READ IN DETECTION DATA ------------------------------------------------------

## Read in detection data
dh_stacked <- readRDS('data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')

  #Remove site P23-C1. Not sure why there was a second camera here but it was only deployed for 10 wks in 2019
  # and we don't have covariate data for it. Looks like mostly baboon, warthog, small antelope.
  dh_stacked <- lapply(dh_stacked, function(x) {x %>% filter(!grepl('P23-C1', site_primary))})
  
  #Make sure they're sorted by site
  dh_stacked <- lapply(dh_stacked, function(y) {y %>% arrange(site_primary)})
  
  #QC
  names(dh_stacked)
  head(dh_stacked$elephant); tail(dh_stacked$elephant) #example
  names(dh_stacked$elephant) #18 weeks per season
  dim(dh_stacked$elephant) #3040/16 seasons = 190 sites
  
  
## SET DATES FOR STRUCTURAL VARIABLES ------------------------------------------
  
  #set years for before/after fence decommissioning (approx between 2020_wet and 2021_cool?)
  fence_years_before <- c('2018|2019|2020')
  fence_years_after <- c('2021|2022|2023')
  
  #set seasons before/after supplemental releases (June-July 2022)
  releases_seasons_after <- c('2022_cool|2022_dry|2022_wet|2023_cool|2023_dry') #everything else will be 'before'    

      
## READ IN SITE-LEVEL COVARIATES -----------------------------------------------  

  site_covar <- fread('data/cleaned/covariates/cleaned_covariates_scaled.csv')
    names(site_covar)
    (covar_names <- names(site_covar)[grepl('Site|3x3|dist|fence', names(site_covar))])
    head(site_covar[,..covar_names]) #these are standardized 
  
  #convert to same structure as det hist (site x season as rows)
  site_season_names <- data.frame('site_season' = dh_stacked$elephant$site_primary)
  site_season_names$Site <- sapply(strsplit(site_season_names$site_season, '_'), '[', 1)
    # table(site_season_names$Site, useNA = 'a') #all have 16 seasons
    table(table(site_season_names$Site))

  site_covar_expanded <- site_covar %>% select(all_of(covar_names)) %>% right_join(site_season_names, by = 'Site')      
    nrow(site_covar_expanded); nrow(site_season_names) #should match
    length(unique(site_covar_expanded$Site)) #should be 190 sites

  #make sure it's sorted by site
  site_covar_expanded <- site_covar_expanded %>% arrange(site_season)
  rownames(site_covar_expanded) <- site_covar_expanded$site_season
  
  #add 'season' field (cool, dry, wet)
  site_covar_expanded$season <- as.factor(sapply(strsplit(site_covar_expanded$site_season, '_'), '[', 3))
  
  #also add field for before/after fence decommissioning
  site_covar_expanded <- site_covar_expanded %>% 
                         mutate(period = case_when(grepl(fence_years_before, site_season) ~ 'Fence',
                                                   grepl(fence_years_after, site_season) ~ 'NoFence'))
  
  #also add field for before/after supplemental releases in 2022
  site_covar_expanded <- site_covar_expanded %>% 
                         mutate(releases = if_else(grepl(releases_seasons_after, site_season), 'After', 'Before'))
    
  #use South/North for 'fence' instead of 0/1
  site_covar_expanded$fence <- ifelse(site_covar_expanded$fence == 0, 'North', 'South')
  
  #list covar I want to use (3x3 VERSIONS FOR NOW)
    (envir_vars <- names(site_covar_expanded)[!grepl('Site|5x5|Afromontane|Water|Dambo|Rock|Cleared|site_season', 
                                                           names(site_covar_expanded), ignore.case = FALSE)])
        #keep fence, season, period, releases for now?
  
    
## MAKE DETECTION COVARIATE FOR SEASON -----------------------------------------
    
  #create an indicator for dry, wet, cool season so that we can estimate different 'p'
  head(site_season_names)
  
  #extract dry/wet/cool part
  site_season_names$season <- as.factor(sapply(strsplit(site_season_names$site_season, '_'), '[', 3))
    table(site_season_names$season, useNA = 'a')
    
  #now replicate across columns so it varies by occasion
  n <- 18 #number of times to replicate (# occasions)
  season_covar <- site_season_names %>% 
    bind_cols(set_names(replicate(n, site_season_names$season, simplify = FALSE), sprintf("%02d", 1:n))) %>%
    arrange(site_season) %>% #sort by site_season before removing this column
    select(-c(site_season, Site, season))

  
## READ IN SURVEY-LEVEL EFFORT COVARIATE ---------------------------------------

  #effort (number days camera was active per week)
  effort_covar <- fread('data/cleaned/covariates/effort/effort_stacked_3-18.csv', header = TRUE)
    
  #remove site P23-C1 (see above)
  effort_covar <- effort_covar %>% filter(!grepl('P23-C1', site_primary))
  
  #make sure it's sorted by site
  effort_covar <- effort_covar %>% arrange(site_primary)

  #QC 
  head(effort_covar)
  dim(effort_covar); dim(dh_stacked$elephant) #good, same number of sites (rows)
  
  #format
  rownames(effort_covar) <- effort_covar$site_primary
  effort_covar <- subset(effort_covar, select = -c(V1, site_primary, fence, class))
  
  #SUMMARIZE FOR TABLE 1 (with 0s removed)
  summary(unlist(effort_covar)[unlist(effort_covar) > 0], na.rm = TRUE) #mean = 2.3
  sd(unlist(effort_covar)[unlist(effort_covar) > 0], na.rm = TRUE) #SD = 3.2
  
  
## LIST FOCAL SPECIES TO REPORT ------------------------------------------------    
  
  names(dh_stacked)
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
    #add duiker / small antelope?
  
  
## CREATE MODEL INPUTS ---------------------------------------------------------
 
  #create object to store
  occu_inputs <- list()
  naive_occ <- NULL
  
  #loop thru species
  for (aa in names(dh_stacked)){
    
    #extract detection history
    dh_aa <- data.frame(dh_stacked[[aa]])
    rownames(dh_aa) <- dh_aa$site_primary
    
    #convert to 0/1
    # dh_aa <- dh_aa %>% mutate(acroaa(.cols = starts_with('X'), 
    #                                  .fns = ~ ifelse(is.na(.), NA, ifelse(. > 0, 1, 0))))
    
    #add field for before/after fence decommissioning to det history for naive occ calculations
    dh_aa <- dh_aa %>% mutate(period = case_when(grepl(fence_years_before, site_primary) ~ 'Fence',
                                                 grepl(fence_years_after, site_primary) ~ 'NoFence'))
    
    #replace 'fence' with correct 'fence' field from site_covar
    dh_aa$fence <- site_covar_expanded$fence[match(dh_aa$site_primary, site_covar_expanded$site_season)]
    # dh_aa$fence <- ifelse(dh_aa$fence == 0, 'North', 'South')

    #calculate naive occupancy
    naive_occ_aa <- dh_aa %>%
      mutate(across(starts_with('X'), ~ ifelse(is.na(.), 0, .))) %>%  #convert NA with 0s
      summarize(total_det = sum(across(starts_with('X')), na.rm = TRUE), 
                naive_occ = mean(rowSums(across(starts_with('X')) > 0, na.rm = TRUE) > 0)) %>%
      bind_rows(
        dh_aa %>% group_by(period, fence) %>% 
          summarize(total_det = sum(across(starts_with('X')), na.rm = TRUE),
                    naive_occ = mean(rowSums(across(starts_with('X')) > 0, na.rm = TRUE) > 0), .groups = 'drop')) %>%
          mutate(species = aa, period = replace_na(period, 'all'), fence = replace_na(fence, 'both'))
    naive_occ <- rbind(naive_occ, naive_occ_aa)
    
    #keep only week columns
    dh_aa <- dh_aa %>% dplyr::select(starts_with('X')) 
    
    #create input (all)
    occu_input_aa <- unmarkedFrameOccu(y = as.matrix(dh_aa),
                                       obsCovs = list(effort = effort_covar, season = season_covar),
                                       siteCovs = site_covar_expanded)
    #store
    occu_inputs[[aa]] <- occu_input_aa
  }
  #warnings are about converting characters to factors
  
  #inspect:
  names(occu_inputs)
  head(occu_inputs$elephant)
  
  head(naive_occ)
  sort(unique(naive_occ$species))
  
  #export naive_occ table for paper (TABLE 2)
  naive_occ_table <- naive_occ %>% filter(period != 'all') %>%
            pivot_wider(names_from = c(period, fence), values_from = c(naive_occ, total_det), names_sort = TRUE) %>%
          #reorder columns:
            # select(1, rev(colnames(.)[-1])) %>%
            select(1,total_det_Fence_South,total_det_Fence_North,total_det_NoFence_South,total_det_NoFence_North,
                   naive_occ_Fence_South,naive_occ_Fence_North,naive_occ_NoFence_South,naive_occ_NoFence_North) %>% 
            #round naive occ columns
            mutate(across(starts_with('naive_occ',), ~ round(.x, 2))) %>%
            filter(species %in% focal_species) %>%
            flextable() %>% 
            colformat_num(big.mark = '') 
  
  #print to Word
  doc0 <- read_docx()
  doc0 <- doc0 %>%
    body_add_par('Total detections and naive occupancy ', style = 'heading 1') %>%
    body_add_flextable(naive_occ_table) %>%
    body_add_par(value = '')
  print(doc0, target = paste('ss_occ/outputs/single_season', 'naive_occ_target_sp.docx', sep = '/'))
  
  #save inputs as R object
  saveRDS(occu_inputs, 'ss_occ/occu_inputs.RDS')
  
  
## RUN DETECTION MODELS --------------------------------------------------------
  
  # Run if necessary:
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  
  #make objects to store results
  p_models <- list()
  p_aic_tables <- list()
  
  #loop thru species
  for (bb in focal_species){
    cat('Processing:', bb, '\n') #to capture species associated with any printouts
    
    #extract input
    occu_input_bb <- occu_inputs[[bb]]
    
    #run detection models:
    p_null <- occu(formula = ~1 ~1, data = occu_input_bb)
    p_effort <- occu(formula = ~effort ~1, data = occu_input_bb)
    p_season <- occu(formula = ~season ~1, data = occu_input_bb)
    p_effort_season <- occu(formula = ~effort + season ~1, data = occu_input_bb)
    
    #store
    p_models[[bb]] <- list(p_null, p_effort, p_season, p_effort_season)
    names(p_models[[bb]]) <- c(as.character(c(p_null@formula)), as.character(c(p_effort@formula)), 
                               as.character(c(p_season@formula)), as.character(c(p_effort_season@formula)))
    
    #extract model selection results
    p_model_list <- fitList(fits = p_models[[bb]])
    p_fit_list <- as(modSel(p_model_list, nullmod = '~1 ~ 1'), 'data.frame')
    
    #save formatted AIC table
    p_aic_tables[[bb]] <- p_fit_list %>% select(model, nPars, AIC, delta, AICwt, cumltvWt) %>% #add betas here??
      mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%  
      flextable() %>% 
      colformat_num(big.mark = '') %>%
      bg(i = ~ model == as.character(c(p_null@formula)), bg = "lightgray") %>%
      # autofit() %>%
      set_caption(bb)
  }

  #inspect
  names(p_models)
  names(p_aic_tables)
  
  #print AIC tables to Word
  doc1 <- read_docx()
  for(cc in focal_species){
    doc1 <- doc1 %>% 
      body_add_flextable(p_aic_tables[[cc]]) %>%
      body_add_par(value = '')
  }
  print(doc1, target = paste('ss_occ/outputs', 'aic_tables_p.docx', sep = '/'))
  
  #save models
  saveRDS(p_models, 'ss_occ/outputs/model_objects_p.RDS')


## Store top detection models for each species ---------------------------------
  
  p_top_mods <- list()
  
  #view AIC table for each
  p_aic_tables$buffalo
  p_top_mods[['buffalo']] <- c(formula('~effort + season ~ 1'),
                               formula('~1 ~ 1'))
    
  p_aic_tables$bushbuck
  p_top_mods[['bushbuck']] <- c(formula('~effort ~ 1'),
                              formula('~effort + season ~ 1'))
  
  p_aic_tables$bushpig
  p_top_mods[['bushpig']] <- c(formula('~effort + season ~ 1'))
  
  p_aic_tables$eland
  p_top_mods[['eland']] <- c(formula('~effort + season ~ 1'),
                             formula('~season ~ 1'),
                             formula('~effort ~ 1'),
                             formula('~1 ~ 1'))
  
  p_aic_tables$elephant
  p_top_mods[['elephant']] <- c(formula('~effort + season ~ 1'))
  
  p_aic_tables$impala
  p_top_mods[['impala']] <- c(formula('~1 ~ 1'),
                              formula('~effort ~ 1'),
                              formula('~season ~ 1'))
  
  p_aic_tables$kudu
  p_top_mods[['kudu']] <- c(formula('~effort ~ 1'),
                            formula('~effort + season ~ 1'))
  
  p_aic_tables$reedbuck
  p_top_mods[['reedbuck']] <- c(formula('~effort + season ~ 1'),
                                formula('~effort ~ 1'))
  
  p_aic_tables$roan
  p_top_mods[['roan']] <- c(formula('~1 ~ 1'),
                            formula('~effort ~ 1'),
                            formula('~season ~ 1'),
                            formula('~effort + season ~ 1'))
  
  p_aic_tables$sable
  p_top_mods[['sable']] <- c(formula('~effort + season ~ 1'))
  
  p_aic_tables$warthog
  p_top_mods[['warthog']] <- c(formula('~effort ~ 1'),
                               formula('~effort + season ~ 1'))
  
  p_aic_tables$waterbuck
  p_top_mods[['waterbuck']] <- c(formula('~effort ~ 1'),
                                 formula('~effort + season ~ 1'))
  
  p_aic_tables$zebra
  p_top_mods[['zebra']] <- c(formula('~effort ~ 1'),
                             formula('~effort + season ~ 1'))

    #inspect and save
    p_top_mods
    saveRDS(p_top_mods, 'ss_occ/outputs/top_models_p.RDS')
    
    
## RUN UNIVARIATE PSI MODELS (ENVIRONMENTAL VARS) ------------------------------
  
  #source functions to process model output
  source('ss_occ/occ_mod_functions.R')
    
  #run if necessary:
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  envir_vars <- c("elev_mean_3x3","elev_sd_3x3","pct_slope_mean_3x3","pct_slope_sd_3x3","tree_vol_mean_3x3","tree_vol_sd_3x3",
                        "dist_dambo_mean_3x3","dist_dambo_sd_3x3","dist_cleared_mean_3x3","dist_cleared_sd_3x3","dist_river","prop_M1_3x3",
                        "prop_M2_3x3","prop_M3_3x3")  #don't use ("fence","period","releases") yet!
  
  #create list of univariate models
  (univar_psi <- lapply(c(1, envir_vars), function(x) as.formula(paste0('~1 ~', x)))) #include 1 for dot model too
  names(univar_psi) <- c('null', envir_vars)

  #make objects to store results
  psi_univar_models <- list()
  psi_univar_aic_tables <- list()
  
  #loop thru species
  for (hh in focal_species){
    
    #extract input
    occu_input_hh <- occu_inputs[[hh]]
    
    #loop thru psi covariates and run univariate models
    univar_models_hh <- list()
    univar_models_coef <- NULL
    for (ii in names(univar_psi)){
      mod_str_ii <- univar_psi[[ii]]
      cat('Processing:', hh, '     Model:', as.character(c(mod_str_ii)), '\n')
      mod_ii <- occu(formula = mod_str_ii, data = occu_input_hh)
      
      #store model and coef/CI
      mod_ii_coef <- param_table(mod_ii, 0.95, as.character(c(mod_ii@formula))) %>% 
                              extract(covar, into = c('param','variable'), regex = '([^()]+)\\(([^()]+)\\)')
      univar_models_coef <- rbind(univar_models_coef, mod_ii_coef)
      univar_models_hh[[as.character(c(mod_ii@formula))]] <- mod_ii
    }
    
    #extract model selection results
    univar_model_list <- fitList(fits = univar_models_hh)
    univar_fit_list <- as(modSel(univar_model_list, nullmod = '~1 ~ 1'), 'data.frame')
    
    #filter coef/CI for psi variables only
    univar_models_coef_psi <- univar_models_coef %>% filter(param == 'psi', variable != 'Int')
      
    #save formatted AIC table
    psi_univar_aic_tables[[hh]] <- univar_fit_list %>% left_join(univar_models_coef_psi, join_by(model == mod_name)) %>%
                                                       select(model, nPars, AIC, delta, AICwt, beta, LCI_95, UCI_95) %>%
                                                       mutate(across(c(AIC, delta, AICwt, beta, LCI_95, UCI_95), ~ round(.x, 2))) %>%  
                                                       flextable() %>% 
                                                       colformat_num(big.mark = '') %>%
                                                       bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>%
                                                       # autofit() %>%
                                                       set_caption(hh)
    
    #store models
    psi_univar_models[[hh]] <- univar_models_hh
  }
  #hessian is singular for: roan ~ prop_M3
  
  names(psi_univar_models)
  names(psi_univar_models$buffalo)
  names(psi_univar_aic_tables)
  
  #print AIC tables to Word
  doc2 <- read_docx()
    for(jj in focal_species){
      doc2 <- doc2 %>% 
        body_add_flextable(psi_univar_aic_tables[[jj]]) %>%
        body_add_par(value = '')
    }
  print(doc2, target = paste('ss_occ/outputs', 'aic_tables_psi_univariate.docx', sep = '/'))
  
  #store models
  saveRDS(psi_univar_models, 'ss_occ/outputs/model_objects_psi_univariate.RDS')
  
  
## Store top psi covariates for each species -----------------------------------
  
  psi_top_covars <- list()
  
  #view AIC table for each
  psi_univar_aic_tables$buffalo
  psi_top_covars[['buffalo']] <- c('elev_sd_3x3','pct_slope_sd_3x3','pct_slope_mean_3x3') #or remove 'pct_slope_mean_3x3'
  
  psi_univar_aic_tables$bushbuck
  psi_top_covars[['bushbuck']] <- c('tree_vol_mean_3x3')
  
  psi_univar_aic_tables$bushpig
  psi_top_covars[['bushpig']] <- c('tree_vol_mean_3x3')
  
  psi_univar_aic_tables$eland
  psi_top_covars[['eland']] <- c('dist_cleared_sd_3x3')
  
  psi_univar_aic_tables$elephant
  psi_top_covars[['elephant']] <- c('prop_M3_3x3')
  
  psi_univar_aic_tables$impala
  psi_top_covars[['impala']] <- c('pct_slope_mean_3x3', 'dist_cleared_mean_3x3', 'elev_sd_3x3', 'elev_mean_3x3')
  
  psi_univar_aic_tables$kudu
  psi_top_covars[['kudu']] <- c('elev_mean_3x3')
  
  psi_univar_aic_tables$reedbuck
  psi_top_covars[['reedbuck']] <- c('dist_cleared_mean_3x3')
  
  psi_univar_aic_tables$roan
  psi_top_covars[['roan']] <- c('prop_M3_3x3','tree_vol_mean_3x3')
  
  psi_univar_aic_tables$sable
  psi_top_covars[['sable']] <- c('elev_mean_3x3')
  
  psi_univar_aic_tables$warthog
  psi_top_covars[['warthog']] <- c('elev_sd_3x3','pct_slope_mean_3x3') #or just keep 'elev_sd_3x3'
  
  psi_univar_aic_tables$waterbuck
  psi_top_covars[['waterbuck']] <- c('elev_mean_3x3') 
    
  psi_univar_aic_tables$zebra
  psi_top_covars[['zebra']] <- c('dist_cleared_mean_3x3','tree_vol_mean_3x3','prop_M1_3x3') #or just keep 'dist_cleared_mean_3x3'
  
    #inspect and save
    psi_top_covars
    saveRDS(psi_top_covars, 'ss_occ/outputs/top_covars_psi.RDS')

      
## CONSTRUCT MULTIVARIATE PSI MODELS (ENVIR VARS) ------------------------------
  
  #read in if necessary
  psi_top_covars <- readRDS('ss_occ/top_covars_psi.RDS')
    
  #to construct covariate combinations, take only those with >1 variable for this next step
  psi_covars_multi <- psi_top_covars[sapply(psi_top_covars, length) > 1]
  
  #store list of variable pairs that can't be uesd in the same models
  corr_pairs <- list(
                    #first, correlated pairs:
                     c('prop_M1_3x3','prop_M3_3x3'), 
                     c('prop_M1_3x3','tree_vol_mean_3x3'),
                     c('prop_M3_3x3','tree_vol_mean_3x3'), 
                     c('pct_slope_mean_3x3','elev_sd_3x3'),
                    #also exclude both versions of same variable:
                     c('elev_mean_3x3','elev_sd_3x3'),
                     c('pct_slope_mean_3x3','pct_slope_sd_3x3'),   
                     c('tree_vol_mean_3x3','tree_vol_sd_3x3'),     
                     c('dist_dambo_mean_3x3','dist_dambo_sd_3x3'), 
                     c('dist_human_mean_3x3','dist_human_sd_3x3'))
  
  #write function to check for correlated pairs
  check_corr_pair <- function(combo, pairs_list) {any(sapply(pairs_list, function(pair) all(pair %in% combo)))}

  #make list of all combinations
  multivar_combos <- list()
  for (kk in names(psi_covars_multi)){
    
    #find all combos
    covars_kk <- psi_covars_multi[[kk]]
    covar_combos <- unlist(lapply(1:length(covars_kk), function(x) combn(covars_kk, x, simplify = FALSE)), recursive = FALSE)
    
    #remove combos that have correlated pairs
    filtered_combos <- Filter(function(combo) !check_corr_pair(combo, corr_pairs), covar_combos)
    
    #turn into formulas
    covar_combo_forms <- lapply(filtered_combos, function(covars_kk) {as.formula(paste0('~1 ~', paste(covars_kk, collapse = " + ")))})
    
    #store
    multivar_combos[[kk]] <- covar_combo_forms
  }

  names(multivar_combos) #just the species to test multivariate models
  lapply(multivar_combos, length) #number of models for each species
  
    #save
    saveRDS(multivar_combos, 'ss_occ/outputs/cand_models_multivar.RDS')
  

## RUN MULTIVARIATE PSI MODELS (ENVIR VARS) ------------------------------------
  
  #run if necessary:
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                     'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  envir_vars <- c("elev_mean_3x3","elev_sd_3x3","pct_slope_mean_3x3","pct_slope_sd_3x3","tree_vol_mean_3x3","tree_vol_sd_3x3",
                  "dist_dambo_mean_3x3","dist_dambo_sd_3x3","dist_cleared_mean_3x3","dist_cleared_sd_3x3","dist_river","prop_M1_3x3",
                  "prop_M2_3x3","prop_M3_3x3")  #don't use ("fence","period","releases") yet!
  multivar_combos <- readRDS('ss_occ/multivar_cand_models.RDS')
  
  #make objects to store results
  psi_multivar_models <- list()
  psi_multivar_aic_tables <- list()
  
  #loop thru species
  for (mm in names(multivar_combos)){
    
    #extract input and model list
    occu_input_mm <- occu_inputs[[mm]]
    model_list_mm <- multivar_combos[[mm]] #names will be generic numbers now

    #loop thru psi covariates and run multivariate models
    multivar_models_mm <- list()
    for (nn in 1:length(model_list_mm)){
      mod_str_nn <- model_list_mm[[nn]]
      cat('Processing:', mm, '     Model:', as.character(c(mod_str_nn)), '\n')
      mod_nn <- occu(formula = mod_str_nn, data = occu_input_mm)
      multivar_models_mm[[as.character(c(mod_nn@formula))]] <- mod_nn
    }
    
    #append null model
    null_mod_mm <- occu(formula = ~1 ~1, data = occu_input_mm)
    multivar_models_mm[[as.character(c(null_mod_mm@formula))]] <- null_mod_mm
    
    #extract model selection results
    multivar_model_list <- fitList(fits = multivar_models_mm)
    multivar_fit_list <- as(modSel(multivar_model_list), 'data.frame')
    
    #save formatted AIC table
    psi_multivar_aic_tables[[mm]] <- multivar_fit_list %>% select(model, nPars, AIC, delta, AICwt, cumltvWt) %>%
                                              mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%  
                                              flextable() %>% 
                                              colformat_num(big.mark = '') %>%
                                              bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>% #shade null
                                              bg(which.max(multivar_fit_list$nPars), bg = "lightblue") %>% #shade global model here too
                                              # autofit() %>%
                                              set_caption(mm)
    
    #store models
    psi_multivar_models[[mm]] <- multivar_models_mm
  }
  #hessian singular for impala
  
  #print tables to Word
  doc3 <- read_docx()
  for (oo in names(psi_multivar_aic_tables)) {
    doc3 <- doc3 %>%
      body_add_flextable(psi_multivar_aic_tables[[oo]]) %>%
      body_add_par(value = '')
  }
  print(doc3, target = paste('ss_occ/outputs', 'aic_tables_psi_multivariate.docx', sep = '/'))
  
  #save model objects
  saveRDS(psi_multivar_models, 'ss_occ/outputs/model_objects_psi_multivariate.RDS')
  
  
## Construct multivar models with structure variables --------------------------

  ## Create lists of top models *so far* for each species
    psi_top_multi <- list()
  
    #for sp we tested multivar model structures:  
      names(psi_multivar_aic_tables)
    
      psi_multivar_aic_tables$buffalo
      psi_top_multi[['buffalo']] <- c('elev_sd_3x3 + pct_slope_sd_3x3')
      
      psi_multivar_aic_tables$impala
      psi_top_multi[['impala']] <- c('pct_slope_mean_3x3',
                                     'dist_cleared_mean_3x3',
                                     'elev_sd_3x3',
                                     'elev_mean_3x3')
      
      psi_multivar_aic_tables$roan
      psi_top_multi[['roan']] <- c('prop_M3_3x3',
                                    'tree_vol_mean_3x3')
      
      psi_multivar_aic_tables$warthog
      psi_top_multi[['warthog']] <- c('elev_sd_3x3')
      
      psi_multivar_aic_tables$zebra
      psi_top_multi[['zebra']] <- c('dist_cleared_mean_3x3 + prop_M1_3x3', 
                                     'dist_cleared_mean_3x3 + tree_vol_mean_3x3')
      
    #for species with univar only:
      names(psi_top_covars[sapply(psi_top_covars, length) == 1])
      
      psi_top_multi[['bushbuck']] <- gsub('~1 ~ ', '', psi_top_covars$bushbuck)
      psi_top_multi[['bushpig']] <- gsub('~1 ~ ', '', psi_top_covars$bushpig)
      psi_top_multi[['eland']] <- gsub('~1 ~ ', '', psi_top_covars$eland)
      psi_top_multi[['elephant']] <- gsub('~1 ~ ', '', psi_top_covars$elephant)
      psi_top_multi[['kudu']] <- gsub('~1 ~ ', '', psi_top_covars$kudu)
      psi_top_multi[['reedbuck']] <- gsub('~1 ~ ', '', psi_top_covars$reedbuck)
      psi_top_multi[['sable']] <- gsub('~1 ~ ', '', psi_top_covars$sable)
      psi_top_multi[['waterbuck']] <- gsub('~1 ~ ', '', psi_top_covars$waterbuck)
      
      #inspect
      lapply(psi_top_multi, length)
      names(psi_top_multi)
    
    
  ## List which species had supplemental releases in 2022
  sp_suppl <-c('buffalo', 'eland', 'impala', 'kudu', 'sable', 'warthog', 'waterbuck', 'zebra')
    
    
  ## List the structural variables
  struct_variables <- c('releases', 'fence', 'period', 'fence*period')
  
  ## Now add the structural combos to the previous top structures for each species 
  
  ## If we only want 'fence*period' and 'releases', not all combinations thereof:
  str_model_list <- list()

    #loop thru species
    for (qq in sort(names(psi_top_multi))) {

      #get top uni/multivar models for this species
      mods_qq <- psi_top_multi[[qq]]
      
      #now construct new model combos
      for (rr in 1:length(mods_qq)){ 
        
        #for each top uni/multivar combo, add this model as-is
        str_model_list[[qq]][[rr]] <- formula(paste0('~1 ~', paste0(mods_qq[rr], collapse = ' + ')))
        
        #add 'fence*period to this model
        str_model_list[[qq]] <- append(str_model_list[[qq]], formula(paste0('~1 ~', paste0(mods_qq[rr], ' + fence*period', collapse = ' + '))))
        
        #add 'releases' to this model (*if* the species had supplemental releases)
        if(qq %in% sp_suppl) {
          str_model_list[[qq]] <- append(str_model_list[[qq]], formula(paste0('~1 ~', paste0(mods_qq[rr], ' + releases', collapse = ' + '))))
          str_model_list[[qq]] <- append(str_model_list[[qq]], formula(paste0('~1 ~', paste0(mods_qq[rr], ' + releases + fence*period', collapse = ' + '))))
          }
      } #end top models loop
    } #end species loop
    
      #inspect and save
      lapply(str_model_list, length)
      saveRDS(str_model_list, 'ss_occ/outputs/cand_models_structure.RDS')
      
      
  ## Or, if we want ALL combinations of 'fence', 'period', 'releases',' fence*period:
    
    #list the vars and the ones that shouldn't be in the same model (fence*period syntax includes both individually)
    #struct_variables <- c('releases', 'fence', 'period', 'fence*period')
    # struct_variables_redundant <- list(c('fence','fence*period'), c('period','fence*period')) 
    # 
    # #generate all combos and remove ones w/ redundant pairs
    # struct_variable_combos <- unlist(lapply(1:length(struct_variables), 
    #                                         function(y) combn(struct_variables, y, simplify = FALSE)), recursive = FALSE) 
    # struct_variable_combos_filtered <- Filter(function(combo) !check_corr_pair(combo, struct_variables_redundant), struct_variable_combos) 
    # 
    # #inspect
    # struct_variable_combos_filtered
    
    # ## Now add the structural combos to the previous top structures for each species
    # str_model_list <- list()
    # 
    # for (qq in sort(names(psi_top_multi))){ #loop thru species
    #   
    #   print(qq)
    #   
    #   #get top uni/multivar models for this species
    #   mods_qq <- psi_top_multi[[qq]]
    #   
    #   #now construct new model combos
    #   str_mods_qq <- list()
    #   for (rr in 1:length(mods_qq)){ 
    #     #for each top uni/multivar combo,
    #     
    #     #add this model as-is,
    #     str_mods_qq <- append(str_mods_qq, 
    #                           formula(paste0('~1 ~', paste0(mods_qq[rr], collapse = ' + '))))
    # 
    #     #then add each combo of structural vars
    #     for (ss in 1:length(struct_variable_combos_filtered)){ 
    #       mod_ss <- (paste0(c(mods_qq[rr], struct_variable_combos_filtered[[ss]]), collapse = " + "))
    #       str_mods_qq <- append(str_mods_qq,
    #                             formula(paste0('~1 ~', mod_ss)))
    #     } #end structural var combos
    #   } #end psi uni/multivar combos
    #   
    #   #store
    #   str_model_list[[qq]] <- str_mods_qq
    #   
    # } #end species loop
    # 
    #   #inspect and save
    #   lapply(str_model_list, length)
    #   saveRDS(str_model_list, 'ss_occ/outputs/single_season/cand_models_structure.RDS')
  
    
## RUN MULTIVARIATE PSI MODELS (ENVIR + STRUCTURAL) -----------------------------
  
  #read in if necessary
  str_model_list <- readRDS('ss_occ/outputs/single_season/cand_models_structure.RDS')
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')

  #make objects to store results
  psi_struct_models <- list()
  psi_struct_aic_tables <- list()
  
  #loop thru species
  for (tt in names(str_model_list)){
    
    #extract input and model list
    occu_input_tt <- occu_inputs[[tt]]
    model_list_tt <- str_model_list[[tt]] #names will be generic numbers now
    
    #loop thru and run models
    struct_models_tt <- list()
    for (uu in 1:length(model_list_tt)){
      mod_str_uu <- model_list_tt[[uu]]
      cat('Processing:', tt, '     Model:', as.character(c(mod_str_uu)), '\n')
      mod_uu <- occu(formula = mod_str_uu, data = occu_input_tt)
      struct_models_tt[[as.character(c(mod_uu@formula))]] <- mod_uu
    }
    
    #append null model
    null_mod_tt <- occu(formula = ~1 ~1, data = occu_input_tt)
    struct_models_tt[[as.character(c(null_mod_tt@formula))]] <- null_mod_tt
    
    #extract model selection results
    struct_model_list <- fitList(fits = struct_models_tt)
    struct_fit_list <- as(modSel(struct_model_list), 'data.frame')
    
    #save formatted AIC table
    psi_struct_aic_tables[[tt]] <- struct_fit_list %>% select(model, nPars, AIC, delta, AICwt, cumltvWt) %>%
                                                           mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%  
                                                           flextable() %>% 
                                                           colformat_num(big.mark = '') %>%
                                                           bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>%
                                                           bg(which.max(struct_fit_list$nPars), bg = "lightblue") %>% #shade global model here too
                                                           # autofit() %>%
                                                           set_caption(tt)
    
    #store models
    psi_struct_models[[tt]] <- struct_models_tt
  }
  ## hessian singular for some models for impala
  psi_struct_models$impala$`~1 ~ dist_cleared_mean_3x3 + releases`
  psi_struct_models$impala$`~1 ~ dist_cleared_mean_3x3 + fence * period`
  psi_struct_models$impala$`~1 ~ dist_cleared_mean_3x3 + releases + fence * period` #this one did converge, though!

  #inspect
  names(psi_struct_models)
  names(psi_struct_aic_tables)
  
  #print tables to Word
  doc4 <- read_docx()
  for (vv in sort(names(psi_struct_aic_tables))) {
    doc4 <- doc4 %>%
      body_add_flextable(psi_struct_aic_tables[[vv]]) %>%
      body_add_par(value = "")
  }
  print(doc4, target = paste('ss_occ/outputs', 'aic_tables_psi_structural.docx', sep = '/'))
  
  #save
  saveRDS(psi_struct_models, 'ss_occ/outputs/model_objects_psi_structure.RDS')
  
  
## Now construct final candidate models ----------------------------------------
  
  ## store top 'psi' models as a list
  psi_top_mods <- list()
    
    #view AIC table for each
    names(psi_struct_aic_tables) 

    psi_struct_aic_tables$buffalo
    psi_top_mods[['buffalo']] <- c(formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + fence * period'),
                                   formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + releases + fence * period'))
    
    psi_struct_aic_tables$bushbuck
    psi_top_mods[['bushbuck']] <- c(formula('~1 ~ tree_vol_mean_3x3 + fence * period'))
    
    psi_struct_aic_tables$bushpig
    psi_top_mods[['bushpig']] <- c(formula('~1 ~ tree_vol_mean_3x3 + fence * period'))
    
    psi_struct_aic_tables$eland
    psi_top_mods[['eland']] <- c(formula('~1 ~ dist_cleared_sd_3x3 + releases'))

    psi_struct_aic_tables$elephant
    psi_top_mods[['elephant']] <- c(formula('~1 ~ prop_M3_3x3 + fence * period'))
    
    psi_struct_aic_tables$impala
    psi_top_mods[['impala']] <- c(formula('~1 ~ dist_cleared_mean_3x3 + releases + fence * period'),
                                  formula('~1 ~ elev_sd_3x3 + releases + fence * period'))
    
    psi_struct_aic_tables$kudu
    psi_top_mods[['kudu']] <- c(formula('~1 ~ elev_mean_3x3 + fence * period'),
                                formula('~1 ~ elev_mean_3x3 + releases + fence * period'))
    
    psi_struct_aic_tables$reedbuck
    psi_top_mods[['reedbuck']] <- c(formula('~1 ~ dist_cleared_mean_3x3 + fence * period'))
    
    psi_struct_aic_tables$roan
    psi_top_mods[['roan']] <- c(formula('~1 ~ tree_vol_mean_3x3 + fence * period'),
                                formula('~1 ~ prop_M3_3x3'))
    
    psi_struct_aic_tables$sable
    psi_top_mods[['sable']] <- c(formula('~1 ~ elev_mean_3x3 + fence * period'),
                                 formula('~1 ~ elev_mean_3x3 + releases + fence * period'))
    
    psi_struct_aic_tables$warthog
    psi_top_mods[['warthog']] <- c(formula('~1 ~ elev_sd_3x3 + fence * period'),
                                   formula('~1 ~ elev_sd_3x3 + releases + fence * period'))
    
    psi_struct_aic_tables$waterbuck
    psi_top_mods[['waterbuck']] <- c(formula('~1 ~ elev_mean_3x3 + releases'), 
                                     formula('~1 ~ elev_mean_3x3'),
                                     formula('~1 ~ elev_mean_3x3 + fence * period'))
    
    psi_struct_aic_tables$zebra
    psi_top_mods[['zebra']] <- c(formula('~1 ~ dist_cleared_mean_3x3 + tree_vol_mean_3x3 + fence * period'),
                                 formula('~1 ~ dist_cleared_mean_3x3 + tree_vol_mean_3x3 + releases + fence * period'))
    
      #inspect and save
      psi_top_mods
      saveRDS(psi_top_mods, 'ss_occ/outputs/top_models_psi.RDS')
  
  
  ## get top 'p' structures
  p_top_mods <- readRDS('ss_occ/outputs/top_models_p.RDS')
  
  
  ## now create combos with all top 'p' and all 'psi'
  final_model_sets <- list()  
  
    #loop for each species
    for (ww in names(p_top_mods)){
      
      #get the top models for 'p' and 'psi'
      p_mods_ww <- p_top_mods[[ww]]
      psi_mods_ww <- psi_top_mods[[ww]]
      
      #make all combos, replacing ~1 on each side
      final_model_sets[[ww]] <- expand_grid(p_mods_ww, psi_mods_ww) %>%
                                        mutate(p = map(p_mods_ww, f_lhs),
                                               psi = map(psi_mods_ww, f_rhs),
                                               combo = map2(p, psi, new_formula)) %>%
                                        pull(combo)
    }
  
    #inspect
    names(final_model_sets)
    lapply(final_model_sets, length)  
    
    #for example:
    final_model_sets$buffalo
    p_top_mods$buffalo; psi_top_mods$buffalo
  
    #save
    saveRDS(final_model_sets, 'ss_occ/outputs/cand_models_final.RDS')
    
    
## RUN FINAL MODELS  -----------------------------------------------------------
  
  #read in if necessary
  final_model_sets <- readRDS('ss_occ/cand_models_final.RDS')
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  
  #make objects to store results
  final_models <- list()
  final_aic_tables <- list()
  top_aic_tables_paper <- NULL #this will be a dataframe
  
  #loop thru species
  for (xx in names(final_model_sets)){
    
    #extract input and model list
    occu_input_xx <- occu_inputs[[xx]]
    model_list_xx <- final_model_sets[[xx]]
    
    #loop thru and run models
    final_models_xx <- list()
    for (yy in 1:length(model_list_xx)){
      mod_str_yy <- model_list_xx[[yy]]
      cat('Processing:', xx, '     Model:', as.character(c(mod_str_yy)), '\n')
      mod_yy <- occu(formula = mod_str_yy, data = occu_input_xx)
      final_models_xx[[as.character(c(mod_yy@formula))]] <- mod_yy
    }
    
    #append null model
    null_mod_xx <- occu(formula = ~1 ~1, data = occu_input_xx)
    final_models_xx[[as.character(c(null_mod_xx@formula))]] <- null_mod_xx
    
    #extract model selection results
    final_model_list <- fitList(fits = final_models_xx)
    final_fit_list <- as(modSel(final_model_list), 'data.frame')
    
    #save formaxxed AIC table
    final_aic_tables[[xx]] <- final_fit_list %>% select(model, nPars, AIC, delta, AICwt, cumltvWt) %>%
                                                 mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%  
                                                 flextable() %>% 
                                                 colformat_num(big.mark = '') %>%
                                                 bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>%
                                                 bg(which.max(final_fit_list$nPars), bg = "lightblue") %>% #shade global model here too
                                                 autofit() %>%
                                                 set_caption(xx)
    
    #save top models (<10 AIC) for each species to report in paper
    top_aic_tables_paper <- rbind(top_aic_tables_paper,
                                  final_fit_list %>% filter(delta < 10) %>%
                                                 select(model, nPars, AIC, delta, AICwt, cumltvWt) %>%
                                                 mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%
                                                 add_row(model = xx, .before = 1))
    
    #store models
    final_models[[xx]] <- final_models_xx
  }
  #no error messages about convergence!
    
  #inspect
  names(final_models)
  names(final_aic_tables)
  
  #print full tables to Word
  doc5 <- read_docx()
  for (zz in sort(names(final_aic_tables))) {
    doc5 <- doc5 %>%
      body_add_flextable(final_aic_tables[[zz]]) %>%
      body_add_par(value = "")
  }
  print(doc5, target = paste('ss_occ/outputs', 'aic_tables_final.docx', sep = '/'))
  
  
  #format and print top models for paper
  top_aic_tables_print <- top_aic_tables_paper %>% mutate(across(c(AIC, delta, AICwt, cumltvWt), ~ round(.x, 2))) %>%  
                                                   flextable() %>% 
                                                   colformat_num(big.mark = '') #%>%
                                                   # autofit() 
  doc6 <- read_docx()
  doc6 <- doc6 %>% body_add_flextable(top_aic_tables_print)
  print(doc6, target = paste('ss_occ/outputs', 'aic_tables_final_for_print.docx', sep = '/'))
  

  #save models
  saveRDS(final_models, 'ss_occ/outputs/model_objects_final.RDS')
  
  
  
########## DON'T NEED BELOW, REALLY? IF I HAVE MARGINAL PLOTS ##################  
  
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
    model_names <- ls(psi_univar_models[[1]][[ff]]) #use 'all' only
    for (model_name in model_names){
      model_object <- psi_univar_models[[1]][[ff]][[model_name]]
      model_table <- param_table(model_object, 0.95, model_name)
      model_table$species <- ff
      param_tables_psi_univariate <- rbind(param_tables_psi_univariate, model_table)
    }
  }
  param_tables_psi_univariate$param <- ifelse(grepl('psi', param_tables_psi_univariate$covar), 'psi','p')
  #save
  write.csv(param_tables_psi_univariate, paste('ss_occ/outputs/single_season', 'psi_univariate_model_parameters.csv', sep = '/'))
  
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
  write.csv(param_tables_psi_multivariate, paste('ss_occ/outputs/single_season', 'psi_multivariate_model_parameters.csv', sep = '/'))
  

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
  param_tables_psi_univariate <- param_tables_psi_univariate %>% 
                                    mutate(covar_name = str_extract(covar, "(?<=\\().+?(?=\\))"))
  sort(unique(param_tables_psi_univariate$mod_name))
  # model_to_plot <- '~effort + season ~ 1'
  
  psi1 <- param_tables_psi_univariate %>% filter(param == 'psi') %>%
    ggplot(aes(x = covar_name, y = beta))

  psi1 + #ggplot(param_tables_psi_univariate, aes(x = covar, y = beta)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbar(aes(ymin = LCI_95, ymax = UCI_95), width = 0.2,
                  position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_wrap(~species, nrow = 3, ncol = 5, scales = 'free_x') +
    # ylim(c(-2.5,2.5)) +
    theme_bw() +
    # labs(title = model_to_plot) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  psi1
  
  #plot (psi - multivariate)
  
  


  
## PREDICTIONS AND MARGINAL PLOTS -----------------------------------------------

  ## Start with NULL models (impala, roan, waterbuck)
  
  #need covarss for plotting and covar means?
  
  ## And univariate psi models (bushbuck, bushpig, kudu, reedbuck, sable, warthog)
  
  
  
  
  
  
  
  