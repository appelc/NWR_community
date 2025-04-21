## Single-season (stacked) occupancy models -- site_seasons as primary periods, weeks as secondary periods

library(data.table)
library(unmarked)
library(tidyverse)
library(knitr)
library(flextable) #to save tables to Word
library(officer) #to save tables to Word


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

ifelse(!dir.exists(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/outputs/single_season', Sys.Date(), sep = '/')), 'Folder exists already')
ifelse(!dir.exists(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 
       dir.create(paste('ss_occ/figures/single_season', Sys.Date(), sep = '/')), 'Folder exists already')


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

  #make sure it's sorted by site
  site_covar_expanded <- site_covar_expanded %>% arrange(site_season)
  rownames(site_covar_expanded) <- site_covar_expanded$site_season
  
  #add 'season' field
  site_covar_expanded$season <- as.factor(sapply(strsplit(site_covar_expanded$site_season, '_'), '[', 3))
    
  #list covar I want to use (3x3 VERSIONS FOR NOW)
    (envir_vars <- names(site_covar_expanded)[!grepl('Site|5x5|Afromontane|Water|Dambo|Rock|Human|site_season', 
                                                           names(site_covar_expanded), ignore.case = FALSE)])

    
## MAKE DETECTION COVARIATE FOR SEASON -----------------------------------------
    
  #create an indicator for dry, wet, cool season so that we can estimate different 'p'
  head(site_season_names)
  
  #extract dry/wet/cool part
  site_season_names$season <- as.factor(sapply(strsplit(site_season_names$site_season, '_'), '[', 3))
    table(site_season_names$season, useNA = 'a')
    
  # #make new field (0=wet, 1=cool, 2=dry)
  # site_season_names$season_indicator <- ifelse(site_season_names$season == 'wet', 0, 
  #                                          ifelse(site_season_names$season == 'cool', 1, 2))
  #   table(site_season_names$season_indicator, site_season_names$season, useNA = 'a')
    
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
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala','kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  
  
## CREATE MODEL INPUTS ---------------------------------------------------------
  
  #set cutoff for before/after fence decommissioning
  #cutoff <- 'wet_2020-18' #anything with 2018,2019,2020 is before; 2021;2022;2023 is after
  
  #create object to store
  occu_inputs <- list()
  naive_occ <- NULL
  
  #loop thru species
  for (ss in names(dh_stacked)){
    
    #extract detection history
    dh_ss <- data.frame(dh_stacked[[ss]])
    rownames(dh_ss) <- dh_ss$site_primary
    
    #convert to 0/1
    # dh_ss <- dh_ss %>% mutate(across(.cols = starts_with('X'), 
    #                                  .fns = ~ ifelse(is.na(.), NA, ifelse(. > 0, 1, 0))))
    
    #find row indices for before/after fence decommissioning
    dh_ss <- dh_ss %>% mutate(period = case_when(grepl('2018|2019|2020', site_primary) ~ 'before',
                                                 grepl('2021|2022|2023', site_primary) ~ 'after'))
    before_rows <- which(dh_ss$period == 'before')
    after_rows <- which(dh_ss$period == 'after')
    
    #also add field for before/after to the site covariates
    site_covar_expanded <- site_covar_expanded %>% mutate(period = case_when(grepl('2018|2019|2020', site_season) ~ 'before',
                                                                             grepl('2021|2022|2023', site_season) ~ 'after'))
    
    #replace 'fence' with correct 'fence' field from site_covar
    dh_ss$fence <- site_covar_expanded$fence[match(dh_ss$site_primary, site_covar_expanded$site_season)]
    dh_ss$fence <- ifelse(dh_ss$fence == 0, 'North', 'South')

    #calculate naive occupancy
    naive_occ_ss <- dh_ss %>%
      mutate(across(starts_with('X'), ~ ifelse(is.na(.), 0, .))) %>%  #convert NA with 0s
      summarize(total_det = sum(across(starts_with('X')), na.rm = TRUE), 
                naive_occ = mean(rowSums(across(starts_with('X')) > 0, na.rm = TRUE) > 0)) %>%
      bind_rows(
        dh_ss %>% group_by(period, fence) %>% 
          summarize(total_det = sum(across(starts_with('X')), na.rm = TRUE),
                    naive_occ = mean(rowSums(across(starts_with('X')) > 0, na.rm = TRUE) > 0), .groups = 'drop')) %>%
          mutate(species = ss, period = replace_na(period, 'all'), fence = replace_na(fence, 'both'))
    naive_occ <- rbind(naive_occ, naive_occ_ss)
    
    #keep only week columns
    dh_ss <- dh_ss %>% dplyr::select(starts_with('X')) 
    
    #create input (all)
    occu_input_ss <- unmarkedFrameOccu(y = as.matrix(dh_ss),
                                       obsCovs = list(effort = effort_covar, season = season_covar),
                                       siteCovs = site_covar_expanded)
    #create input (before)
    occu_input_before_ss <- unmarkedFrameOccu(y = as.matrix(dh_ss[before_rows,]),
                                       obsCovs = list(effort = effort_covar[before_rows,], 
                                                      season = season_covar[before_rows,]),
                                       siteCovs = site_covar_expanded[before_rows,-'period'])
    #create input (after)
    occu_input_after_ss <- unmarkedFrameOccu(y = as.matrix(dh_ss[after_rows,]),
                                              obsCovs = list(effort = effort_covar[after_rows,], 
                                                             season = season_covar[after_rows,]),
                                              siteCovs = site_covar_expanded[after_rows,-'period'])
    
    #store
    occu_inputs[['all']][[ss]] <- occu_input_ss
    occu_inputs[['before']][[ss]] <- occu_input_before_ss
    occu_inputs[['after']][[ss]] <- occu_input_after_ss
  }
  #warnings are about converting characters to factors
  
  #inspect:
  names(occu_inputs)
  names(occu_inputs$all)
  names(occu_inputs$before)
  names(occu_inputs$after)
  head(occu_inputs$all$elephant)
  
  head(naive_occ)
  sort(unique(naive_occ$species))
  
  #export naive_occ table for paper (TABLE 2)
  naive_occ_table <- naive_occ %>% filter(period != 'all') %>%
            pivot_wider(names_from = c(period, fence), values_from = c(naive_occ, total_det), names_sort = TRUE) %>%
            select(1, rev(colnames(.)[-1])) %>%
            mutate(across(c(naive_occ_before_South, naive_occ_before_North, naive_occ_after_South, naive_occ_after_North), ~ round(.x, 2))) %>%
            filter(species %in% focal_species) %>%
            flextable() %>% 
            colformat_num(big.mark = '') #%>%
            # autofit() 
  
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
  # occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala','kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  
  #make objects to store results
  p_models <- list()
  p_aic_tables <- list()
  
  #loop thru all/before/after
  for (aa in names(occu_inputs)){
    inputs_aa <- occu_inputs[[aa]]
    
    #loop thru species
    for (pp in focal_species){ #or (pp in names(inputs_aa))
      cat('Processing:', aa, '     For:', pp, '\n') #to capture species associated with any printouts
      
      #extract input
      occu_input_pp <- inputs_aa[[pp]]
      
      #run detection models:
      p_null <- occu(formula = ~1 ~1, data = occu_input_pp)
      p_effort <- occu(formula = ~effort ~1, data = occu_input_pp)
      p_season <- occu(formula = ~season ~1, data = occu_input_pp)
      p_effort_season <- occu(formula = ~effort + season ~1, data = occu_input_pp)
      
      #store
      p_models[[aa]][[pp]] <- list(p_null, p_effort, p_season, p_effort_season)
      names(p_models[[aa]][[pp]]) <- c(as.character(c(p_null@formula)), as.character(c(p_effort@formula)), 
                                       as.character(c(p_season@formula)), as.character(c(p_effort_season@formula)))
      
      #extract model selection results
      p_model_list <- fitList(fits = p_models[[aa]][[pp]])
      p_mod_sel <- as(modSel(p_model_list, nullmod = '~1 ~ 1'), 'data.frame')
      
      #save formatted AIC table
      p_aic_tables[[aa]][[pp]] <- p_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
        mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
        flextable() %>% 
        colformat_num(big.mark = '') %>%
        bg(i = ~ model == as.character(c(p_null@formula)), bg = "lightgray") %>%
        autofit() %>%
        set_caption(pp)
    }
  }
  #non-convergence for: bushpig (before), zebra (after) ...hmm

  #print AIC tables to Word
  doc1 <- read_docx()
  for (cc in names(p_aic_tables)){
    doc1 <- doc1 %>% body_add_par(value = cc, style = 'heading 1')
    for(bb in focal_species){
      doc1 <- doc1 %>% 
        body_add_flextable(p_aic_tables[[cc]][[bb]]) %>%
        body_add_par(value = '')
    }
  }
  
  print(doc1, target = paste('ss_occ/outputs/single_season', 'aic_tables_p.docx', sep = '/'))


  
## RUN UNIVARIATE PSI MODELS (ENVIRONMENTAL VARS) ------------------------------
  
  # Run if necessary:
  occu_inputs <- readRDS('ss_occ/occu_inputs.RDS')
  focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala','kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')
  envir_vars <- c("elev_mean_3x3","elev_sd_3x3","pct_slope_mean_3x3","pct_slope_sd_3x3","tree_vol_mean_3x3","tree_vol_sd_3x3",
                        "dist_dambo_mean_3x3","dist_dambo_sd_3x3","dist_human_mean_3x3","dist_human_sd_3x3","dist_river","prop_M1_3x3",
                        "prop_M2_3x3","prop_M3_3x3")       
                                #don't use ("fence","period") yet!
  
  #create list of univariate models
  # (univar_psi <- lapply(c(1, envir_vars), function(x) as.formula(paste0('~effort + season ~', x)))) #include 1 for dot model too
  (univar_psi <- lapply(c(1, envir_vars), function(x) as.formula(paste0('~1 ~', x)))) #include 1 for dot model too
    names(univar_psi) <- c('null', envir_vars)

  #make objects to store results
  psi_univar_models <- list()
  psi_univar_aic_tables <- list()
  
  #loop thru all/before/after
  for (dd in names(occu_inputs)){
    inputs_dd <- occu_inputs[[dd]]
    
    #loop thru species
    for (mm in focal_species){
      cat('Processing:', dd, '     For:', mm, '\n') #to capture species associated with any printouts
      
      #extract input
      occu_input_mm <- inputs_dd[[mm]]
      
      #remove the 'period' model if we're doing separate before/after models
      univar_psi_dd <- if(dd == 'all') {univar_psi} else {within(univar_psi, rm(period))}
      
      #loop thru psi covariates and run univariate models
      univar_models_mm <- list()
      for (uu in names(univar_psi_dd)){
        mod_str_uu <- univar_psi_dd[[uu]]
        mod_uu <- occu(formula = mod_str_uu, data = occu_input_mm)
        univar_models_mm[[as.character(c(mod_uu@formula))]] <- mod_uu
      }
      
      #extract model selection results
      univar_model_list <- fitList(fits = univar_models_mm)
      # univar_mod_sel <- as(modSel(univar_model_list, nullmod = '~effort + season ~ 1'), 'data.frame')
      univar_mod_sel <- as(modSel(univar_model_list, nullmod = '~1 ~ 1'), 'data.frame')
      
      #save formatted AIC table
      psi_univar_aic_tables[[dd]][[mm]] <- univar_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
                                                              mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
                                                              flextable() %>% 
                                                              colformat_num(big.mark = '') %>%
                                                              bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>%
                                                              autofit() %>%
                                                              set_caption(paste0(mm, ' (', dd, ')'))
      
      #store models
      psi_univar_models[[dd]][[mm]] <- univar_models_mm
    }
  }  
    
  #hessian is singular for: roan (all), bushpig (before), zebra (before), roan (after), zebra (after)
  
  names(psi_univar_models)
  names(psi_univar_models$all)
  names(psi_univar_aic_tables)
  names(psi_univar_aic_tables$all)
  
  #print AIC tables to Word
  doc2 <- read_docx()
  # for (ee in names(psi_univar_aic_tables)){
    # doc2 <- doc2 %>% body_add_par(value = ee, style = 'heading 1') #uncomment these to do all, before, after
    for(ff in focal_species){
      doc2 <- doc2 %>% 
        # body_add_flextable(psi_univar_aic_tables[[ee]][[ff]]) %>% #just doing 'all' for now
        body_add_flextable(psi_univar_aic_tables[['all']][[ff]]) %>%
        body_add_par(value = '')
    }
  # }
  print(doc2, target = paste('ss_occ/outputs/single_season', 'aic_tables_psi_univariate.docx', sep = '/'))
  
  
## CONSTRUCT MULTIVARIATE PSI MODELS -------------------------------------------
  
  #Construct model sets for species with multiple variables within 10 AIC
  # (except impala and roan, for which ALL univariate models were within 10 AIC, including null)
  
  #view AIC tables for each species and list cov within 10 AIC of the top:
  psi_univar_aic_tables$all$buffalo
  buffalo_vars <- c('elev_sd_3x3','pct_slope_sd_3x3','pct_slope_mean_3x3')
    # multivar_list[['buffalo']] <- c('dist_human_sd_3x3','tree_vol_mean_3x3','prop_M3_3x3','pct_slope_sd_3x3','fence')     #removed prop_M1
  
  psi_univar_aic_tables$all$bushbuck
  bushbuck_vars <- c('tree_vol_mean_3x3')
    # multivar_list[['bushbuck']] <- c('tree_vol_mean_3x3')
    ### only 'tree_vol_mean_3x3'
    
  psi_univar_aic_tables$all$bushpig
  bushpig_vars <- c('tree_vol_mean_3x3')
    # multivar_list[['bushpig']] <- c('tree_vol_mean_3x3','fence')

  psi_univar_aic_tables$all$eland
    # multivar_list[['eland']] <- c('dist_human_sd_3x3','prop_M2_3x3','dist_dambo_sd_3x3')
    #NOW THEY"RE ALL WITHIN 10 AIC
  
  psi_univar_aic_tables$all$elephant
  elephant_vars <- c('prop_M3_3x3')
    # multivar_list[['elephant']] <- c('tree_vol_mean_3x3','pct_slope_mean_3x3')    #removed pct_slope_sd
    ### only 'fence' now
    
  psi_univar_aic_tables$all$impala
  impala_vars <- c('pct_slope_mean_3x3','dist_human_mean_3x3','elev_sd_3x3','elev_mean_3x3','prop_M3_3x3','tree_vol_mean_3x3','dist_human_sd_3x3','dist_dambo_sd_3x3') #also NULL. 
    # multivar_list[['impala']] <- c('fence','pct_slope_mean_3x3','dist_human_mean_3x3','elev_mean_3x3','prop_M3_3x3')
      #removed 'elev_sd_3x3' and 'tree_vol_sd_3x3'
    
  psi_univar_aic_tables$all$kudu
  kudu_vars <- 'elev_mean_3x3'
    # multivar_list[['kudu']] <- c('fence')
    ### only 'fence' now
    
  psi_univar_aic_tables$all$reedbuck
  reedbuck_vars <- c('dist_human_mean_3x3','dist_dambo_mean_3x3')
  # multivar_list[['redbuck']] <- c('fence')
  ### only 'fence' now
  
  psi_univar_aic_tables$all$roan
  roan_vars <- c('prop_M3_3x3','tree_vol_mean_3x3')
    # multivar_list[['roan']] <- c('prop_M3_3x3','fence')
    #removed 'prop_M1_3x3' and 'tree_vol_mean_3x3'
  
  psi_univar_aic_tables$all$sable
  sable_vars <- c('elev_mean_3x3')
    # multivar_list[['sable']] <- c('fence')
    ### only 'fence' now
  
  psi_univar_aic_tables$all$warthog
  warthog_vars <- c('elev_sd_3x3','pct_slope_mean_3x3') 
    # multivar_list[['warthog']] <- c('fence')
    ### only 'fence' now
    
  psi_univar_aic_tables$all$waterbuck
  waterbuck_vars <- c('elev_mean_3x3')
    # multivar_list[['waterbuck']] <- c('pct_slope_mean_3x3','elev_sd_3x3','dist_human_mean_3x3','dist_dambo_mean_3x3',
    #                    'prop_M3_3x3','fence','dist_river','tree_vol_mean_3x3') #removed prop_M1, dist_dambo_sd_3x3, dist_human_sd_3x3,tree_vol_sd_3x3#also include null!
    # multivar_list[['waterbuck']] <- c('elev_mean_3x3')
    ### only 'elev_mean_3x3' now
    
  psi_univar_aic_tables$all$zebra
  zebra_vars <- c('dist_human_mean_3x3','tree_vol_mean_3x3','prop_M1_3x3')
    # multivar_list[['zebra']] <- c('dist_dambo_mean_3x3','prop_M1_3x3','dist_river','prop_M2_3x3','fence')
    # multivar_list[['zebra']] <- c('fence')
    ### only 'fence' now
    
  
  #combine covariate lists [WHAT ABOUT ELAND?]
  covar_lists <- list('buffalo' = buffalo_vars, 'bushbuck' = bushbuck_vars, 'bushpig' = bushpig_vars, 'elephant' = elephant_vars, 
                      'impala' = impala_vars, 'kudu' = kudu_vars, 'reedbuck' = reedbuck_vars, 'roan' = roan_vars, 
                      'sable' = sable_vars, 'warthog' = warthog_vars, 'waterbuck' = waterbuck_vars, 'zebra' = zebra_vars)
  
  #take only those with >1 variable for this next step:
  covar_lists_multi <- covar_lists[sapply(covar_lists, length) > 1]
  
  #store list of variable pairs that can't be uesd in the same models
  corr_pairs <- list(#first, correlated pairs:
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
  for (gg in names(covar_lists_multi)){
    
    #find all combos
    covars_gg <- covar_lists_multi[[gg]]
    covar_combos <- unlist(lapply(1:length(covars_gg), function(x) combn(covars_gg, x, simplify = FALSE)), recursive = FALSE)
    
    #remove combos that have correlated pairs
    filtered_combos <- Filter(function(combo) !check_corr_pair(combo, corr_pairs), covar_combos)
    
    #turn into formulas
    covar_combo_forms <- lapply(filtered_combos, function(covars_gg) {as.formula(paste0('~1 ~', paste(covars_gg, collapse = " + ")))})
    
    #store
    multivar_combos[[gg]] <- covar_combo_forms
  }

  names(multivar_combos)
  lapply(multivar_combos, length) #number of models for each species
  

## RUN MULTIVARIATE PSI MODELS -------------------------------------------------
  
  #make objects to store results
  psi_multivar_models <- list()
  psi_multivar_aic_tables <- list()
  
  #loop thru species
  for (yy in names(multivar_combos)){
    
    print(yy) #print species name to capture any associated printouts
    
    #extract input and model list
    occu_input_yy <- occu_inputs[[1]][[yy]] #use 'any' only
    model_list_yy <- multivar_combos[[yy]] #names will be generic numbers now

    #loop thru psi covariates and run multivariate models
    multivar_models_yy <- list()
    for (nn in 1:length(model_list_yy)){
      mod_str_nn <- model_list_yy[[nn]]
      mod_nn <- occu(formula = mod_str_nn, data = occu_input_yy)
      multivar_models_yy[[as.character(c(mod_nn@formula))]] <- mod_nn
    }
    
    #append null model
    null_mod_yy <- occu(formula = ~1 ~1, data = occu_input_yy)
    multivar_models_yy[[as.character(c(null_mod_yy@formula))]] <- null_mod_yy
    
    #extract model selection results
    multivar_model_list <- fitList(fits = multivar_models_yy)
    multivar_mod_sel <- as(modSel(multivar_model_list), 'data.frame')
    
    #save formatted AIC table
    psi_multivar_aic_tables[[yy]] <- multivar_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
                                              mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
                                              flextable() %>% 
                                              colformat_num(big.mark = '') %>%
                                              bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>% #shade null
                                              bg(which.max(multivar_mod_sel$nPars), bg = "lightblue") %>% #shade global model here too
                                              autofit() %>%
                                              set_caption(yy)
    
    #store models
    psi_multivar_models[[yy]] <- multivar_models_yy
  }
  #hessian singular for impala
  
  #print tables to Word
  doc3 <- read_docx()
  for (zz in names(psi_multivar_aic_tables)) {
    doc3 <- doc3 %>%
      body_add_flextable(psi_multivar_aic_tables[[zz]]) %>%
      body_add_par(value = '')
  }
  print(doc3, target = paste('ss_occ/outputs/single_season', 'aic_tables_psi_multivariate.docx', sep = '/'))
  
  
## MAKE NEW MODEL LIST ---------------------------------------------------------

  ## for species with multivar models
    psi_multivar_aic_tables$buffalo
    buffalo_mod <- c(formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3'),
                     formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + fence'),
                     formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + period'),
                     formula('~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + fence*period'))
  
    psi_multivar_aic_tables$impala #dunno!!
    impala_mod <- c(formula('~1 ~1'), #??
                    formula('~1 ~fence'),
                    formula('~1 ~period'),
                    formula('~1 ~fence*period'))
    
    psi_multivar_aic_tables$reedbuck
    reedbuck_mod <- c(formula('~1 ~ dist_human_mean_3x3 + dist_dambo_mean_3x3'),
                      formula('~1 ~ dist_human_mean_3x3 + dist_dambo_mean_3x3 + fence'),
                      formula('~1 ~ dist_human_mean_3x3 + dist_dambo_mean_3x3 + period'),
                      formula('~1 ~ dist_human_mean_3x3 + dist_dambo_mean_3x3 + fence*period'))
    
    psi_multivar_aic_tables$roan
    roan_mod <- c(formula('~1 ~ prop_M3_3x3'), 
                  formula('~1 ~ tree_vol_mean_3x3'),
                  formula('~1 ~ prop_M3_3x3 + fence'),
                  formula('~1 ~ prop_M3_3x3 + period'),
                  formula('~1 ~ prop_M3_3x3 + fence*period'),
                  formula('~1 ~ tree_vol_mean_3x3 + fence'),
                  formula('~1 ~ tree_vol_mean_3x3 + period'),
                  formula('~1 ~ tree_vol_mean_3x3 + fence*period'))
    
    psi_multivar_aic_tables$warthog
    warthog_mod <- c(formula('~1 ~ elev_sd_3x3'),
                     formula('~1 ~ elev_sd_3x3 + fence'),
                     formula('~1 ~ elev_sd_3x3 + period'),
                     formula('~1 ~ elev_sd_3x3 + fence*period'))
    
    psi_multivar_aic_tables$zebra
    zebra_mod <- c(formula('~1 ~ dist_human_mean_3x3 + prop_M1_3x3'), 
                   formula('~1 ~ dist_human_mean_3x3 + tree_vol_mean_3x3'),
                   formula('~1 ~ dist_human_mean_3x3 + prop_M1_3x3 + fence'), 
                   formula('~1 ~ dist_human_mean_3x3 + prop_M1_3x3 + period'),
                   formula('~1 ~ dist_human_mean_3x3 + prop_M1_3x3 + fence*period'),
                   formula('~1 ~ dist_human_mean_3x3 + tree_vol_mean_3x3 + fence'),
                   formula('~1 ~ dist_human_mean_3x3 + tree_vol_mean_3x3 + period'),
                   formula('~1 ~ dist_human_mean_3x3 + tree_vol_mean_3x3 + fence*period'))
  
  ## for species with univar models (shd automate this)
  covar_lists[sapply(covar_lists, length) == 1]
  
  bushbuck_mod <- c(formula('~1 ~ tree_vol_mean_3x3'),
                    formula('~1 ~ tree_vol_mean_3x3 + fence'),
                    formula('~1 ~ tree_vol_mean_3x3 + period'),
                    formula('~1 ~ tree_vol_mean_3x3 + fence*period'))
  
  bushpig_mod <- c(formula('~1 ~ tree_vol_mean_3x3'),
                   formula('~1 ~ tree_vol_mean_3x3 + fence'),
                   formula('~1 ~ tree_vol_mean_3x3 + period'),
                   formula('~1 ~ tree_vol_mean_3x3 + fence*period'))
  
  elephant_mod <- c(formula('~1 ~ prop_M3_3x3'),
                    formula('~1 ~ prop_M3_3x3 + fence'),
                    formula('~1 ~ prop_M3_3x3 + period'),
                    formula('~1 ~ prop_M3_3x3 + fence*period'))
  
  kudu_mod <- c(formula('~1 ~ elev_mean_3x3'),
                formula('~1 ~ elev_mean_3x3 + fence'),
                formula('~1 ~ elev_mean_3x3 + period'),
                formula('~1 ~ elev_mean_3x3 + fence*period'))
  
  sable_mod <- c(formula('~1 ~ elev_mean_3x3'),
                 formula('~1 ~ elev_mean_3x3 + fence'),
                 formula('~1 ~ elev_mean_3x3 + period'),
                 formula('~1 ~ elev_mean_3x3 + fence*period'))
  
  waterbuck_mod <- c(formula('~1 ~ elev_mean_3x3'),
                     formula('~1 ~ elev_mean_3x3 + fence'),
                     formula('~1 ~ elev_mean_3x3 + period'),
                     formula('~1 ~ elev_mean_3x3 + fence*period'))
  
  ## for eland?
  
  eland_mod <- c(formula('~1 ~1'), 
                 formula('~1 ~fence'),
                 formula('~1 ~period'),
                 formula('~1 ~fence*period'))
  
    
  ## COMBINE
  model_lists <- list('buffalo' = buffalo_mod, 'bushbuck' = bushbuck_mod, 'bushpig' = bushpig_mod, 'elephant' = elephant_mod, eland = eland_mod,
                       'impala' = impala_mod, 'kudu' = kudu_mod, 'reedbuck' = reedbuck_mod, 'roan' = roan_mod, 
                       'sable' = sable_mod, 'warthog' = warthog_mod, 'waterbuck' = waterbuck_mod, 'zebra' = zebra_mod)
  
  
## ADD STRUCTURAL VARS + INTERACTION -------------------------------------------
  
  #make objects to store results
  psi_interact_models <- list()
  psi_interact_aic_tables <- list()
  
  #loop thru species
  for (vv in names(model_lists)){
    
    print(vv) #print species name to capture any associated printouts
    
    #extract input and model list
    occu_input_vv <- occu_inputs[[1]][[vv]] #use 'any' only
    model_list_vv <- model_lists[[vv]] #names will be generic numbers now
    
    #loop thru psi covariates and run multivariate models
    interact_models_vv <- list()
    for (ww in 1:length(model_list_vv)){
      mod_str_ww <- model_list_vv[[ww]]
      mod_ww <- occu(formula = mod_str_ww, data = occu_input_vv)
      interact_models_vv[[as.character(c(mod_ww@formula))]] <- mod_ww
    }
    
    #append null model
    null_mod_vv <- occu(formula = ~1 ~1, data = occu_input_vv)
    interact_models_vv[[as.character(c(null_mod_vv@formula))]] <- null_mod_vv
    
    #extract model selection results
    interact_model_list <- fitList(fits = interact_models_vv)
    interact_mod_sel <- as(modSel(interact_model_list), 'data.frame')
    
    #save formatted AIC table
    psi_interact_aic_tables[[vv]] <- interact_mod_sel %>% select(model, nPars, AIC, delta, AICwt, cumltvWt, Rsq) %>%
      mutate(across(c(AIC, delta, AICwt, cumltvWt, Rsq), ~ round(.x, 2))) %>%  
      flextable() %>% 
      colformat_num(big.mark = '') %>%
      bg(i = ~ model == as.character('~1 ~ 1'), bg = "lightgray") %>%
      bg(which.max(interact_mod_sel$nPars), bg = "lightblue") %>% #shade global model here too
      autofit() %>%
      set_caption(vv)
    
    #store models
    psi_interact_models[[vv]] <- interact_models_vv
  }
  #hessian singular for impala
  
  #inspect
  names(psi_interact_models)
  names(psi_interact_aic_tables)
  
  #did interaction syntax work? I think so!
  ## **WHAT IF I CHANGE FENCE TO NORTH/SOUTH INSTEAD OF 0/1 FOR EASE OF INTERPETATION LIKE THIS
  psi_interact_models$buffalo$`~1 ~ elev_sd_3x3 + pct_slope_sd_3x3 + fence * period`
  
  #print tables to Word
  doc4 <- read_docx()
  for (name in names(psi_interact_aic_tables)) {
    doc4 <- doc4 %>%
      body_add_flextable(psi_interact_aic_tables[[name]]) %>%
      body_add_par(value = "")
  }
  print(doc4, target = paste('ss_occ/outputs/single_season', 'aic_tables_psi_interaction.docx', sep = '/'))
  
  
  
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
  
  
  
  
  
  
  
  