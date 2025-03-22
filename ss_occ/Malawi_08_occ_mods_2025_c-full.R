######
## STEP 8 - Run occupancy models for 2025 chapter (2018-23 detections)
##          (Uses detection histories previously created using 'Malawi_07_det_hist_species.R')
##
## C: - run full model sets
##    - construct and inspect AIC tables
######

library(data.table)
library(tidyverse)
library(RPresence)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)

    
## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

  ifelse(!dir.exists(paste('Malawi/outputs/models', Sys.Date(), sep = '/')), 
         dir.create(paste('Malawi/outputs/models', Sys.Date(), sep = '/')), 'Folder exists already')
  print(paste('created folder', paste('Malawi/outputs/models', Sys.Date(), sep = '/')))
  
  ifelse(!dir.exists(paste('Malawi/outputs/figures', Sys.Date(), sep = '/')), 
         dir.create(paste('Malawi/outputs/figures', Sys.Date(), sep = '/')), 'Folder exists already')
  print(paste('created folder', paste('Malawi/outputs/figures', Sys.Date(), sep = '/')))
  
  
## CREATE MODEL INPUTS ---------------------------------------------------------

## Read in if necessary
  dh_combined <- fread('Malawi/outputs/models/combined_dethist_082824.csv')
  hexData <- fread('Malawi/outputs/models/hex_covariates.csv')
    
## Create site covariate dataframe
  siteCovar <- data.frame(hexData[hexData$hex %in% unique(dh_combined$hex), #keep only the 49 hexagons in det histories
                                  c('M1_prop_z','M2_prop_z','M3_prop_z','TREEVOL_z','SLOPE_MEAN_z','SLOPE_SD_z',
                                    'DAMBO_DIST_z','FIRE_YRS_z','ELEV_MEAN_z','ELEV_SD_z','COST_ELEV_z','BuaRiver_indicator')]) #use these columns
  
  
## VIEW AIC TABLES -------------------------------------------------------------
  aic_tables <- readRDS('Malawi/outputs/models/2024-08-29/aic_tables_univariate.RDS')
    names(aic_tables)

    #focal species
    aic_tables$elephant #M1, cost_elev, M3, elev_mean, treevol, slope_mean, dambo, slope_sd, BuaRiver, elev_sd, M2, fire
    aic_tables$buffalo #elev_mean, M1, slope_sd, slope_mean, treevol, M3, M2
    aic_tables$kudu #elev_mean, elev_sd, slope_sd, slope_mean, M1, cost_elev
    aic_tables$sable #slope_sd, slope_mean, elev_mean, elev_sd, fire, dambo, M1, M2, BuaRiver, cost_elev, M3, treevol
    aic_tables$warthog #elev_mean, M1, dambo, slope_mean, slope_sd, M3, cost_elev, fire, treevol
    aic_tables$waterbuck #slope_mean, elev_mean, slope_sd, cost_elev, elev_sd, BuaRiver, M1, dambo, M3, treevol, fire, M2
    aic_tables$eland #elev_mean, dambo_dist, M3, M1, treevol, fire, BuaRiver, slope_sd, slope_mean, elev_sd, M2, cost_elev
    aic_tables$zebra #elev_mean, slope_sd, slope_mean, M1, elev_sd, dambo, BuaRiver, treevol, M2, cost_elev, M3, fire
    
    #some interesting ones:
    aic_tables$aardvark #elevation
    aic_tables$baboon #M2, M3, treevol
    aic_tables$bushpig #treevol
    aic_tables$honey_badger #treevol
    aic_tables$impala #M2
    aic_tables$leopard #elev_mean
    
    
## CREATE LIST OF MODELS -------------------------------------------------------
  
  #Based on univariate models within <10 AIC of top model, and with non-correlated variables (using rho < 0.6)
  mod_lists <- list('elephant' = list(as.formula('psi ~ BuaRiver_indicator'),
                                      as.formula('psi ~ M1_prop_z'),
                                      as.formula('psi ~ SLOPE_SD_z'),
                                      as.formula('psi ~ TREEVOL_z'),
                                      as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z'),
                                      as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z + TREEVOL_z'),
                                      as.formula('psi ~ BuaRiver_indicator + TREEVOL_z'),
                                      as.formula('psi ~ M1_prop_z + SLOPE_SD_z'),
                                      as.formula('psi ~ SLOPE_SD_z + TREEVOL_z')),
                    'Cape buffalo' = list(as.formula('psi ~ ELEV_MEAN_z'),
                                          as.formula('psi ~ M1_prop_z'),
                                          as.formula('psi ~ ELEV_MEAN_z + M1_prop_z')),
                    'Eland' = list(as.formula('psi ~ DAMBO_DIST_z'),
                                   as.formula('psi ~ SLOPE_SD_z'),
                                   as.formula('psi ~ COST_ELEV_z'),
                                   as.formula('psi ~ SLOPE_MEAN_z'),
                                   as.formula('psi ~ ELEV_SD_z'),
                                   as.formula('psi ~ M1_prop_z'),
                                   as.formula('psi ~ 1'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_SD_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_MEAN_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + ELEV_SD_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + M1_prop_z'),
                                   as.formula('psi ~ SLOPE_SD_z + COST_ELEV_z'),
                                   as.formula('psi ~ SLOPE_SD_z + M1_prop_z'),
                                   as.formula('psi ~ COST_ELEV_z + SLOPE_MEAN_z'),
                                   as.formula('psi ~ COST_ELEV_z + ELEV_SD_z'),
                                   as.formula('psi ~ COST_ELEV_z + M1_prop_z'),
                                   as.formula('psi ~ SLOPE_MEAN_z + M1_prop_z'),
                                   as.formula('psi ~ ELEV_SD_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_SD_z + COST_ELEV_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_SD_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z + SLOPE_MEAN_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z + ELEV_SD_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_MEAN_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + ELEV_SD_z + M1_prop_z'),
                                   as.formula('psi ~ SLOPE_SD_z + COST_ELEV_z + M1_prop_z'),
                                   as.formula('psi ~ COST_ELEV_z + SLOPE_MEAN_z + M1_prop_z'),
                                   as.formula('psi ~ COST_ELEV_z + ELEV_SD_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + SLOPE_SD_z + COST_ELEV_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z + SLOPE_MEAN_z + M1_prop_z'),
                                   as.formula('psi ~ DAMBO_DIST_z + COST_ELEV_z + ELEV_SD_z + M1_prop_z')),
                    'Greater kudu' = list(as.formula('psi ~ SLOPE_SD_z'),
                                          as.formula('psi ~ SLOPE_MEAN_z'),
                                          as.formula('psi ~ TREEVOL_z'),
                                          as.formula('psi ~ M1_prop_z'),
                                          as.formula('psi ~ SLOPE_SD_z + TREEVOL_z'),
                                          as.formula('psi ~ SLOPE_SD_z + M1_prop_z'),
                                          as.formula('psi ~ SLOPE_MEAN_z + TREEVOL_z'),
                                          as.formula('psi ~ SLOPE_MEAN_z + M1_prop_z')),
                    'Sable antelope' = list(as.formula('psi ~ BuaRiver_indicator'),
                                            as.formula('psi ~ SLOPE_SD_z'),
                                            as.formula('psi ~ SLOPE_MEAN_z'),
                                            as.formula('psi ~ TREEVOL_z'),
                                            as.formula('psi ~ M1_prop_z'),
                                            as.formula('psi ~ M3_prop_z'),
                                            as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z'),
                                            as.formula('psi ~ BuaRiver_indicator + SLOPE_MEAN_z'),
                                            as.formula('psi ~ BuaRiver_indicator + TREEVOL_z'),
                                            as.formula('psi ~ SLOPE_SD_z + TREEVOL_z'),
                                            as.formula('psi ~ SLOPE_SD_z + M1_prop_z'),
                                            as.formula('psi ~ SLOPE_SD_z + M3_prop_z'),
                                            as.formula('psi ~ SLOPE_MEAN_z + TREEVOL_z'),
                                            as.formula('psi ~ SLOPE_MEAN_z + M1_prop_z'),
                                            as.formula('psi ~ SLOPE_MEAN_z + M3_prop_z'),
                                            as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z + TREEVOL_z'),
                                            as.formula('psi ~ BuaRiver_indicator + SLOPE_MEAN_z + TREEVOL_z')),
                    'Warthog' = list(as.formula('psi ~ BuaRiver_indicator'),
                                     as.formula('psi ~ M1_prop_z')),
                    'Waterbuck' = list(as.formula('psi ~ ELEV_MEAN_z'),
                                       as.formula('psi ~ SLOPE_MEAN_z'),
                                       as.formula('psi ~ TREEVOL_z'),
                                       as.formula('psi ~ ELEV_SD_z'),
                                       as.formula('psi ~ SLOPE_SD_z'),
                                       as.formula('psi ~ M1_prop_z'),
                                       as.formula('psi ~ M3_prop_z'),
                                       as.formula('psi ~ BuaRiver_indicator'),
                                       as.formula('psi ~ ELEV_MEAN_z + TREEVOL_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + SLOPE_SD_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + M1_prop_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + M3_prop_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + BuaRiver_indicator'),
                                       as.formula('psi ~ SLOPE_MEAN_z + TREEVOL_z'),
                                       as.formula('psi ~ SLOPE_MEAN_z + M1_prop_z'),
                                       as.formula('psi ~ SLOPE_MEAN_z + M3_prop_z'),
                                       as.formula('psi ~ SLOPE_MEAN_z + BuaRiver_indicator'),
                                       as.formula('psi ~ TREEVOL_z + ELEV_SD_z'),
                                       as.formula('psi ~ TREEVOL_z + SLOPE_SD_z'),
                                       as.formula('psi ~ TREEVOL_z + BuaRiver_indicator'),
                                       as.formula('psi ~ ELEV_SD_z + M1_prop_z'),
                                       as.formula('psi ~ ELEV_SD_z + M3_prop_z'),
                                       as.formula('psi ~ ELEV_SD_z + BuaRiver_indicator'),
                                       as.formula('psi ~ SLOPE_SD_z + M1_prop_z'),
                                       as.formula('psi ~ SLOPE_SD_z + M3_prop_z'),
                                       as.formula('psi ~ SLOPE_SD_z + BuaRiver_indicator'),
                                       as.formula('psi ~ ELEV_MEAN_z + TREEVOL_z + SLOPE_SD_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + TREEVOL_z + BuaRiver_indicator'),
                                       as.formula('psi ~ ELEV_MEAN_z + SLOPE_SD_z + M1_prop_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + SLOPE_SD_z + M3_prop_z'),
                                       as.formula('psi ~ ELEV_MEAN_z + SLOPE_SD_z + BuaRiver_indicator'),
                                       as.formula('psi ~ SLOPE_MEAN_z + TREEVOL_z + BuaRiver_indicator'),
                                       as.formula('psi ~ TREEVOL_z + ELEV_SD_z + M1_prop_z'),
                                       as.formula('psi ~ TREEVOL_z + ELEV_SD_z + M3_prop_z'),
                                       as.formula('psi ~ TREEVOL_z + ELEV_SD_z + BuaRiver_indicator'),
                                       as.formula('psi ~ TREEVOL_z + SLOPE_SD_z + M1_prop_z'),
                                       as.formula('psi ~ TREEVOL_z + SLOPE_SD_z + M3_prop_z'),
                                       as.formula('psi ~ TREEVOL_z + SLOPE_SD_z + BuaRiver_indicator'),
                                       as.formula('psi ~ ELEV_MEAN_z + TREEVOL_z + SLOPE_SD_z + BuaRiver_indicator')),
                    'Zebra' = list(as.formula('psi ~ BuaRiver_indicator'),
                                   as.formula('psi ~ SLOPE_SD_z'),
                                   as.formula('psi ~ SLOPE_MEAN_z'),
                                   as.formula('psi ~ ELEV_MEAN_z'),
                                   as.formula('psi ~ M2_prop_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_MEAN_z'),
                                   as.formula('psi ~ BuaRiver_indicator + ELEV_MEAN_z'),
                                   as.formula('psi ~ BuaRiver_indicator + M2_prop_z'),
                                   as.formula('psi ~ SLOPE_SD_z + ELEV_MEAN_z'),
                                   as.formula('psi ~ SLOPE_SD_z + M2_prop_z'),
                                   as.formula('psi ~ SLOPE_MEAN_z + M2_prop_z'),
                                   as.formula('psi ~ ELEV_MEAN_z + M2_prop_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z + ELEV_MEAN_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z + M2_prop_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_MEAN_z + M2_prop_z'),
                                   as.formula('psi ~ BuaRiver_indicator + ELEV_MEAN_z + M2_prop_z'),
                                   as.formula('psi ~ SLOPE_SD_z + ELEV_MEAN_z + M2_prop_z'),
                                   as.formula('psi ~ BuaRiver_indicator + SLOPE_SD_z + ELEV_MEAN_z + M2_prop_z')))
  
## Set up dataframe for model outputs
  modResults <- structure(list(model = character(), species = character(), aic = numeric()), class = 'data.frame')
  
## Initialize list to store results
  all_models = list()
  
  
  
## RUN UNIVARIATE MODELS -------------------------------------------------------
  
## Loop through all univariate models
  
  #for each species:
  for (ss in names(mod_lists)){
    
    #extract detection history
    dh_ss <- data.frame(dh_combined[dh_combined$species == ss,])
    rownames(dh_ss) <- dh_ss$hex
    dh_ss <- dh_ss %>% select(starts_with('X')) #keep only week columns

    #create PAO input file
    input_ss <- createPao(data = dh_ss, unitnames = rownames(dh_ss), unitcov = siteCovar)

    #for each model structure:
    all_models_ss <- list()  
    mod_lists_ss <- mod_lists[[ss]]
    for (uu in mod_lists_ss){
    
      #construct model name
      modname_uu = paste('Malawi/outputs/models/', Sys.Date(), '/', ss, '_pDOT', gsub(' ~ ', '', as.character(c(uu))), sep = '')
      
      #run model
      model_uu <- occMod(model = list(uu, p~1), data = input_ss, type = 'so', modname = modname_uu, outfile = 'modname_uu')
      
      #store results
      all_models_ss[[as.character(c(uu))]] <- model_uu
      modResults[nrow(modResults)+1,] <- c(paste('pDOT', gsub(' ~ ', '', as.character(c(uu))), sep = ''), ss, model_uu$aic)
    
    } #end model loop
    
    all_models[[ss]] <- all_models_ss
    
  } #end species loop
  
  #inspect
  names(all_models)
  names(all_models$Elephant)
  
  head(modResults)
  tail(modResults)

  #save CSV
  write_rds(all_models, paste('Malawi/outputs/models/', Sys.Date(), '/', 'allspecies', '_models_list_all.rds', sep = ''))
  write.csv(modResults, paste('Malawi/outputs/models/', Sys.Date(), '/', 'allspecies', '_model_table_all.csv', sep = ''))
  
    
  
## MODEL SELECTION  ------------------------------------------------------------
  
  #Create AIC tables from 'createAicTable' function
  aic_tables_detailed <- lapply(all_models, function(x) {
    aic <- summary(createAicTable(x))
    return(aic)
  })
  
  #Or create AIC tables manually
  aic_tables <- lapply(unique(modResults$species), function(x) {
    table <- modResults[modResults$species == x,]
    table <- table[order(table$aic),]
    table$delta_AIC <- as.numeric(table$aic) - min(as.numeric(table$aic))
    table$modlike <- exp(-table$delta_AIC/2)
    table$wgt <- as.numeric(table$modlike) / sum(as.numeric(table$modlike))
    return(table)
  })
  names(aic_tables) <- unique(modResults$species)

  #Compare:
  aic_tables_detailed$Eland
  aic_tables$Eland
  
  #SAVE
  write_rds(aic_tables, 'Malawi/outputs/models/2024-08-29/aic_tables_all.RDS')
  write_rds(aic_tables_detailed, 'Malawi/outputs/models/2024-08-29/aic_tables_all_detailed.RDS')
  

  