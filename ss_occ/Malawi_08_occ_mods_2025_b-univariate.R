######
## STEP 8 - Run occupancy models for 2025 chapter (2018-23 detections)
##          (Uses detection histories previously created using 'Malawi_07_det_hist_species.R')
##
## B: - run univariate models
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
  dh_combined <- fread('Malawi/outputs/models/combined_dethist_thru2023_011425.csv')
  hexData <- fread('Malawi/outputs/models/hex_covariates.csv')
    
  #pad hex names to match
  hexData$hex_padded <- sprintf('%s%02d', substr(hexData$hex, 1, 1),
                                as.numeric(substr(hexData$hex, 2, nchar(hexData$hex))))
  
## Create site covariate dataframe
  siteCovar <- data.frame(hexData[hexData$hex_padded %in% unique(dh_combined$hex), #keep only the 69 hexagons in det histories
                                  c('M1_prop_z','M2_prop_z','M3_prop_z','TREEVOL_z','SLOPE_MEAN_z','SLOPE_SD_z',
                                    'DAMBO_DIST_z','FIRE_YRS_z','ELEV_MEAN_z','ELEV_SD_z','COST_ELEV_z','BuaRiver_indicator',
                                    'hex_padded')]) #use these columns
  
## Create list of univariate models
  (univar_psi <- lapply(c(1,colnames(siteCovar)), function(x) as.formula(paste('psi ~', x)))) #include 1 for dot model too
  
## Set up dataframe for model outputs
  modResults <- structure(list(model = character(), species = character(), aic = numeric()), class = 'data.frame')

## Initialize list to store results
  univar_models = list()
  
  
## RUN UNIVARIATE MODELS -------------------------------------------------------
  
## Loop through all univariate models
  
  #for each species:
  for (ss in unique(dh_combined$species)){
    
    #extract detection history
    dh_ss <- data.frame(dh_combined[dh_combined$species == ss,])
    rownames(dh_ss) <- dh_ss$hex
    dh_ss <- dh_ss %>% select(starts_with('X')) #keep only week columns

    #create PAO input file
    input_ss <- createPao(data = dh_ss, unitnames = rownames(dh_ss), unitcov = siteCovar)

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

  #save CSV
  # write.csv(modResults[modResults$species %in% speciesName,], paste('Malawi/outputs/models/', Sys.Date(), '/', speciesName, '_param_values.csv', sep = ''))
  write_rds(univar_models, paste('Malawi/outputs/models/', Sys.Date(), '/', 'allspecies', '_models_list_univariate.rds', sep = ''))
  write.csv(modResults, paste('Malawi/outputs/models/', Sys.Date(), '/', 'allspecies', '_model_table_univariate.csv', sep = ''))
  
    
  
## MODEL SELECTION  ------------------------------------------------------------
  
  #Create AIC tables from 'createAicTable' function
  aic_tables_detailed <- lapply(univar_models, function(x) {
    aic <- summary(createAicTable(x))
    return(aic)
  })
  
  #Or create AIC tables manually
  aic_tables <- lapply(unique(modResults$species), function(x) {
    table <- modResults[modResults$species == x,]
    table <- table[order(table$aic),]
    table$delta_AIC <- as.numeric(table$aic) - min(as.numeric(table$aic))
    table$delta_AIC <- as.numeric(table$aic) - min(as.numeric(table$aic))
    table$modlike <- exp(-table$delta_AIC/2)
    table$wgt <- as.numeric(table$modlike) / sum(as.numeric(table$modlike))
    return(table)
  })
  names(aic_tables) <- sort(unique(modResults$species))
    head(aic_tables)
  
  #compare:
  aic_tables[[1]]
  aic_tables_detailed[[1]]
  
  #SAVE
  write_rds(aic_tables, 'Malawi/outputs/models/2024-08-29/aic_tables_univariate.RDS')
  write_rds(aic_tables_detailed, 'Malawi/outputs/models/2024-08-29/aic_tables_univariate_detailed.RDS')
  

  
  