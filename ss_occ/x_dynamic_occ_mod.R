## Dynamic occupancy models

library(data.table)
library(unmarked)


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------
  
  ifelse(!dir.exists(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')))
  
  ifelse(!dir.exists(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')))


## CREATE MODEL INPUTS ---------------------------------------------------------
  
  ## Read in detection data and hexagon covariate data
  # dh_combined <- fread('data/combined_dethist_thru2023_011425.csv')
  dh_combined <- readRDS('data/raw/detection_histories/dh_hexagon/detection_histories_hexagon_all_padwks_2018-2023.RDS')
  hex_data <- fread('data/cleaned/hex_covariates.csv')
    
    #pad hex names to match
    hex_data$hex_padded <- sprintf('%s%02d', substr(hex_data$hex, 1, 1),
                                  as.numeric(substr(hex_data$hex, 2, nchar(hex_data$hex))))
  
  ## Create site covariate dataframe
  siteCovar <- data.frame(hex_data[hex_data$hex_padded %in% rownames(dh_combined[[1]]), #keep only the 69 hexagons in det histories
                                  c('M1_prop_z','M2_prop_z','M3_prop_z','TREEVOL_z','SLOPE_MEAN_z','SLOPE_SD_z',
                                    'DAMBO_DIST_z','FIRE_YRS_z','ELEV_MEAN_z','ELEV_SD_z','COST_ELEV_z','BuaRiver_indicator',
                                    'hex_padded')]) #use these columns

  
  ## NOTE: this is all the hex level, not camera level
  
  
## TEST  -----------------------------------------------------------------------
  
  #get one species
  det <- dh_combined$elephant
    dim(det) #69 hex * 313 sampling periods (6 yrs)
    
  #create inputs
  year <- matrix(c('2018','2019','2020','2021','2022','2023'), nrow(det), 6, byrow=TRUE)
    dim(year)
  
  umf <- unmarkedMultFrame(y = det,
                           yearlySiteCovs = list(year = year),
                           numPrimary = 6)
  
  
  
  
  yy <- matrix(y, M, J*T)  
  yy
  
  year <- matrix(c('01','02','03','04','05','06','07','08','09','10'),
                 nrow(yy), T, byrow=TRUE)
  
  simUMF <- unmarkedMultFrame(
    y = det[,-c('file','hex','species')],
    yearlySiteCovs = list(year = year),
    numPrimary = 6)
  
  summary(simUMF)
  
  