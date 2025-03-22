# Single-season occupancy models

## Step 2:
## - prep detection data

## **DO I NEED TO DO THIS? end result is the same as the existing file 'data/raw/detection_histories/dh_hexagon/detection_histories_hexagon_all_padwks_2018-2023.RDS'
## (that's just a list instead of a dataframe)



library(data.table)
library(tidyverse)
#library(RPresence)
#library(ggplot2)


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

  ifelse(!dir.exists(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')))
  
  ifelse(!dir.exists(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')))


    
## IMPORT DETECTION DATA (2018 thru 2023) -------------------------------------------
  
  ## Read in all detection histories and combine 
  dh_path <- 'data/raw/detection_histories/dh_hexagon/2025-02-21/'
  
  #list all csvs
  dh_files <- list.files(dh_path, pattern = '*.csv', full.names = TRUE)
  
  #read in and format each csv
  dh_list <- lapply(dh_files, function (file){
    dh <- read.csv(file)
    rownames(dh) <- dh$X #add hexagons as rownames
    dh <- dh[order(dh$X),] #order alphabetically by hexagon
    dh <- dh[,-1] #remove hexagon column now
    dh[dh > 0] <- 1 #convert to binary
    dh$file <- basename(file) #add column for filename (includes species)
    dh$hex <- rownames(dh) #add hex names back in
    dh$species <- gsub('dh_|_hexagon_2025-02-21.csv', '', dh$file) #pull out species name
      
    return(dh)
  } )
  
  #combine into one dataframe  
  dh_combined <- rbindlist(dh_list)
    head(dh_combined)
    tail(dh_combined) #the 'padded' wks are <lgcl> instead of <num> bc all are NAs -- this is ok
  
  table(dh_combined$species, useNA = 'a') #should be 69 rows (hexagons) for each species
  head(dh_combined[dh_combined$species == 'elephant',]) #example
  

  
## SAVE CLEANED DETECTION DATA -------------------------------------------------
  write.csv(dh_combined, 'data/cleaned/combined_dethist_thru2023.csv')
  
  
  
  