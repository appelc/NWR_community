# Now take reviewed fire detections and convert to covariate and/or detection history

library(data.table)
library(tidyverse)
library(zoo)


# Read in validated detections -------------------------------------------------
dir <- 'clip_fire/'
(file_names <- list.files(dir, pattern = '*working.csv', full.names = TRUE))

  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL))
  
  #combine
  reviewed <- rbindlist(file_list, fill = TRUE)
  
  #QC
  table(reviewed$fire, useNA = 'a')

    
# Summarize and format  --------------------------------------------------------
fires <- reviewed %>% filter(fire == 1) %>% group_by(site_check) %>%
  separate(site_check, c('hex','cam','check_yr','check_month','check_day'), remove = FALSE)

  #format dates and pad hex names
  fires$date <- as.POSIXct(strptime(fires$date, '%m/%d/%y'), tz = 'Africa/Blantyre')
  fires$hex_padded <- paste(substr(fires$hex, 1, 1),
                            str_pad(str_sub(fires$hex, 2), width = 2, side = 'left', pad = '0'), sep = '')
  fires$site <- paste(fires$hex_padded, fires$cam, sep = '-')
  
  #keep only columns we need
  fire_df <- fires %>% ungroup() %>% select(site, date, fire)

  #read in detection history template
  det_template <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')
  det_template <- det_template[[1]] #only need a few columns from the template, not species data
  det_template <- det_template %>% select(site, date, surv)
  det_template$date <- as.POSIXct(strptime(det_template$date, format = '%Y-%m-%d'), tz = 'Africa/Blantyre')
  
  #match up
  fire_data <- det_template %>% left_join(fire_df, join_by(site == site, date == date))
  
  #QC: any recorded fires outside survey period?
  table(fire_data$surv, fire_data$fire, useNA = 'a') #nope, good
  
  #now calculate 'time since fire'
  fire_data <- fire_data %>% group_by(site) %>% arrange(date) %>% 
    mutate(last_fire = ifelse(surv == 1, na.locf(ifelse(fire == 1, date, NA), na.rm = FALSE), NA), 
           days_since_fire = ifelse(surv == 1, as.numeric(difftime(date, last_fire, units = 'days')), NA)) %>%
    arrange(site)

  #set max threshold and cap
  (max_days <- 60)
  fire_data <- fire_data %>% mutate(days_since_fire_adj = pmin(days_since_fire, max_days))

  #also set to max if there hasn't been a fire detected there yet (only if surv == 1)
  fire_data$days_since_fire_adj <- ifelse(fire_data$surv == 0, NA, #if not surveyed, keep as NA
                                          ifelse(!is.na(fire_data$days_since_fire_adj), fire_data$days_since_fire_adj, #if surveyed, if there has been a fire, use that
                                                 max_days)) #otherwise, use max
  
  #QC again: any outside surv period?
  table(fire_data[fire_data$surv == 0,]$days_since_fire_adj, useNA = 'a') #good, should all be NAs
  table(fire_data[fire_data$surv == 1,]$days_since_fire_adj, useNA = 'a')
  
  #save
  fire_covariate <- fire_data %>% select(site, date, days_since_fire_adj)
  
  
# Convert to covariate format --------------------------------------------------
  
  ## primary periods 1 week, secondary periods 1 day ---------------------------
    
    #read in dates
    dates_1_7 <- fread('data/cleaned/period_1-7_date_key.csv')
    dates_1_7$date <- as.POSIXct(strptime(dates_1_7$date, format = '%Y-%m-%d'), tz = 'Africa/Blantyre')
  
    #merge fire data with sampling periods
    fire_covariate_1_7 <- fire_covariate %>% left_join(dates_1_7[,c('date','primary','secondary','period')],
                                                       join_by(date == date))
    
    #create WIDE version for dynamic model (site x (primary x secondary))
    fire_covariate_1_7_wide <- reshape2::dcast(fire_covariate_1_7, site ~ factor(period, levels = unique(fire_covariate_1_7$period)),
                                               value.var = 'days_since_fire_adj',
                                               fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    
      #split into BEFORE and AFTER fence decomissioning
      cutoff <- '126-7'
      cutoff_index <- which(names(fire_covariate_1_7_wide) == cutoff)
      fire_covariate_1_7_wide_before <- fire_covariate_1_7_wide[, 1:cutoff_index]
      fire_covariate_1_7_wide_after <- fire_covariate_1_7_wide[, c(1, (cutoff_index + 1):ncol(fire_covariate_1_7_wide))]
    
    #create STACKED version (site_primary x secondary)
    fire_covariate_1_7$site_primary <- paste(fire_covariate_1_7$site, 
                                             formatC(as.numeric(fire_covariate_1_7$primary), width = 3, flag = '0'), sep = '_')
    fire_covariate_1_7$secondary <- sprintf('%02d', as.numeric(fire_covariate_1_7$secondary))
    fire_covariate_1_7_stacked <- fire_covariate_1_7 %>% ungroup() %>% select(site_primary, days_since_fire_adj, secondary) %>%
      pivot_wider(names_from = secondary, values_from = days_since_fire_adj)
    
      #split into BEFORE and AFTER fence decomissioning
      cutoff_date <- '2021-04-01'
      fire_covariate_1_7$before_after <- ifelse(fire_covariate_1_7$date <= cutoff_date, 'before', 'after')
      
      fire_covariate_1_7_stacked_before <- fire_covariate_1_7 %>% ungroup() %>% filter(before_after == 'before') %>%
        select(site_primary, days_since_fire_adj, secondary) %>%
        pivot_wider(names_from = secondary, values_from = days_since_fire_adj)
    
      fire_covariate_1_7_stacked_after <- fire_covariate_1_7 %>% ungroup() %>% filter(before_after == 'after') %>%
        select(site_primary, days_since_fire_adj, secondary) %>%
        pivot_wider(names_from = secondary, values_from = days_since_fire_adj)
      
    #save
    write.csv(fire_covariate_1_7_wide, 'data/cleaned/covariates/fire_1_7_wide.csv')
    write.csv(fire_covariate_1_7_wide_before, 'data/cleaned/covariates/fire_1_7_wide_before.csv')
    write.csv(fire_covariate_1_7_wide_after, 'data/cleaned/covariates/fire_1_7_wide_after.csv')
    write.csv(fire_covariate_1_7_stacked, 'data/cleaned/covariates/fire_1_7_stacked.csv')  
    write.csv(fire_covariate_1_7_stacked_before, 'data/cleaned/covariates/fire_1_7_stacked_before.csv')  
    write.csv(fire_covariate_1_7_stacked_after, 'data/cleaned/covariates/fire_1_7_stacked_after.csv')  
    
  
  #primary periods 18 weeks (3 SEASONS), secondary periods 7 days --------------
  
    #read in dates
    dates_3_18 <- fread('data/cleaned/period_3-18_date_key.csv')
    dates_3_18$date <- as.POSIXct(strptime(dates_3_18$date, format = '%Y-%m-%d'), tz = 'Africa/Blantyre')
    
    #merge fire data with sampling periods
    fire_covariate_3_18 <- fire_covariate %>% left_join(dates_3_18[,c('date','primary','secondary','period')],
                                                       join_by(date == date))
    
    #create WIDE version for dynamic model (site x (primary x secondary))
    
    #RULES TO SUMMARIZE AT A WEEKLY LEVEL: 
    max_days
    #if all are NA, use NA
    #if all are 60 days, use 8 weeks
    #if any is 0 days, use 0 weeks (this is the week with fire)
    #if any is 1-59, use floor(max(x)/7)
    
    weekly_aggregate <- function(x) {
      if (all(is.na(x))) {
        return(NA_real_)  #if all are NA, return NA
      } 
      else if (all(is.na(x) | x == max_days)) {
        return(floor(max_days / 7))  #if all are either 60 or NA, return 8 weeks
      }
      else if (any(x == 0, na.rm = TRUE)) {
        return(0)  #if any is 0, return 0
      }
      else if (any(x >= 1 & x <= (max_days-1), na.rm = TRUE)) {
        return(floor(max(x, na.rm = TRUE) / 7))  #if any is between 0-59, convert to wks
      }
      return(NA_real_) #if none are met, return NA
    }
    
    fire_covariate_3_18_wide <- reshape2::dcast(fire_covariate_3_18, site ~ factor(period, levels = unique(fire_covariate_3_18$period)),
                                               value.var = 'days_since_fire_adj',
                                               fun.aggregate = weekly_aggregate)
    
      #remove 'dry_2018' (only has 5 weeks) and pad out 'dry_2023' (has 13 weeks)
      fire_covariate_3_18_wide <- fire_covariate_3_18_wide %>% select(-"dry_2018-14", -"dry_2018-15", -"dry_2018-16", -"dry_2018-17", -"dry_2018-18")
      fire_covariate_3_18_wide <- fire_covariate_3_18_wide %>% mutate('dry_2023-14' = NA, 'dry_2023-15' = NA, 'dry_2023-16' = NA, 
                                          'dry_2023-17' = NA, 'dry_2023-18' = NA)
      
      #split into BEFORE and AFTER fence decomissioning
      cutoff <- 'wet_2020-18'
      cutoff_index <- which(names(fire_covariate_3_18_wide) == cutoff)
      fire_covariate_3_18_wide_before <- fire_covariate_3_18_wide[, 1:cutoff_index] #columns through cutoff season (inclusive)
      fire_covariate_3_18_wide_after <- fire_covariate_3_18_wide[, c(1, (cutoff_index + 1):ncol(fire_covariate_3_18))] #columns after
      
    #create STACKED version (site_primary x secondary)      
    fire_covariate_3_18$site_primary <- paste(fire_covariate_3_18$site, fire_covariate_3_18$primary, sep = '_')
    fire_covariate_3_18$secondary <- sprintf('%02d', as.numeric(fire_covariate_3_18$secondary))
    
    fire_covariate_3_18_stacked <- fire_covariate_3_18 %>% ungroup() %>%
      select(site_primary, days_since_fire_adj, secondary) %>%
      pivot_wider(names_from = secondary, values_from = days_since_fire_adj, names_sort = TRUE,
                  values_fn = weekly_aggregate)
    
      #split into BEFORE and AFTER fence decomissioning
      cutoff_date <- max(dates_3_18[dates_3_18$period == cutoff,]$date)
      fire_covariate_3_18$before_after <- ifelse(fire_covariate_3_18$date <= cutoff_date, 'before', 'after')
        
      fire_covariate_3_18_stacked_before <- fire_covariate_3_18 %>% ungroup() %>% filter(before_after == 'before') %>%
        select(site_primary, days_since_fire_adj, secondary) %>%
        pivot_wider(names_from = secondary, values_from = days_since_fire_adj, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
  
      fire_covariate_3_18_stacked_after <- fire_covariate_3_18 %>% ungroup() %>% filter(before_after == 'after') %>%
        select(site_primary, days_since_fire_adj, secondary) %>%
        pivot_wider(names_from = secondary, values_from = days_since_fire_adj, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
    #save
    write.csv(fire_covariate_3_18_wide, 'data/cleaned/covariates/fire_3_18_wide.csv')
    write.csv(fire_covariate_3_18_wide_before, 'data/cleaned/covariates/fire_3_18_wide_before.csv')
    write.csv(fire_covariate_3_18_wide_after, 'data/cleaned/covariates/fire_3_18_wide_after.csv')
    write.csv(fire_covariate_3_18_stacked, 'data/cleaned/covariates/fire_3_18_stacked.csv')  
    write.csv(fire_covariate_3_18_stacked_before, 'data/cleaned/covariates/fire_3_18_stacked_before.csv')  
    write.csv(fire_covariate_3_18_stacked_after, 'data/cleaned/covariates/fire_3_18_stacked_after.csv')  
    
    
  #primary periods 30 days, secondary periods 10 days --------------------------
    
    ## Need to figure out rules to summarize here. Will I actually use this format?
    
    
    