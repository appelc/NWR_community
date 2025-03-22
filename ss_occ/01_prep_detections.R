## Modify detection histories for given primary/secondary sampling periods

library(data.table)
library(tidyverse)


# 30-day PRIMARY PERIODS (10-30)  ----------------------------------------------

## Create folder to store results ----------------------------------------------
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam_before', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam_before', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam_after', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam_after', sep = '/')), 'Folder exists already')
  
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked', sep = '/')), 
           dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_before', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_before', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_after', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_after', sep = '/')), 'Folder exists already')
  
    
## Read in long-format camera-level detection histories  -----------------------
  dh_long <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')
    names(dh_long)
    head(dh_long[[1]])
  

## Establish sampling periods --------------------------------------------------

  #get start/end dates
  (beg <- min(dh_long[[1]]$date))
  (end <- max(dh_long[[1]]$date))
  (n_days <- interval(beg, end)/days(1))
  
  #set primary period length and assign to all dates
  primary <- 30
  (n_primary <- round(n_days / primary))
  dates <- format(as.Date(beg) + days(0:(n_days)), format = '%Y-%m-%d') #check days(0:nDays) or days(1:(nDays+1))
  dates_df <- data.frame('date' = as.POSIXct(strptime(dates, '%Y-%m-%d'), tz = 'Africa/Blantyre'))
  dates_df$primary <- rep(1:nrow(dates_df), each = primary, length.out = nrow(dates_df))
  
  #set secondary period
  secondary <- 10
  (n_secondary <- round(primary/secondary))
  dates_df$secondary <- rep(1:n_secondary, each = secondary, length.out = nrow(dates_df))

  #create a combined field
  dates_df$period <- paste(formatC(as.numeric(dates_df$primary), width = 2, flag = "0"), 
                           formatC(as.numeric(dates_df$secondary), width = 2, flag = "0"), sep = '-')
  
  #do all primary periods have the same number of secondary periods?
  (n_periods <- n_primary * n_secondary) #should have this many
  length(unique(dates_df$period)) #yes, good
  
  #get season based on the 1st day of each primary period (not *quite* right... doesn't match actual months)
  dates_primary <- dates_df %>% group_by(primary) %>% summarise(start_date = min(date), 
                                                                month = month(start_date))
  dates_primary$season <- NA
  dates_primary[dates_primary$month %in% c(12,1,2,3),]$season <- 'wet'  
  dates_primary[dates_primary$month %in% c(4,5,6,7),]$season <- 'cool'
  dates_primary[dates_primary$month %in% c(8,9,10,11),]$season <- 'dry'  
  
  #match back up to dates
  dates_df$season <- dates_primary$season[match(dates_df$primary, dates_primary$primary)]
  
  #save date/period key
  write.csv(dates_df, 'data/cleaned/period_10-30_date_key.csv')
  
  
## Convert to wide -------------------------------------------------------------
  dh_wide <- list()
  dh_wide_before <- list()
  dh_wide_after <- list()
  dh_stacked <- list()
  dh_stacked_before <- list()
  dh_stacked_after <- list()
  
  #loop thru each species to match dates with periods and convert to wide
  for (sp in names(dh_long)){
    
    #get long-format det hist for this species
    dh_sp <- dh_long[[sp]]
    
    #format date
    dh_sp$date <- as.POSIXct(strptime(dh_sp$date, '%Y-%m-%d'), tz = 'Africa/Blantyre')
    
    #merge date/period key
    dh_sp <- left_join(dh_sp, dates_df, by = 'date')
    
    ## WIDE:
      #aggregate by period
      dh_wide_sp <- reshape2::dcast(dh_sp[,-c('primary','secondary')], site ~ factor(period, levels = unique(dh_sp$period)),
                                    value.var = 'n_photos_cleaned', 
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      #further split into BEFORE and AFTER fence decomissioning 
      cutoff <- '30-03'
      cutoff_index <- which(names(dh_wide_sp) == cutoff)
      
      dh_wide_sp_before <- dh_wide_sp[, 1:cutoff_index] #columns through cutoff season (inclusive)
      dh_wide_sp_after <- dh_wide_sp[, c(1, (cutoff_index + 1):ncol(dh_wide_sp))] #columns after
      
    ## STACKED:      
      #also create stacked version
      dh_sp$site_primary <- paste(dh_sp$site, formatC(as.numeric(dh_sp$primary), width = 2, flag = "0"), sep = '_')
      dh_stacked_sp <- reshape2::dcast(dh_sp[,c('site_primary', 'fence', 'season', 'class', 'n_photos_cleaned', 'secondary')],
                                       site_primary + fence + season + class ~ factor(secondary, levels = unique(dh_sp$secondary)),
                                       value.var = 'n_photos_cleaned',
                                       fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      #further split into BEFORE and AFTER fence decomissioning
      cutoff_date <- '2021-04-19'
      dh_sp$before_after <- ifelse(dh_sp$date <= cutoff_date, 'before', 'after')
      
      dh_stacked_sp_before <- dh_sp %>% filter(before_after == 'before') %>%
        select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
        pivot_wider(names_from = secondary, values_from = n_photos_cleaned, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      dh_stacked_sp_after <- dh_sp %>% filter(before_after == 'after') %>%
        select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
        pivot_wider(names_from = secondary, values_from = n_photos_cleaned, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      
    #save
    dh_wide[[sp]] <- dh_wide_sp
    dh_wide_before[[sp]] <- dh_wide_sp_before
    dh_wide_after[[sp]] <- dh_wide_sp_after
    write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_10-30_periods_cam/', 'dh_wide_cam_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp_before, paste('data/cleaned/detection_histories_10-30_periods_cam_before/', 'dh_wide_cam_before_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp_after, paste('data/cleaned/detection_histories_10-30_periods_cam_after/', 'dh_wide_cam_after_', sp, '.csv', sep = ''))
    
    dh_stacked[[sp]] <- dh_stacked_sp
    dh_stacked_before[[sp]] <- dh_stacked_sp_before
    dh_stacked_after[[sp]] <- dh_stacked_sp_after
    write.csv(dh_stacked_sp, paste('data/cleaned/detection_histories_10-30_periods_cam_stacked/', 'dh_wide_cam_stacked_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_before, paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_before/', 'dh_wide_cam_stacked_before_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_after, paste('data/cleaned/detection_histories_10-30_periods_cam_stacked_after/', 'dh_wide_cam_stacked_after_', sp, '.csv', sep = ''))
  }
  
  #save
  write_rds(dh_wide, 'data/cleaned/detection_histories_10-30_periods_cam/dh_wide_cam_all.RDS')
  write_rds(dh_wide_before, 'data/cleaned/detection_histories_10-30_periods_cam_before/dh_wide_cam_all_before.RDS')
  write_rds(dh_wide_after, 'data/cleaned/detection_histories_10-30_periods_cam_after/dh_wide_cam_all_after.RDS')
  
  write_rds(dh_stacked, 'data/cleaned/detection_histories_10-30_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')
  write_rds(dh_stacked_before, 'data/cleaned/detection_histories_10-30_periods_cam_stacked_before/dh_wide_cam_all_stacked_before.RDS')
  write_rds(dh_stacked_after, 'data/cleaned/detection_histories_10-30_periods_cam_stacked_after/dh_wide_cam_all_stacked_after.RDS')
  

# ## Aggregate to hexagon-level --------------------------------------------------
#   dh_wide_hex <- list()
#   
#   #loop through each class
#   for (ss in names(dh_wide)){
#     
#     #get wide-format det hist for this species
#     dh_wide_ss <- dh_wide[[ss]]
#     
#     #find total number of periods
#     # (total_p <- ncol(dh_wide_ss) - 1)
#     n_periods
#     
#     #add hexagon column
#     dh_wide_ss$hex <- sapply(strsplit(as.character(dh_wide_ss$site), '\\-'), '[', 1)
#     
#     #create empty dataframe with sites * periods
#     dh_wide_hex_ss <- data.frame(matrix(nrow = length(unique(dh_wide_ss$hex)), ncol = n_periods))
#     rownames(dh_wide_hex_ss) <- sort(unique(dh_wide_ss$hex))
#   
#     #iterate thru hexagons -- considers a week surveyed if ANY of the cameras were recording that week
#     for (hh in unique(dh_wide_ss$hex)){
#       dh_wide_ss_hh <- dh_wide_ss %>% filter(hex == hh) %>% select(-c(site,hex)) #keep rows for this hex only
#       dh_wide_hex_ss_hh <- apply(dh_wide_ss_hh, 2, max, na.rm = TRUE) #take max value for each period *WARNINGS OK, JUST MEANS NAs*
#       dh_wide_hex_ss[hh,] <- dh_wide_hex_ss_hh
#     }
#     
#     #convert -Inf to NAs
#     dh_wide_hex_ss[dh_wide_hex_ss == -Inf] <- NA
#     
#     #save
#     dh_wide_hex[[ss]] <- dh_wide_hex_ss
#     write.csv(dh_wide_hex_ss, paste('data/cleaned/detection_histories_10-30_periods_hex/', 'dh_wide_hex_', sp, '.csv', sep = ''))
#   }
#   
#   #inspect
#   names(dh_wide_hex)
#   head(dh_wide_hex$baboon)
#   
#   #save
#   write_rds(dh_wide_hex, 'data/cleaned/detection_histories_10-30_periods_hex/dh_wide_hex_all.RDS')

  
  #Any other formatting here, like convert to binary? Otherwise, proceed to occ mod  
  


# WEEKS AS PRIMARY PERIODS (1-7)  ----------------------------------------------
  
## Create folder to store results ----------------------------------------------
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam_before', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam_before', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam_after', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam_after', sep = '/')), 'Folder exists already')
  
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_before', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_before', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_after', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_after', sep = '/')), 'Folder exists already')
  
    
## Read in long-format camera-level detection histories  -----------------------
  dh_long <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')
  names(dh_long)
  
  
## Establish sampling periods --------------------------------------------------
  
  #get start/end dates
  (beg <- min(dh_long[[1]]$date))
  (end <- max(dh_long[[1]]$date))
  (n_days <- interval(beg, end)/days(1))
  
  #set primary period length and assign to all dates
  primary <- 7
  (n_primary <- ceiling(n_days / primary))
  dates <- format(as.Date(beg) + days(0:(n_days)), format = '%Y-%m-%d') #check days(0:nDays) or days(1:(nDays+1))
  dates_df <- data.frame('date' = as.POSIXct(strptime(dates, '%Y-%m-%d'), tz = 'Africa/Blantyre'))
  dates_df$primary <- rep(1:nrow(dates_df), each = primary, length.out = nrow(dates_df))
  
  #set secondary period
  secondary <- 1
  (n_secondary <- round(primary/secondary))
  dates_df$secondary <- rep(1:n_secondary, each = secondary, length.out = nrow(dates_df))
  
  #create a combined field
  dates_df$period <- paste(formatC(as.numeric(dates_df$primary), width = 3, flag = "0"), 
                           formatC(as.numeric(dates_df$secondary), width = 1, flag = "0"), sep = '-')
  
  #do all primary periods have the same number of secondary periods?
  (n_periods <- n_primary * n_secondary) #should have this many
  length(unique(dates_df$period)) #no, need to pad the last occasion
  
  #must be a better way to do this...
  tail(dates_df)
  dates_df <- rbind(dates_df,
                    data.frame('date' = c(max(dates_df$date) + days(seq(1:5))),
                               'primary' = rep(261, 5),
                               'secondary' = seq(3, 7),
                               'period' = c('261-3','261-4','261-5','261-6','261-7')))
  length(unique(dates_df$period)) #good
  table(dates_df$primary)
  table(dates_df$secondary)
  
  #get calendar week and season based on the 1st day of each primary period
  dates_primary <- dates_df %>% group_by(primary) %>% summarise(start_date = min(date), 
                                                                month = month(start_date),
                                                                calendar_week = week(start_date))
  dates_primary$season <- NA
  dates_primary[dates_primary$month %in% c(12,1,2,3),]$season <- 'wet'  
  dates_primary[dates_primary$month %in% c(4,5,6,7),]$season <- 'cool'
  dates_primary[dates_primary$month %in% c(8,9,10,11),]$season <- 'dry'  
  
  #match back up to dates
  dates_df$calendar_week <- dates_primary$calendar_week[match(dates_df$primary, dates_primary$primary)]
  dates_df$season <- dates_primary$season[match(dates_df$primary, dates_primary$primary)]
  
  #save date/period key
  write.csv(dates_df, 'data/cleaned/period_1-7_date_key.csv')
  
  
## Convert to wide -------------------------------------------------------------
  dh_wide <- list()
  dh_wide_before <- list()
  dh_wide_after <- list()
  dh_stacked <- list()
  dh_stacked_before <- list()
  dh_stacked_after <- list()
  
  #loop thru each species to match dates with periods and convert to wide
  for (sp in names(dh_long)){
    
    #get long-format det hist for this species
    dh_sp <- dh_long[[sp]]
    
    #format date
    dh_sp$date <- as.POSIXct(strptime(dh_sp$date, '%Y-%m-%d'), tz = 'Africa/Blantyre')
    
    #merge date/period key
    dh_sp <- left_join(dh_sp, dates_df, by = 'date')
    
    ## WIDE:
    #aggregate by period
    dh_wide_sp <- reshape2::dcast(dh_sp, site ~ factor(period, levels = unique(dh_sp$period)),
                                  value.var = 'n_photos_cleaned', 
                                  fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    
    #further split into BEFORE and AFTER fence decomissioning 
    cutoff <- '126-7'
    cutoff_index <- which(names(dh_wide_sp) == cutoff)
    
    dh_wide_sp_before <- dh_wide_sp[, 1:cutoff_index] #columns through cutoff season (inclusive)
    dh_wide_sp_after <- dh_wide_sp[, c(1, (cutoff_index + 1):ncol(dh_wide_sp))] #columns after
    
    ## STACKED:
    #also create stacked version
    dh_sp$site_primary <- paste(dh_sp$site, formatC(as.numeric(dh_sp$primary), width = 3, flag = "0"), sep = '_')
    dh_stacked_sp <- dh_sp %>% select(site_primary, fence, class, n_photos_cleaned, secondary, calendar_week, season) %>%
      pivot_wider(names_from = secondary, values_from = n_photos_cleaned)
    
    #further split into BEFORE and AFTER fence decomissioning
    cutoff_date <- '2021-04-01'
    dh_sp$before_after <- ifelse(dh_sp$date <= cutoff_date, 'before', 'after')
    
    dh_stacked_sp_before <- dh_sp %>% filter(before_after == 'before') %>%
      select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
      pivot_wider(names_from = secondary, values_from = n_photos_cleaned)
                  #names_sort = TRUE,
                  #values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE)) ##shouldn't need this...
    
    dh_stacked_sp_after <- dh_sp %>% filter(before_after == 'after') %>%
      select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
      pivot_wider(names_from = secondary, values_from = n_photos_cleaned)
                  #names_sort = TRUE,
                  #values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    
    
    #save
    dh_wide[[sp]] <- dh_wide_sp
    dh_wide_before[[sp]] <- dh_wide_sp_before
    dh_wide_after[[sp]] <- dh_wide_sp_after
    write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_1-7_periods_cam/', 'dh_wide_cam_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_1-7_periods_cam_before/', 'dh_wide_cam_before_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_1-7_periods_cam_after//', 'dh_wide_cam_after_', sp, '.csv', sep = ''))
    
    dh_stacked[[sp]] <- dh_stacked_sp
    dh_stacked_before[[sp]] <- dh_stacked_sp_before
    dh_stacked_after[[sp]] <- dh_stacked_sp_after
    write.csv(dh_stacked_sp, paste('data/cleaned/detection_histories_1-7_periods_cam_stacked/', 'dh_wide_cam_stacked_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_before, paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_before/', 'dh_wide_cam_stacked_before_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_after, paste('data/cleaned/detection_histories_1-7_periods_cam_stacked_after/', 'dh_wide_cam_stacked_after_', sp, '.csv', sep = ''))
  }
  
  #save
  write_rds(dh_wide, 'data/cleaned/detection_histories_1-7_periods_cam/dh_wide_cam_all.RDS')
  write_rds(dh_wide_before, 'data/cleaned/detection_histories_1-7_periods_cam_before/dh_wide_cam_all_before.RDS')
  write_rds(dh_wide_after, 'data/cleaned/detection_histories_1-7_periods_cam_after/dh_wide_cam_all_after.RDS')

  write_rds(dh_stacked, 'data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')
  write_rds(dh_stacked_before, 'data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked_before.RDS')
  write_rds(dh_stacked_after, 'data/cleaned/detection_histories_1-7_periods_cam_stacked/dh_wide_cam_all_stacked_after.RDS')
  
  

# SEASONS AS PRIMARY PERIODS (3-18)  --------------------------------------------
  
## Create folder to store results ----------------------------------------------
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_3-18_periods_cam', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_3-18_periods_cam', sep = '/')), 'Folder exists already')
  ifelse(!dir.exists(paste('data/cleaned/detection_histories_3-18_periods_cam_stacked', sep = '/')), 
         dir.create(paste('data/cleaned/detection_histories_3-18_periods_cam_stacked', sep = '/')), 'Folder exists already')
  
  
## Read in long-format camera-level detection histories  -----------------------
  dh_long <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')
  
  
## Establish sampling periods --------------------------------------------------
  
  #get start/end dates
  (beg <- min(dh_long[[1]]$date))
  (end <- max(dh_long[[1]]$date))
  (n_days <- interval(beg, end)/days(1))
  
  #set primary period start/end dates (seasons: wet=Dec-March, cool=Apr-Jul, dry=Aug-Nov)
  (n_primary <- 3)
  (n_yrs <- year(end) - year(beg) + 1)
  dates_primary <- data.frame('season' = rep(c('dry','wet','cool'), length.out = n_yrs * n_primary),
                              'year' = rep(year(beg):year(end), each = n_primary),
                              'start' = rep(c('08-01','12-01','04-01'), length.out = n_yrs * n_primary))
  dates_primary$period <- paste(dates_primary$season, dates_primary$year, sep = '_')
  dates_primary$start <- as.POSIXct(strptime(paste(dates_primary$year, dates_primary$start, sep = '-'),
                                             format = '%Y-%m-%d'), tz = 'Africa/Blantyre')
  dates_primary <- dates_primary %>% arrange(start) %>% mutate(end = lead(start) - days(1)) #subtract 1 to not include the next start date

  #get all dates between the start of the first season and end of last season
  (first <- min(dates_primary$start))
  (last <- max(dates_primary$end[!is.na(dates_primary$end)]))
  (n_days <- interval(first, last)/days(1))
  
  dates <- format(as.Date(first) + days(1:(n_days)), format = '%Y-%m-%d') #check days(0:nDays) or days(1:(nDays+1))
  dates_df <- data.frame('date' = as.POSIXct(strptime(dates, '%Y-%m-%d'), tz = 'Africa/Blantyre'))
  
  #now match up primary periods with dates
  dates_df <- dates_df %>% rowwise() %>% 
                mutate(primary = dates_primary$period[which(date >= dates_primary$start & date <= dates_primary$end)]) %>% ungroup()
  
  #set secondary period
  secondary <- 7
  dates_df <- dates_df %>% group_by(primary) %>% mutate(secondary = ceiling(row_number() / secondary)) %>% ungroup()
  
  #create a combined field
  dates_df$period <- paste(dates_df$primary, 
                           formatC(as.numeric(dates_df$secondary), width = 2, flag = "0"), sep = '-')

  #do all primary periods have the same number of secondary periods?
  (n_primary <- length(unique(dates_df$primary))) 
  (n_secondary <- max(dates_df$secondary))
  n_primary * n_secondary
  length(unique(dates_df$period)) #yes bc I padded them
  
  #remove 1 totally empty season (also remove dry_2018? only have one month)
  dates_df <- dates_df %>% filter(!primary %in% c('cool_2018'))
  
  #save date/period key
  write.csv(dates_df, 'data/cleaned/period_3-18_date_key.csv')
  
  
## Convert to wide -------------------------------------------------------------
  dh_wide <- list()
  dh_wide_before <- list()
  dh_wide_after <- list()
  dh_stacked <- list()
  dh_stacked_before <- list()
  dh_stacked_after <- list()
  
  #loop thru each species to match dates with periods and convert to wide
  for (sp in names(dh_long)){
    
    #get long-format det hist for this species
    dh_sp <- dh_long[[sp]]
    
    #format date
    dh_sp$date <- as.POSIXct(strptime(dh_sp$date, '%Y-%m-%d'), tz = 'Africa/Blantyre')
    
    #merge date/period key 
    dh_sp <- left_join(dh_sp, dates_df, by = 'date')
    
    ## WIDE:
      #aggregate by period
      dh_wide_sp <- reshape2::dcast(dh_sp, site ~ factor(period, levels = unique(dh_sp$period)),
                                    value.var = 'n_photos_cleaned', 
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      #remove 'dry_2018' (only has 5 weeks) and pad out 'dry_2023' (has 13 weeks)
      dh_wide_sp <- dh_wide_sp %>% select(-"dry_2018-14", -"dry_2018-15", -"dry_2018-16", -"dry_2018-17", -"dry_2018-18")
      dh_wide_sp <- dh_wide_sp %>% mutate('dry_2023-14' = NA, 'dry_2023-15' = NA, 'dry_2023-16' = NA, 
                                          'dry_2023-17' = NA, 'dry_2023-18' = NA)
      
      #further split into BEFORE and AFTER fence decomissioning (WHICH SEASON TO USE?)
      cutoff <- 'wet_2020-18'
      cutoff_index <- which(names(dh_wide_sp) == cutoff)
    
      dh_wide_sp_before <- dh_wide_sp[, 1:cutoff_index] #columns through cutoff season (inclusive)
      dh_wide_sp_after <- dh_wide_sp[, c(1, (cutoff_index + 1):ncol(dh_wide_sp))] #columns after
      
    
    ## STACKED:
      #stack by site-primary
      dh_sp$site_primary <- paste(dh_sp$site, dh_sp$primary, sep = '_')
      dh_sp$secondary <- sprintf("%02d", dh_sp$secondary)
      
      dh_stacked_sp <- dh_sp %>% select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
        pivot_wider(names_from = secondary, values_from = n_photos_cleaned, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      #further split into BEFORE and AFTER fence decomissioning
      cutoff_date <- max(dates_df[dates_df$period == cutoff,]$date)
      dh_sp$before_after <- ifelse(dh_sp$date <= cutoff_date, 'before', 'after')
      
      dh_stacked_sp_before <- dh_sp %>% filter(before_after == 'before') %>%
        select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
        pivot_wider(names_from = secondary, values_from = n_photos_cleaned, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
      
      dh_stacked_sp_after <- dh_sp %>% filter(before_after == 'after') %>%
        select(site_primary, fence, class, n_photos_cleaned, secondary) %>%
        pivot_wider(names_from = secondary, values_from = n_photos_cleaned, names_sort = TRUE,
                    values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    
    #save
    dh_wide[[sp]] <- dh_wide_sp
    dh_wide_before[[sp]] <- dh_wide_sp_before
    dh_wide_after[[sp]] <- dh_wide_sp_after
    write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_3-18_periods_cam/', 'dh_wide_cam_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp_before, paste('data/cleaned/detection_histories_3-18_periods_cam/', 'dh_wide_cam_before_', sp, '.csv', sep = ''))
    write.csv(dh_wide_sp_after, paste('data/cleaned/detection_histories_3-18_periods_cam/', 'dh_wide_cam_after_', sp, '.csv', sep = ''))
    
    dh_stacked[[sp]] <- dh_stacked_sp
    dh_stacked_before[[sp]] <- dh_stacked_sp_before
    dh_stacked_after[[sp]] <- dh_stacked_sp_after
    
    write.csv(dh_stacked_sp, paste('data/cleaned/detection_histories_3-18_periods_cam_stacked/', 'dh_wide_cam_stacked_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_before, paste('data/cleaned/detection_histories_3-18_periods_cam_stacked/', 'dh_wide_cam_stacked_before_', sp, '.csv', sep = ''))
    write.csv(dh_stacked_sp_after, paste('data/cleaned/detection_histories_3-18_periods_cam_stacked/', 'dh_wide_cam_stacked_after_', sp, '.csv', sep = ''))
  }
  
  #save
  write_rds(dh_wide, 'data/cleaned/detection_histories_3-18_periods_cam/dh_wide_cam_all.RDS')  
  write_rds(dh_wide_before, 'data/cleaned/detection_histories_3-18_periods_cam/dh_wide_cam_all_before.RDS')  
  write_rds(dh_wide_after, 'data/cleaned/detection_histories_3-18_periods_cam/dh_wide_cam_all_after.RDS')  
  
  write_rds(dh_stacked, 'data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked.RDS')
  write_rds(dh_stacked_before, 'data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked_before.RDS')
  write_rds(dh_stacked_after, 'data/cleaned/detection_histories_3-18_periods_cam_stacked/dh_wide_cam_all_stacked_after.RDS')
  
  

    