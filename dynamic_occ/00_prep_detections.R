## Create separate det histories for wet/cool/dry season

library(data.table)
library(tidyverse)


## Read in long-format camera-level detection histories  -----------------------
dh_long <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')
  names(dh_long)
  head(dh_long[[1]])

  sort(unique(dh_long[[1]]$site)); length(unique(dh_long[[1]]$site))
  
  #Remove site P23-C1. Not sure why there was a second camera here but it was only deployed for 10 wks in 2019
  # and we don't have covariate data for it. Looks like mostly baboon, warthog, small antelope.
  dh_long <- lapply(dh_long, function(x) {x %>% filter(!grepl('P23-C1', site))})
  
  sort(unique(dh_long[[1]]$site)); length(unique(dh_long[[1]]$site))

  
## Establish sampling periods --------------------------------------------------

  #get start/end dates
  (beg <- min(dh_long[[1]]$date))
  (end <- max(dh_long[[1]]$date))
  (n_days <- interval(beg, end)/days(1))
  
  #set primary period start/end dates (seasons: wet=Dec-April, cool=May-Jul, dry=Aug-Nov)
  (n_primary <- 3) #3 per year
  (n_yrs <- year(end) - year(beg) + 1)
  dates_primary <- data.frame('season' = rep(c('cool','dry','wet'), length.out = n_yrs * n_primary),
                              'year' = rep(year(beg):year(end), each = n_primary),
                              'start' = rep(c('05-01','08-01','12-01'), length.out = n_yrs * n_primary))
  dates_primary$period <- paste(dates_primary$year, dates_primary$season, sep = '_')
  dates_primary$start <- as.POSIXct(strptime(paste(dates_primary$year, dates_primary$start, sep = '-'),
                                             format = '%Y-%m-%d'), tz = 'Africa/Blantyre')
  dates_primary <- dates_primary %>% arrange(start) %>% 
                                     mutate(end = lead(start) - days(1)) #subtract 1 to not include the next start date
  
  #list all days and match to a primary period
  dates <- format(as.Date(beg) + days(0:(n_days)), format = '%Y-%m-%d') 
  dates_df <- data.frame('date' = as.POSIXct(strptime(dates, '%Y-%m-%d'), tz = 'Africa/Blantyre'))
  
  #now match up primary periods with dates
  dates_df <- dates_df %>% rowwise() %>% 
    mutate(primary = dates_primary$period[which(date >= dates_primary$start & date <= dates_primary$end)]) %>% ungroup()
  
  #set secondary period (7 days)
  secondary <- 7
  dates_df <- dates_df %>% group_by(primary) %>% mutate(secondary = ceiling(row_number() / secondary)) %>% ungroup()
  
  #create a combined field
  dates_df$occasion <- paste(dates_df$primary, 
                           formatC(as.numeric(dates_df$secondary), width = 2, flag = "0"), sep = '-')
  
  #how many wks in each season?
  dates_df %>% group_by(primary) %>% summarise(max = max(secondary)) 
    #wet: 22 weeks
    #cool: 14 weeks
    #dry: 18 weeks
    #we'll get rid of 2018_dry later and pad out 2023_dry
  
  #save date/period key
  write.csv(dates_df, 'data/cleaned/season_date_key.csv')
  
  
## Create wide detection history -----------------------------------------------  
  
#create folder to save results
ifelse(!dir.exists(paste('data/cleaned/detection_histories_seasonal', sep = '/')), 
       dir.create(paste('data/cleaned/detection_histories_seasonal', sep = '/')), 'Folder exists already')

#loop thru each species to match dates with periods and convert to wide
dh_wide <- list()
for (sp in names(dh_long)){
    
  #get long-format det hist for this species
  dh_sp <- dh_long[[sp]]
  
  #format date
  dh_sp$date <- as.POSIXct(strptime(dh_sp$date, '%Y-%m-%d'), tz = 'Africa/Blantyre')
  
  #merge date/season key 
  dh_sp <- left_join(dh_sp, dates_df, by = 'date')
  
  #aggregate by season (preserve NAs if not surveyed any days per week)
  dh_wide_sp <- dh_sp %>% select(site, class, occasion, n_photos_cleaned) %>%
    arrange(site) %>%
    pivot_wider(names_from = occasion, values_from = n_photos_cleaned, names_sort = TRUE,
                values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
  
  #remove '2018_dry' (only has 5 weeks) and pad out '2023_dry' (has 13 weeks, needs 18)
  dh_wide_sp <- dh_wide_sp %>% 
    arrange(site) %>% 
    select(-contains('2018_dry')) %>%
    mutate('2023_dry-14' = NA, '2023_dry-15' = NA, '2023_dry-16' = NA, '2023_dry-17' = NA, '2023_dry-18' = NA) 
  
  #Create effort covariate (only need to do this once; will be the same for all species)
  if(sp == names(dh_long)[1]){
    
    #pivot wide with days surveyed per week
    dh_wide_effort <- dh_sp %>% select(site, occasion, surv) %>%
      arrange(site) %>%
      pivot_wider(names_from = occasion, values_from = surv, names_sort = TRUE,
                  values_fn = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    
    #remove '2018_dry' and pad '2023_dry'
    dh_wide_effort <- dh_wide_effort %>% 
      arrange(site) %>% 
      select(-contains('2018_dry')) %>% 
      mutate('2023_dry-14' = 0, '2023_dry-15' = 0, '2023_dry-16' = 0, '2023_dry-17' = 0, '2023_dry-18' = 0) 
  }
  
  #split into separate dataframes by season
  dh_wide_sp_wet <- dh_wide_sp %>% select(contains(c('site','class','wet')))
  dh_wide_sp_cool <- dh_wide_sp %>% select(contains(c('site','class','cool')))
  dh_wide_sp_dry <- dh_wide_sp %>% select(contains(c('site','class','dry')))
  
  #combine into list and save
  dh_sp_all <- list('wet' = dh_wide_sp_wet, 'cool' = dh_wide_sp_cool, 'dry' = dh_wide_sp_dry)
  dh_wide[[sp]] <- dh_sp_all

  #save CSVs for each species
  write.csv(dh_wide_sp, paste('data/cleaned/detection_histories_seasonal/', 'dh_wide_', sp, '.csv', sep = ''))
  write.csv(dh_wide_sp_wet, paste('data/cleaned/detection_histories_seasonal/', 'dh_wide_', sp, '_wet.csv', sep = ''))
  write.csv(dh_wide_sp_cool, paste('data/cleaned/detection_histories_seasonal/', 'dh_wide_', sp, '_cool.csv', sep = ''))
  write.csv(dh_wide_sp_dry, paste('data/cleaned/detection_histories_seasonal/', 'dh_wide_', sp, '_dry.csv', sep = ''))
  
  cat('finished:', sp, '\n')
}
  
  #save as R objects
  write_rds(dh_wide, 'data/cleaned/detection_histories_seasonal/dh_seasonal_all.RDS')  

  #save effort covariate
  write.csv(dh_wide_effort, 'data/cleaned/covariates/effort/effort_seasonal.csv')
  write.csv(dh_wide_effort %>% select(contains(c('site','wet'))), 'data/cleaned/covariates/effort/effort_seasonal_wet.csv')
  write.csv(dh_wide_effort %>% select(contains(c('site','cool'))), 'data/cleaned/covariates/effort/effort_seasonal_cool.csv')
  write.csv(dh_wide_effort %>% select(contains(c('site','dry'))), 'data/cleaned/covariates/effort/effort_seasonal_dry.csv')
  
  
## Plot effort -----------------------------------------------------------------  

  #read back in if necessary
  dh_wide_effort <- fread('data/cleaned/covariates/effort/effort_seasonal.csv')
  
  #pivot to long
  effort_long <- dh_wide_effort %>% pivot_longer(cols = -site, names_to = 'season', values_to = 'surveyed')
  
  #add north/south
  site_covar <- fread('data/cleaned/covariates/cleaned_covariates.csv')
  effort_long$fence <- site_covar$fence[match(effort_long$site, site_covar$Site)]
  effort_long$fence <- ifelse(effort_long$fence == 0, 'North', 'South')
  
  #aggregate by hex
  effort_long_hex <- effort_long %>% separate(site, into = c('hex','site')) %>%
                                     group_by(hex, season, fence) %>%
                                     summarise(surv = max(surveyed))
  
  #aggregate by season
  effort_long_season <- effort_long %>% separate(season, into = c('year_season','week'), sep = '-') %>%
                                        group_by(site, year_season, fence) %>%
                                        summarise(surv = max(surveyed))
  
  #aggregate by hex and season
  effort_long_hex_season <- effort_long %>% separate(site, into = c('hex','site'), sep = '-') %>%
                                            separate(season, into = c('year_season','week'), sep = '-') %>%
                                            group_by(hex, year_season, fence) %>%
                                            summarise(surv = max(surveyed))
  
  #axis indices for season cutoffs
  cutoffs <- c(22,36,54,76,90,108,130,144,162,184,198,216,238,252) #wet = 22 wks, cool = 14 wks, dry = 18 wks
  
  #plot
  effort_plot_site_wk <- ggplot(effort_long, aes(x = season, y = site, fill = as.logical(surveyed))) +
    geom_raster(hjust = 0) +
    scale_fill_manual(values = c('lightgray','darkorange')) +
    geom_vline(xintercept = cutoffs, linetype = 'dashed') +
    facet_grid(rows = vars(fence), drop = TRUE, scales = 'free') +
    xlab('week') +
    theme_bw(base_size = 12) 
  effort_plot_site_wk
  
  effort_plot_site_season <- ggplot(effort_long_season, aes(x = year_season, y = site, fill = as.logical(surv))) +
    geom_raster(hjust = 0) +
    scale_fill_manual(values = c('lightgray','darkorange')) +
    geom_vline(xintercept = rep(1:15), linetype = 'dashed') +
    facet_grid(rows = vars(fence), drop = TRUE, scales = 'free') +
    xlab('season') +
    theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1.5, vjust = 1.4))
  effort_plot_site_season
  
  effort_plot_hex_wk <- ggplot(effort_long_hex, aes(x = season, y = hex, fill = as.logical(surv))) +
    geom_raster(hjust = 0) +
    scale_fill_manual(values = c('lightgray','darkorange')) +
    geom_vline(xintercept = cutoffs, linetype = 'dashed') +
    facet_grid(rows = vars(fence), drop = TRUE, scales = 'free') +
    xlab('week') +
    theme_bw(base_size = 12) 
  effort_plot_hex_wk
  
  effort_plot_hex_season <- ggplot(effort_long_hex_season, aes(x = year_season, y = hex, fill = as.logical(surv))) +
    geom_raster(hjust = 0) +
    scale_fill_manual(values = c('lightgray','darkorange')) +
    geom_vline(xintercept = rep(1:15), linetype = 'dashed') +
    facet_grid(rows = vars(fence), drop = TRUE, scales = 'free') +
    xlab('season') +
    theme_bw(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1.5, vjust = 1.4))
  effort_plot_hex_season
  
  ggsave('dynamic_occ/figures/effort_site_week.png', effort_plot_site_wk, dpi = 300, width = 6, height = 3)
  ggsave('dynamic_occ/figures/effort_site_season.png', effort_plot_site_season, dpi = 300, width = 6, height = 3)
  ggsave('dynamic_occ/figures/effort_hex_wk.png', effort_plot_hex_wk, dpi = 300, width = 6, height = 3)
  ggsave('dynamic_occ/figures/effort_hex_season.png', effort_plot_hex_season, dpi = 300, width = 6, height = 3)
  
  
  
  