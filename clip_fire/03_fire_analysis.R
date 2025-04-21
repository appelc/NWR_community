## Fire analysis -- detections pre/post fire, time to first detection

library(data.table)
library(tidyverse)


## Read in fire data  ----------------------------------------------------------

fire_df <- fread('clip_fire/fire_detections.csv') 

#list sites with fire
sites_with_fire <- unique(fire_df$site)

  #some QC:

  #calculate time elapsed between fires at each site
  fire_df <- fire_df %>% arrange(site, date) %>% 
    group_by(site) %>%
    mutate(date_diff = difftime(date, lag(date), units = 'days')) %>% 
    select(-c(V1)) %>%
    ungroup()
  
  #inspect
  fire_df[order(as.numeric(fire_df$date_diff)),]
    #K15-A (2019-11-14) did actually re-burn 3 days later
    #T24-B (2022-11-05) did actually re-burn 3 days later
    #O06-B (2019-11-07) burned 42 days after a previous fire (Sept and Nov 2019)
    #D10-C (2020-11-04) burned 49 days after a previous fire (Sept and Nov 2020, then also Oct 2022)
  
    #remove the second fire for K15-A? yeah, call the first one the start of the fire event
    fire_df <- fire_df[!(fire_df$site == 'K15-A' & !is.na(fire_df$date_diff) & as.numeric(fire_df$date_diff) == 3),]
  
    #same for T24-B
    fire_df <- fire_df[!(fire_df$site == 'T24-B' & !is.na(fire_df$date_diff) & as.numeric(fire_df$date_diff) == 3),]
    
    
## Read in detection data ------------------------------------------------------

dh_long <- readRDS('data/raw/detection_histories/dh_long/detection_histories_long_all_2018-2023.RDS')

focal_species <- c('buffalo','bushbuck','bushpig','eland','elephant','impala',
                   'kudu','reedbuck','roan','sable','warthog','waterbuck','zebra')

  #filter to focal species and convert from list to dataframe
  dh_focal <- dh_long[focal_species]
  dh_focal_df <- rbindlist(dh_focal)

  #filter to sites with fire
  dh_with_fire <- dh_focal_df %>% filter(site %in% sites_with_fire)
  
  
## For each fire date, list the 30 days before/after ---------------------------
  
window <- 60
fire_dates <- NULL

for (rr in 1:nrow(fire_df)){
  
  date_rr <- fire_df[rr,]$date
  site_rr <- fire_df[rr,]$site
  
  beg <- as.Date(date_rr) - window
  diff_days <- window * 2
  range <- format(beg + 0:diff_days, format = '%Y-%m-%d')
 
  fire_dates <- rbind(fire_dates, data.frame('site' = site_rr,
                                             'fire_date' = date_rr,
                                             'date' = as.Date(range)))
}
head(fire_dates)


## Now grab these dates from each det hist -------------------------------------

head(dh_focal_df)
head(fire_dates)

#make col for matching
dh_focal_df$site_date <- paste(dh_focal_df$site, dh_focal_df$date, sep = '_')
fire_dates$site_date <- paste(fire_dates$site, fire_dates$date, sep = '_')

#filter
dh_fires <- dh_focal_df %>% 
            left_join(fire_dates[,c('site_date','fire_date')], by = 'site_date', relationship = 'many-to-many') %>% #bc there will be one for each species
            filter(site_date %in% fire_dates$site_date) %>%
            mutate(site_fire = paste(site, fire_date, sep = '_')) %>%
            mutate(diff_days = difftime(date, fire_date, units = 'days'))
  
  head(dh_fires[,c('site','date','class','n_photos_cleaned','site_fire','diff_days')])


  #which fires had any animal detected within the before/after window?
  fires_with_animals <- dh_fires %>% group_by(site_fire) %>% 
                                     summarise(n_photos = sum(n_photos_cleaned, na.rm = TRUE)) %>%
                                     filter(n_photos > 0) %>% pull(site_fire)


## Plot by fire ----------------------------------------------------------------

  #make species colors
  custom_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                   "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
                   "#FC8D62", "#8DA0CB", "#E78AC3")
  species_colors <- setNames(custom_colors, focal_species)

  #plot
  fire_plots <- list()
  fire_plots_log <- list()
  
  #only loop thru fires that animals detected within the window
  for (ff in fires_with_animals){
    
    fire_subset <- dh_fires %>% filter(site_fire == ff)
    
    #use 'diff_days' for -30 through 30; use 'date' for calendar days       
    plot_ff <- ggplot(fire_subset, aes(x = diff_days, y = as.integer(n_photos_cleaned))) +
      geom_point(aes(color = ifelse(n_photos_cleaned == 0, NA, class)), size = 3,
                 position = position_jitter(width = 0.5)) + 
      scale_color_manual(values = species_colors, na.value = NA) +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      annotate('text', x = 0, y = 0, label = unique(fire_subset$fire_date), vjust = -0.5, color = 'black', fontface = 'bold', size = 5) +
      #use rug to note which days have been surveyed (better way to do this?)
      geom_rug(data = fire_subset[fire_subset$surv == 1,], aes(x = diff_days), inherit.aes = FALSE, linewidth = 3, color = 'gray') +
      ylab('Photos per day') +
      # ggtitle(ff) +
      ggtitle(paste('Site:', unique(fire_subset$site))) +
      theme_bw() + theme(legend.title = element_blank(),
                         axis.text = element_text(size = 14),
                         axis.title = element_text(size = 16),
                         legend.text = element_text(size = 14),
                         title = element_text(size = 18))
    fire_plots[[ff]] <- plot_ff
    
    plot_ff_log <- ggplot(fire_subset, aes(x = diff_days, y = log(n_photos_cleaned))) +
      geom_point(aes(color = ifelse(n_photos_cleaned == 0, NA, class)), size = 3) + #jitter doesn't work with log; why?
      scale_color_manual(values = species_colors, na.value = NA) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      annotate('text', x = 0, y = 0, label = unique(fire_subset$fire_date), vjust = -0.5, color = 'black', fontface = 'bold', size = 5) +
      #use rug to note which days have been surveyed (better way to do this?)
      geom_rug(data = fire_subset[fire_subset$surv == 1,], aes(x = diff_days), inherit.aes = FALSE, linewidth = 3, color = 'gray') +
      ylab('log(Photos per day)') +
      ylim(c(0,6.1)) +
      # ggtitle(ff) +
      ggtitle(paste('Site:', unique(fire_subset$site))) +
      theme_bw() + theme(legend.title = element_blank(),
                         axis.text = element_text(size = 14),
                         axis.title = element_text(size = 16),
                         legend.text = element_text(size = 14),
                         title = element_text(size = 18))
    fire_plots_log[[ff]] <- plot_ff_log
    
    #save
    ggsave(paste0('clip_fire/figures/by_fire_60/raw/', ff, '.png'), plot_ff, width = 6, height = 5, dpi = 300)
    ggsave(paste0('clip_fire/figures/by_fire_60/log/', ff, '_log.png'), plot_ff_log, width = 6, height = 5, dpi = 300)
  }
  
  
## Or plot by species ----------------------------------------------------------
  
#loop thru all
  
  species_plots <- list()
  species_plots_log <- list()
  
  for (pp in unique(dh_fires$class)){
    
    species_subset <- dh_fires %>% filter(class == pp) # n_photos_cleaned > 0
    
    plot_pp <- ggplot(species_subset, aes(x = diff_days, y = as.integer(n_photos_cleaned))) +
      geom_point(aes(color = ifelse(n_photos_cleaned > 0, site_fire, NA)), size = 3,
                 position = position_jitter(width = 0.5)) + 
      scale_color_discrete(na.value = NA) +
      # scale_color_manual(values = species_colors, na.value = NA) + 
      geom_vline(xintercept = 0, linetype = 'dashed') +
      ylab('Photos per day') +
      # ylim(c(0,6.1)) +
      ggtitle(paste(unique(species_subset$class))) +
      theme_bw() + theme(legend.position = 'none',
                         legend.title = element_blank(),
                         axis.text = element_text(size = 14),
                         axis.title = element_text(size = 16),
                         legend.text = element_text(size = 14),
                         title = element_text(size = 18))
    species_plots[[pp]] <- plot_pp
    
    plot_pp_log <- ggplot(species_subset, aes(x = diff_days, y = log(n_photos_cleaned))) +
      geom_point(aes(color = ifelse(n_photos_cleaned > 0, site_fire, NA)), size = 3,
                 position = position_jitter(width = 0.5)) + 
      scale_color_discrete(na.value = NA) + #if color = site_fire
      # scale_color_manual(values = species_colors, na.value = NA) +  #if color = class
      geom_vline(xintercept = 0, linetype = 'dashed') +
      ylab('log(Photos per day)') +
      ylim(c(0,6.1)) +
      ggtitle(paste(unique(species_subset$class))) +
      theme_bw() + theme(legend.position = 'none',
                         legend.title = element_blank(),
                         axis.text = element_text(size = 14),
                         axis.title = element_text(size = 16),
                         legend.text = element_text(size = 14),
                         title = element_text(size = 18))
    species_plots_log[[pp]] <- plot_pp_log
    
    #save
    ggsave(paste0('clip_fire/figures/by_species_60/raw/', pp, '.png'), plot_pp, width = 6, height = 5, dpi = 300)
    ggsave(paste0('clip_fire/figures/by_species_60/log/', pp, '_log.png'), plot_pp_log, width = 6, height = 5, dpi = 300)
  }

  
## Calculate time to first detection -------------------------------------------
  
  head(dh_fires)

  #get first det after each fire
  first_det_fire <- dh_fires %>% group_by(class, site_fire) %>% 
                                 filter(n_photos_cleaned > 0) %>%     #only ones w/ detections
                                 summarise(first_det = min(date)) %>% #get first date
                                 separate(site_fire, into = c('site','fire_date'), sep = '_', remove = FALSE) %>%
                                 mutate(time_diff = difftime(date(first_det), date(fire_date), units = 'days')) %>%
                                 filter(time_diff > 0)
    
  #summarize among all fires
  first_det_sum <- first_det_fire %>% group_by(class) %>% 
                                      # filter(time_diff > 0) %>% #only keep where first detection was after fire (WHAT ABOUT ZEROES???)
                                      summarize(mean = mean(time_diff),
                                                median = median(time_diff),
                                                min = min(time_diff),
                                                max = max(time_diff),
                                                sd = sd(time_diff))
  first_det_sum  

  #plot
  plot_first_det <- ggplot(first_det_sum, aes(x = reorder(class, -median), y = median)) + #sort by ascending mean, need to use first_det_sum?
                    geom_point(alpha = 0) +
                    geom_boxplot(data = first_det_fire, aes(x = class, y = time_diff)) +
                    geom_point(data = first_det_fire, aes(x = class, y = time_diff), alpha = 0.3) +
                    # geom_point(color = 'darkorange', size = 3, ) +
                    ylab('Days to first detection') +
                    theme_bw() + theme(axis.text = element_text(size = 14),
                                       axis.title.x = element_blank(),
                                       axis.title.y = element_text(size = 14),
                                       axis.text.x = element_text(angle = 45, vjust = 0.7))
  plot_first_det  

  ggsave('clip_fire/figures/time_to_det_60.png', plot_first_det, width = 8, height = 5, dpi = 300)  
  
  ## This really needs to be adjusted for commonality, though!
  ## e.g., roan are very rarely detected, so between-detection interval will be large, right?
  ## could compare with time to first detection from some random date instead?
  
  
## Model time to first detection -----------------------------------------------
  
  head(first_det_sum)

  
  
## Model detection rate before/after -------------------------------------------
  
  head(dh_fires)
  
  #(number after - number before) ~ class
  
  #number after ~ number before * class
  
  