# Now take reviewed fire detections and convert to covariate and/or detection history

library(data.table)
library(tidyverse)


# Read in validated detections
dir <- 'clip_fire/'
file_names <- list.files(dir, pattern = '*working.csv', full.names = TRUE)

  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL))
  
  #combine
  reviewed <- rbindlist(file_list, fill = TRUE)
  
  
# Summarize and format
fires <- reviewed %>% filter(fire == 1) %>% group_by(site_check) %>%
  separate(site_check, c('hex','cam','check_yr','check_month','check_day'), remove = FALSE)

fires$date <- as.POSIXct(strptime(fires$date, '%m/%d/%y'), tz = 'Africa/Blantyre')
