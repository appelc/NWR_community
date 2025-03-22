# Explore fire detections from CLIP
## takes CSVs with predictions, parses them, and outputs a CSV with sites/dates to review

library(data.table)
library(tidyverse)


# NWR 2021 ---------------------------------------------------------------------
dir <- 'clip_fire/preds_2021/'
file_names <- list.files(dir, full.names = TRUE); length(file_names)

  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL)); length(file_list)
    #1 empty

  #combine
  preds <- rbindlist(file_list, fill = TRUE)

  #parse
  preds$image_path <- gsub('/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/','',preds$image_path)
  preds <- preds %>% separate(image_path, c('year','hex','site_check','filename'), remove = FALSE, sep = '/') 
  preds$date <- str_extract(preds$filename, "(?<=__)[0-9]{4}-[0-9]{2}-[0-9]{2}")
  preds$time <- str_extract(preds$filename, "[0-9]{2}-[0-9]{2}-[0-9]{2}(?=\\()")
  preds$datetime <- as.POSIXct(strptime(paste(preds$date, preds$time, sep = ' '), '%Y-%m-%d %H-%M-%S'), tz = 'Africa/Blantyre')

  #check
  sort(unique(preds$site_check))
  
  #create dataframe to use for review
  review <- preds %>% 
    filter(score >= 0.25) %>% 
    group_by(site_check) %>% 
    reframe(date = unique(date), fire = '') %>%
    arrange(site_check, date)

  #save
  write.csv(review, 'clip_fire/review_fire_nwr2021.csv')
    #save a copy of this as '_working' with my fire review 0/1
  
  
# NWR 2022 ---------------------------------------------------------------------
  dir <- 'clip_fire/preds_2022/'
  file_names <- list.files(dir, full.names = TRUE); length(file_names)
  
  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL)); length(file_list)
  
  #combine
  preds <- rbindlist(file_list, fill = TRUE)
  
  #parse
  preds$image_path <- gsub('/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/','',preds$image_path)
  preds <- preds %>% separate(image_path, c('year','hex','site_check','filename'), remove = FALSE, sep = '/') 
  preds$date <- str_extract(preds$filename, "(?<=__)[0-9]{4}-[0-9]{2}-[0-9]{2}")
  preds$time <- str_extract(preds$filename, "[0-9]{2}-[0-9]{2}-[0-9]{2}(?=\\()")
  preds$datetime <- as.POSIXct(strptime(paste(preds$date, preds$time, sep = ' '), '%Y-%m-%d %H-%M-%S'), tz = 'Africa/Blantyre')
  
  #check
  sort(unique(preds$site_check))
  
  #create dataframe to use for review
  review <- preds %>% 
    filter(score >= 0.25) %>% 
    group_by(site_check) %>% 
    reframe(date = unique(date), fire = '') %>%
    arrange(site_check, date)
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2022.csv')
    #save a copy of this as '_working' with my fire review 0/1
  
  
# NWR 2023 ---------------------------------------------------------------------
  dir <- 'clip_fire/preds_2023/'
  file_names <- list.files(dir, full.names = TRUE); length(file_names)
  
  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL)); length(file_list)
  
  #combine
  preds <- rbindlist(file_list, fill = TRUE)
  
  #parse
  preds$image_path <- gsub('/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/','',preds$image_path)
  preds <- preds %>% separate(image_path, c('year','hex','site_check','filename'), remove = FALSE, sep = '/') 
  preds$date <- str_extract(preds$filename, "(?<=__)[0-9]{4}-[0-9]{2}-[0-9]{2}")
  preds$time <- str_extract(preds$filename, "[0-9]{2}-[0-9]{2}-[0-9]{2}(?=\\()")
  preds$datetime <- as.POSIXct(strptime(paste(preds$date, preds$time, sep = ' '), '%Y-%m-%d %H-%M-%S'), tz = 'Africa/Blantyre')
  
  #check
  sort(unique(preds$site_check))
  
  #create dataframe to use for review
  review <- preds %>% 
    filter(score >= 0.25) %>% 
    group_by(site_check) %>% 
    reframe(date = unique(date), fire = '') %>%
    arrange(site_check, date)
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2023.csv')
  write.csv(review, 'clip_fire/review_fire_nwr2023_working.csv')
  