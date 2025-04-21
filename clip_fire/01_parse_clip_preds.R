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
  
  ## TO REPORT:
  summary(preds$score)
  hist(preds$score)
  length(unique(preds$image_path)) #699,952 images
  length(unique(preds[preds$score >= 0.25,]$image_path)) #57,694 images with scores >= 0.25
  table(review$site_check) #number of days with *predicted* fire per site-check
  tmp <- data.frame(table(review$site_check)); summary(tmp$Freq); sd(tmp$Freq) #mean 13 days per camera (SD 15), range 1-62 
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2021.csv')
    #save a copy of this as '_working' with my fire review 0/1
  
  #store to combine later
  preds21 <- preds
  review21 <- review
  
  
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
  
  ## TO REPORT:
  summary(preds$score)
  hist(preds$score)
  length(unique(preds$image_path)) #1,279,373 images
  length(unique(preds[preds$score >= 0.25,]$image_path)) #34,508 images with scores >= 0.25
  table(review$site_check) #number of days with *predicted* fire per site-check
  tmp <- data.frame(table(review$site_check)); summary(tmp$Freq); sd(tmp$Freq) #mean 10 days per camera (SD 12), range 1-62 
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2022.csv')
    #save a copy of this as '_working' with my fire review 0/1
  
  #store to combine later
  preds22 <- preds
  review22 <- review
  
  
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
  
  ## TO REPORT:
  summary(preds$score)
  hist(preds$score)
  length(unique(preds$image_path)) #1,553,782 images
  length(unique(preds[preds$score >= 0.25,]$image_path)) #42,573 images with scores >= 0.25
  table(review$site_check) #number of days with *predicted* fire per site-check
  tmp <- data.frame(table(review$site_check)); summary(tmp$Freq); sd(tmp$Freq) #mean 8 days per camera (SD 9), range 1-44 
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2023.csv')
  # write.csv(review, 'clip_fire/review_fire_nwr2023_working.csv')
  
  #store to combine later
  preds23 <- preds
  review23 <- review
  
  
# NWR 2019 ---------------------------------------------------------------------
  dir <- 'clip_fire/preds_2019/'
  file_names <- list.files(dir, full.names = TRUE); length(file_names)
  
  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL)); length(file_list)
  
  #combine
  preds <- rbindlist(file_list, fill = TRUE)
  
  #parse -- these ones aren't renamed and are less organized
  preds$image_path <- gsub('/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/','',preds$image_path)
  preds$filename <- basename(preds$image_path)
  preds$hex <- sapply(strsplit(preds$image_path, '/'), '[', 2)
  preds$site <- sapply(strsplit(preds$image_path, '/'), '[', 3) #won't work for all, but oh well
  
  #check
  sort(unique(preds$site))
  
  #create dataframe to use for review
  review <- preds %>% 
    filter(score >= 0.25) 
    # %>% 
    # group_by(site) %>% 
    # reframe(date = unique(date), fire = '') %>%
    # arrange(site_check, date)
  
  ## TO REPORT:
  summary(preds$score)
  hist(preds$score)
  length(unique(preds$image_path)) #32,652 images
  length(unique(preds[preds$score >= 0.25,]$image_path)) #44 images with scores >= 0.25
  table(review$site, useNA = 'a') #number of days with *predicted* fire per site-check
  tmp <- data.frame(table(review$site)); summary(tmp$Freq); sd(tmp$Freq) #mean 6 days per camera (SD 9), range 1-24 
  
  #save
  write.csv(review, 'clip_fire/review_fire_nwr2019.csv')
  # write.csv(review, 'clip_fire/review_fire_nwr2019_working.csv')
  
  #store to combine later
  preds19 <- preds
  review19 <- review
  
  
# NWR 2020 ---------------------------------------------------------------------
  dir <- 'clip_fire/preds_2020/'
  file_names <- list.files(dir, full.names = TRUE); length(file_names)

  #read in all CSVs (error checking for empty CSVs)
  file_list <- lapply(file_names, function(f) tryCatch(fread(f), error = function(e) NULL)); length(file_list)

  #combine
  preds <- rbindlist(file_list, fill = TRUE)

  #parse -- not renamed
  preds$image_path <- gsub('/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/', '', preds$image_path)
  preds$filename <- basename(preds$image_path)
  preds$hex <- sapply(strsplit(preds$image_path, '/'), '[', 2)
  preds$site_check <- sapply(strsplit(preds$image_path, '/'), '[', 3) 
  
  #check
  sort(unique(preds$site_check))

  #create dataframe to use for review -- can't filter by date here either
  review <- preds %>%
    filter(score >= 0.25)
  # %>%
  # group_by(site) %>%
  # reframe(date = unique(date), fire = '') %>%
  # arrange(site_check, date)

  ## TO REPORT:
  summary(preds$score)
  hist(preds$score)
  length(unique(preds$image_path)) #1,831,919 images
  length(unique(preds[preds$score >= 0.25,]$image_path)) #28840 images with scores >= 0.25
  table(review$site, useNA = 'a') #number of days with *predicted* fire per site-check
  tmp <- data.frame(table(review$site)); summary(tmp$Freq); sd(tmp$Freq) #mean 444 days per camera (SD 1034), range 1-5643

  #save
  write.csv(review, 'clip_fire/review_fire_nwr2020.csv')
  write.csv(review, 'clip_fire/review_fire_nwr2020_working.csv')

  #store to combine later
  preds20 <- preds
  review20 <- review

  
## COMBINE AND SUMMARIZE -------------------------------------------------------
  
  #edit 2019
  review19$site_check <- review19$site
  
  all_preds <- rbind(preds19, preds20, preds21, preds22, preds23, fill = TRUE)
  all_review <- rbind(review19, review20, review21, review22, review23, fill = TRUE)
  
  length(unique(all_preds$image_path)) #5,397,678 images
  nrow(all_preds) #same
  
  nrow(all_preds[all_preds$score >= 0.25,]) #163,659 images with scores > 0.25
  
  tmp <- data.frame(table(all_review$site_check)); summary(tmp$Freq); sd(tmp$Freq) #mean 10 days per camera (SD 12), range 1-62
  
  ## More summaries in the next step (02_parse_fire_reviewed.R)
  
  