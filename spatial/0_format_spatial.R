# Summarize rasters and extract covariate values at NWR camera stations

library(terra)
library(sf)
library(ggplot2)


## Set CRS ---------------------------------------------------------------------

  nwr_crs <- 'EPSG:32736' #WGS84, UTM zone 36S
  

## Load camera coords ----------------------------------------------------------

  #read CSV
  camera_csv <- read.csv('data/raw/covariates/MonitoringPlots_corrected_031825.csv')
    length(unique(camera_csv$HexIDcorre)) #70 hex
    length(unique(camera_csv$Site)) #210 camera stations
    
  #convert to sf object with correct CRS
  camera_sf <- st_as_sf(camera_csv, coords = c('UTMXcorrec','UTMYcorrec'), crs = nwr_crs)
  
  #convert to vect object
  camera_coords <- vect(camera_sf)
  
  #QC plot
  ggplot() +
    geom_sf(data = camera_sf, color = 'blue', size = 2) +
    coord_sf(datum = nwr_crs)
  
 
## Load covariate rasters ------------------------------------------------------

  #set file paths
  raster_files <- c('elev' = 'data/raw/covariates/Nkhotakota Veg Data/TIFFS/elevation.tif',
                    'pct_slope' = 'data/raw/covariates/Nkhotakota Veg Data/TIFFS/pct_slope.tif',
                    'tree_vol' = 'data/raw/covariates/Nkhotakota Veg Data/TIFFS/nwr_tree_vol.tif')
  
  #handle land cover separately
  lc_file <- c('land_cov' = 'data/raw/covariates/Nkhotakota Veg Data/TIFFS/NWR_LC_v2.tif')
  
  #create function to read rasters, check CRS, and re-project if needed
  load_and_check_raster <- function(file_path, name) {
    cat('Loading raster:', name, '\n')
    r <- rast(file_path)
    if (crs(r) != nwr_crs) {
      cat(' Reprojecting to', nwr_crs, '\n')
      r <- project(r, nwr_crs)
    }
    return(r)
  }
  
  #read and check (continuous rasters)
  raster_covar <- lapply(seq_along(raster_files), function(i) {
    load_and_check_raster(raster_files[i], names(raster_files)[i])
  }); names(raster_covar) <- names(raster_files)
  
  #read and check (land cover)
  lc_raster <- load_and_check_raster(lc_file, names(lc_file))
  names(lc_raster) <- names(lc_file)
  
  #QC plots
  plot(raster_covar$elev); points(camera_coords, col = 'white')
  plot(raster_covar$pct_slope); points(camera_coords, col = 'white')
  plot(raster_covar$tree_vol); points(camera_coords, col = 'white')
  plot(lc_raster); points(camera_coords, col = 'white')
  
  #view summary statistics
  summary(values(raster_covar$elev))
  summary(values(raster_covar$pct_slope))
  summary(values(raster_covar$tree_vol))
  summary(values(lc_raster))
  
  #view resolution
  terra::res(raster_covar$elev) #30x30m for elev
  terra::res(raster_covar$pct_slope) #10x10m for slope
  terra::res(raster_covar$tree_vol) #20x20 for tree vol
  terra::res(lc_raster) #20x20 for land cover
  
  
## Reclassify land cover to create 'cleared areas' class -----------------------
  
  freq(lc_raster)  
  
  #change all 51 (agriculture) to 50 (clearings) 
  lc_raster_rc <- lc_raster
  lc_raster_rc[lc_raster_rc == 51] <- 50 #50 will now be 'cleared'

  freq(lc_raster_rc)  #compare with above
  
    
## Specify extent and clip? ----------------------------------------------------
  
  
  
## Summarize using moving windows (continuous var) -----------------------------
  
  #make empty lists for storing results
  resampled_covar_3x3 <- list()
  resampled_covar_5x5 <- list()
  
  #loop thru rasters
  for (rr in names(raster_covar)){
    
    #get raster
    rast = raster_covar[[rr]]
    
    #calculate moving window values
    mean_3x3 <- focal(rast, w = 3, fun = 'mean', na.rm = FALSE) #figure out how to handle NAs; see examples in help file
    # mean_3x3 <- focal(rast, w = matrix(1/9, nrow = 3, ncol = 3)) #apparently equivalent to above
    # mean_3x3 <- focal(rast, w = 3, fun = 'mean', na.rm = TRUE) #but this one is not the same
    mean_5x5 <- focal(rast, w = 5, fun = 'mean', na.rm = FALSE)

    sd_3x3 <- focal(rast, w = 3, fun = 'sd')
    sd_5x5 <- focal(rast, w = 5, fun = 'sd')
    
    #save
    resampled_covar_3x3[[paste0(rr, '_mean', '_3x3')]] <- mean_3x3
    resampled_covar_3x3[[paste0(rr, '_sd', '_3x3')]] <- sd_3x3
    resampled_covar_5x5[[paste0(rr, '_mean', '_5x5')]] <- mean_5x5
    resampled_covar_5x5[[paste0(rr, '_sd', '_5x5')]] <- sd_5x5
  }
  
  str(resampled_covar_3x3)
  str(resampled_covar_5x5)
    
  
## Summarize using moving windows (land cover) ---------------------------------
  
  #make key of class names
  unique(lc_raster_rc)
  lc_key <- data.frame('land_cov' = unique(lc_raster_rc),
                       'class_name' = c('M1','M2','M3','Afromontane','Water',
                                        'Dambo','Rock','Cleared'))
  n_classes <- nrow(lc_key)

  #make empty lists for storing results
  lc_props_3x3 <- list()
  lc_props_5x5 <- list()
  
  #loop thru classes
  for (ii in 1:nrow(lc_key)){
    
    #create 0/1 raster
    lc_binary <- lc_raster_rc == lc_key$land_cov[ii]
    
    #calculate proportion of this class in window
    lc_props_3x3[[lc_key$class_name[ii]]] <- focal(lc_binary, w = 3, fun = 'mean', na.rm = FALSE) #or convert NA to 0 first
    lc_props_5x5[[lc_key$class_name[ii]]] <- focal(lc_binary, w = 5, fun = 'mean', na.rm = FALSE) #or convert NA to 0 first
    
    #fix column names
    names(lc_props_3x3[[lc_key$class_name[ii]]]) <- paste0('prop_', lc_key$class_name[ii], '_3x3') 
    names(lc_props_5x5[[lc_key$class_name[ii]]]) <- paste0('prop_', lc_key$class_name[ii], '_5x5') 
  }
  
  str(lc_props_3x3)
  str(lc_props_5x5)
  summary(lc_props_3x3$M1) #check that they are all between 0-1 and not all 0s
  summary(lc_props_3x3$M2) 
  summary(lc_props_3x3$M3) 
  summary(lc_props_3x3$Afromontane) 
  summary(lc_props_3x3$Dambo) 
  summary(lc_props_3x3$Water) 
  summary(lc_props_3x3$Rock) 
  summary(lc_props_3x3$Cleared) 
  
  
## Extract values at camera stations (continuous var) --------------------------

  #make empty lists for storing results
  extracted_3x3 <- list()
  extracted_5x5 <- list()
  
  #loop thru rasters (3x3)
  for (cc in names(resampled_covar_3x3)){ 
    
    #get raster
    rast_3x3 = resampled_covar_3x3[[cc]]

    #extract and fix col name
    extracted_vals_3x3 <- extract(rast_3x3, camera_coords, bind = TRUE)
    names(extracted_vals_3x3)[ncol(extracted_vals_3x3)] <- cc

    #save
    extracted_3x3[[cc]] <- extracted_vals_3x3
  }
  
  #loop thru rasters (5x5)
  for (dd in names(resampled_covar_5x5)){ 
    
    #get raster
    rast_5x5 = resampled_covar_5x5[[dd]]
    
    #extract and fix col name
    extracted_vals_5x5 <- extract(rast_5x5, camera_coords, bind = TRUE)
    names(extracted_vals_5x5)[ncol(extracted_vals_5x5)] <- dd
    
    #save
    extracted_5x5[[dd]] <- extracted_vals_5x5
  }
  
  names(extracted_3x3)
  names(extracted_5x5)
  names(extracted_3x3$elev_mean) #example
  names(extracted_5x5$elev_mean) #example
  
  #combine list items (3x3) -- keep just last column in each dataframe except the first
  values_3x3 <- extracted_3x3[[1]]
  if(length(extracted_3x3) > 1) {
    for(i in 2:length(extracted_3x3)) {
      cols_to_keep <- c("Site", names(extracted_3x3)[i])
      values_3x3 <- merge(values_3x3, extracted_3x3[[i]][, cols_to_keep], by="Site")
    }
  }
  
  #combine list items (5x5)
  values_5x5 <- extracted_5x5[[1]]
  if(length(extracted_5x5) > 1) {
    for(i in 2:length(extracted_5x5)) {
      cols_to_keep <- c("Site", names(extracted_5x5)[i])
      values_5x5 <- merge(values_5x5, extracted_5x5[[i]][, cols_to_keep], by="Site")
    }
  }
  
  head(values_3x3)
  head(values_5x5)
  
  
## Extract values at camera stations (land cover) ------------------------------
  
  #make empty lists for storing results
  lc_extracted_3x3 <- list()
  lc_extracted_5x5 <- list()
  
  #loop thru LC rasters
  for (ll in names(lc_props_3x3)){ #names are the same for 3x3 and 5x5
    
    #get raster
    lc_rast_3x3 = lc_props_3x3[[ll]]
    lc_rast_5x5 = lc_props_5x5[[ll]]
    
    #extract
    extracted_lc_3x3 <- extract(lc_rast_3x3, camera_coords, bind = TRUE)
    extracted_lc_5x5 <- extract(lc_rast_5x5, camera_coords, bind = TRUE)
    
    #names look OK now, no need to fix
    
    #save
    lc_extracted_3x3[[ll]] <- extracted_lc_3x3
    lc_extracted_5x5[[ll]] <- extracted_lc_5x5
  }
  
  names(lc_extracted_3x3)
  names(lc_extracted_5x5)
  names(lc_extracted_3x3$M2) #examples
  names(lc_extracted_5x5$M2)

  #combine list items (3x3)
  lc_values_3x3 <- lc_extracted_3x3[[1]]
  if(length(lc_extracted_3x3) > 1) {
    for(i in 2:length(lc_extracted_3x3)) {
      cols_to_keep <- c("Site", paste0('prop_', names(lc_extracted_3x3)[i], '_3x3'))
      lc_values_3x3 <- merge(lc_values_3x3, lc_extracted_3x3[[i]][, cols_to_keep], by="Site")
    }
  }
  
  #combine list items (5x5)
  lc_values_5x5 <- lc_extracted_5x5[[1]]
  if(length(lc_extracted_5x5) > 1) {
    for(i in 2:length(lc_extracted_5x5)) {
      cols_to_keep <- c("Site", paste0('prop_', names(lc_extracted_5x5)[i], '_5x5'))
      lc_values_5x5 <- merge(lc_values_5x5, lc_extracted_5x5[[i]][, cols_to_keep], by="Site")
    }
  }
  
  head(lc_values_3x3) 
  head(lc_values_5x5)
  
  #check for all 0s again:
  summary(lc_values_3x3$prop_M1_3x3)
  summary(lc_values_3x3$prop_M2_3x3)
  summary(lc_values_3x3$prop_M3_3x3)
  summary(lc_values_3x3$prop_Afromontane_3x3) #ok, none of our cameras were in Afromontane
  summary(lc_values_3x3$prop_Water_3x3) #ok, none of our cameras were in water
  summary(lc_values_3x3$prop_Dambo_3x3) 
  summary(lc_values_3x3$prop_Rock_3x3) #ok, none of our cameras were in water
  summary(lc_values_3x3$prop_Cleared_3x3)
  
  
## Calculate distance from dambo -----------------------------------------------
  
  #we're using the original raster now, not the 3x3 or 5x5 summaries
  
  #which LC class is 'dambo'?
  lc_key
  freq(lc_raster_rc) #11 is Dambo
    
  #reclassify 0/1
  dambo_raster <- lc_raster == 11
  freq(dambo_raster)
  
  #shrink (e.g., remove small areas). first set min patch size (pixels)
  min_size <- 2
  
  #calculate patch sizes
  dambo_patches_rast <- terra::patches(dambo_raster, zeroAsNA=TRUE) #is zeroAsNA=TRUE here correct?
  dambo_patch_sizes <- terra::zonal(dambo_raster, dambo_patches_rast, fun="sum")
    summary(dambo_patch_sizes[,2])
    head(dambo_patch_sizes)
  
  #keep patches > minimum size
  dambo_patches_keep <- dambo_patch_sizes[dambo_patch_sizes$land_cov >= min_size, 1]
  dambo_clean <- ifel(dambo_patches_rast %in% dambo_patches_keep, 1, NA)
  
  #compare with original
  plot(dambo_raster, col = c('purple4','yellow'))
  plot(dambo_clean, colNA = 'purple4', col = 'yellow') #some tiny patches went away. was the min size enough?
  
  freq(dambo_raster)
  freq(dambo_clean)
  
  #calculate dist raster
  dambo_dist_rast <- terra::distance(dambo_clean)
    plot(dambo_dist_rast)
    summary(dambo_dist_rast)
    
  #now summarize at 3x3 and 5x5?  
  dambo_dist_rast_mean_3x3 <- focal(dambo_dist_rast, w = 3, fun = 'mean', na.rm = FALSE)  
  dambo_dist_rast_mean_5x5 <- focal(dambo_dist_rast, w = 5, fun = 'mean', na.rm = FALSE)  
  dambo_dist_rast_sd_3x3 <- focal(dambo_dist_rast, w = 3, fun = 'sd')  
  dambo_dist_rast_sd_5x5 <- focal(dambo_dist_rast, w = 3, fun = 'sd')  
  
  #extract dist to dambo for each camera station
  dambo_distances <- terra::extract(dambo_dist_rast_mean_3x3, camera_coords, bind = TRUE)
    names(dambo_distances)[ncol(dambo_distances)] <- 'dist_dambo_mean_3x3'
  dambo_distances <- terra::extract(dambo_dist_rast_sd_3x3, dambo_distances, bind = TRUE)
    names(dambo_distances)[ncol(dambo_distances)] <- 'dist_dambo_sd_3x3'
    
  dambo_distances <- terra::extract(dambo_dist_rast_mean_5x5, dambo_distances, bind = TRUE)
    names(dambo_distances)[ncol(dambo_distances)] <- 'dist_dambo_mean_5x5'
  dambo_distances <- terra::extract(dambo_dist_rast_sd_5x5, dambo_distances, bind = TRUE)
    names(dambo_distances)[ncol(dambo_distances)] <- 'dist_dambo_sd_5x5'
  
  #view summaries  
  names(dambo_distances)
  summary(dambo_distances$dist_dambo_mean_3x3); summary(dambo_distances$dist_dambo_sd_3x3)
  hist(dambo_distances$dist_dambo_mean_3x3)
  hist(dambo_distances$dist_dambo_sd_3x3)
  
  
## Calculate distance from cleared areas ----------------------------------  
  
  #we're using the original raster now, not the 3x3 or 5x5 summaries 
  
  #which LC class is 'Cleared'?
  lc_key
  freq(lc_raster_rc) #50 is Cleared
  
  #reclassify 0/1
  cleared_raster <- lc_raster == 50 
  freq(cleared_raster)
  
  #shrink (e.g., remove small areas). first set min patch size (pixels)
  min_size <- 2
  
  #calculate patch sizes
  cleared_patches_rast <- terra::patches(cleared_raster, zeroAsNA=TRUE) #is zeroAsNA=TRUE here correct?
  cleared_patch_sizes <- terra::zonal(cleared_raster, cleared_patches_rast, fun="sum")
    summary(cleared_patch_sizes[,2])
    head(cleared_patch_sizes)
  
  #keep patches > minimum size
  cleared_patches_keep <- cleared_patch_sizes[cleared_patch_sizes$land_cov >= min_size, 1]
  cleared_clean <- ifel(cleared_patches_rast %in% cleared_patches_keep, 1, NA)
  
  #compare with original
  plot(cleared_raster, col = c('purple4','yellow'))
  plot(cleared_clean, colNA = 'purple4', col = 'yellow')
  
  freq(cleared_raster)
  freq(cleared_clean)
  
  #calculate dist raster
  cleared_dist_rast <- terra::distance(cleared_clean)
    plot(cleared_dist_rast)
    summary(cleared_dist_rast)
  
  #now summarize at 3x3 and 5x5?  
  cleared_dist_rast_mean_3x3 <- focal(cleared_dist_rast, w = 3, fun = 'mean', na.rm = FALSE)  
  cleared_dist_rast_mean_5x5 <- focal(cleared_dist_rast, w = 5, fun = 'mean', na.rm = FALSE)  
  cleared_dist_rast_sd_3x3 <- focal(cleared_dist_rast, w = 3, fun = 'sd')  
  cleared_dist_rast_sd_5x5 <- focal(cleared_dist_rast, w = 3, fun = 'sd')  
  
  #extract dist to cleared for each camera station
  cleared_distances <- terra::extract(cleared_dist_rast_mean_3x3, camera_coords, bind = TRUE)
    names(cleared_distances)[ncol(cleared_distances)] <- 'dist_cleared_mean_3x3'
  cleared_distances <- terra::extract(cleared_dist_rast_sd_3x3, cleared_distances, bind = TRUE)
    names(cleared_distances)[ncol(cleared_distances)] <- 'dist_cleared_sd_3x3'
    
  cleared_distances <- terra::extract(cleared_dist_rast_mean_5x5, cleared_distances, bind = TRUE)
    names(cleared_distances)[ncol(cleared_distances)] <- 'dist_cleared_mean_5x5'
  cleared_distances <- terra::extract(cleared_dist_rast_sd_5x5, cleared_distances, bind = TRUE)
    names(cleared_distances)[ncol(cleared_distances)] <- 'dist_cleared_sd_5x5'
    
  #view summaries  
  names(cleared_distances)
  summary(cleared_distances$dist_cleared_mean_3x3); summary(cleared_distances$dist_cleared_sd_3x3)
  hist(cleared_distances$dist_cleared_mean_3x3)
  hist(cleared_distances$dist_cleared_sd_3x3)    
    
  
## Read covariate shapefiles (rivers) -------------------------------------------
  
  rivers <- vect('data/raw/covariates/Nkhotakota Veg Data/Shapefiles/nwr_rivers.shp.zip')
  rivers <- project(rivers, crs(nwr_crs))

  plot(rivers)
    
  
## Calculate distance to rivers (vector) ---------------------------------------    
  
  #calculate distance to all rivers
  dist_to_rivers <- terra::distance(camera_coords, rivers)
    dim(dist_to_rivers)

  #find just the nearest river
  nearest_river <- apply(dist_to_rivers, 1, min)
    
  #now add to camera coords info
  river_distances <- camera_coords
  river_distances$dist_river <- nearest_river  

  #view summary
  summary(river_distances$dist_river)
  hist(river_distances$dist_river)
  
  
## Combine/save all covariate values -------------------------------------------
  
  head(values_3x3)
  head(values_5x5)
  
  head(lc_values_3x3) 
  head(lc_values_5x5)
  
  head(dambo_distances)
  head(cleared_distances)
  head(river_distances)
  
  #actually just export them all and I will work with them in the next script
  
  #save as csv bc column names are truncated to 10 characters in shp
  write.csv(as.data.frame(values_3x3), 'spatial/summarized_covariates/continuous_3x3.csv', row.names = FALSE)
  write.csv(as.data.frame(values_5x5), 'spatial/summarized_covariates/continuous_5x5.csv', row.names = FALSE)
  write.csv(as.data.frame(lc_values_3x3), 'spatial/summarized_covariates/landcover_3x3.csv', row.names = FALSE)
  write.csv(as.data.frame(lc_values_5x5), 'spatial/summarized_covariates/landcover_5x5.csv', row.names = FALSE)
  write.csv(as.data.frame(dambo_distances), 'spatial/summarized_covariates/dist_dambo.csv', row.names = FALSE)
  write.csv(as.data.frame(cleared_distances), 'spatial/summarized_covariates/dist_cleared.csv', row.names = FALSE)
  write.csv(as.data.frame(river_distances), 'spatial/summarized_covariates/dist_river.csv', row.names = FALSE)
  
  #save as shp
  # writeVector(values_3x3, 'spatial/summarized_covariates/continuous_3x3.shp')
  # writeVector(values_5x5, 'spatial/summarized_covariates/continuous_5x5.shp')
  # writeVector(lc_values_3x3, 'spatial/summarized_covariates/landcover_3x3.shp')
  # writeVector(lc_values_5x5, 'spatial/summarized_covariates/landcover_5x5.shp')
  # writeVector(dambo_distances, 'spatial/summarized_covariates/dist_dambo.shp')
  # writeVector(cleared_distances, 'spatial/summarized_covariates/dist_cleared.shp')
  # writeVector(river_distances, 'spatial/summarized_covariates/dist_river.shp')
  
  
## Maps for Appendix -----------------------------------------------------------    
  
  #read in study area boundary and convert to polygon
  study_area <- st_read('data/raw/covariates/Nkhotakota Veg Data/Shapefiles/Fence_line.shp.zip')
  study_area_polygon <- sf::st_cast(study_area, 'POLYGON')
  
  #clip continuous rasters to study area boundary (just 3x3 for plots for now)
  clipped_rasters_3x3 <- list()
  for (rr in 1:length(resampled_covar_3x3)){
    original_r <- resampled_covar_3x3[[rr]]
    cropped_r <- crop(original_r, study_area_polygon)
    masked_r <- mask(cropped_r, study_area_polygon)
    clipped_rasters_3x3[[(names(resampled_covar_3x3)[rr])]] <- masked_r
  }
  names(clipped_rasters_3x3)  

  #plot continuous rasters
  tiff('spatial/continuous_rasters_3x3.tiff', width=1500, height=1600, res=300)
  par(mfrow=c(3,2))
  for (ll in 1:length(clipped_rasters_3x3)){
    plot(clipped_rasters_3x3[[ll]], main = names(clipped_rasters_3x3)[ll])
  }  
  dev.off()
  
  #clip land cover rasters to study area boundary (just 3x3 for plots for now)
  clipped_lc_3x3 <- list()
  for (bb in 1:length(lc_props_3x3)){
    original_r <- lc_props_3x3[[bb]]
    cropped_r <- crop(original_r, study_area_polygon)
    masked_r <- mask(cropped_r, study_area_polygon)
    clipped_lc_3x3[[(names(lc_props_3x3)[bb])]] <- masked_r
  }
  names(clipped_lc_3x3)  
  
  #plot land cover rasters
  tiff('spatial/land_cover_rasters_3x3_select.tiff', width=1500, height=800, res=300)
  # par(mfrow=c(4,2))
  par(mfrow=c(1,3))
  # for (yy in 1:length(clipped_lc_3x3)){ #to plot them all
  for (yy in 1:3){ #just plot the first 3 now (M1, M2, M3)
    plot(clipped_lc_3x3[[yy]], main = names(clipped_lc_3x3)[yy])
  }  
  dev.off()
   
  #clip dist to dambo (just 3x3 for now)
  cropped_dambo_mean <- crop(dambo_dist_rast_mean_3x3, study_area_polygon)
    masked_dambo_mean <- terra::mask(cropped_dambo_mean, study_area_polygon)
  cropped_dambo_sd <- crop(dambo_dist_rast_sd_3x3, study_area_polygon)
    masked_dambo_sd <- terra::mask(cropped_dambo_sd, study_area_polygon)
  
  #clip dist to cleared (just 3x3 for now)
  cropped_cleared_mean <- crop(cleared_dist_rast_mean_3x3, study_area_polygon)
    masked_cleared_mean <- terra::mask(cropped_cleared_mean, study_area_polygon)
  cropped_cleared_sd <- crop(cleared_dist_rast_sd_3x3, study_area_polygon)
    masked_cleared_sd <- terra::mask(cropped_cleared_sd, study_area_polygon)
    
  #plot dist to dambo and dist to cleared disturbance
  tiff('spatial/dist_dambo_cleared_raster_3x32.tiff', width = 1500, height = 1200, res = 300)
  par(mfrow=c(2,2))
    plot(masked_dambo_mean, main = 'Distance to dambo (mean)')
    plot(masked_cleared_mean, main = 'Distance to cleared (mean)')
    plot(masked_dambo_sd, main = 'Distance to dambo (SD)')
    plot(masked_cleared_sd, main = 'Distance to cleared (SD)')
  dev.off()
  
  
  #plot dist to rivers? well it's not a raster
  tiff('spatial/rivers.tiff', width = 1500, height = 1500, res = 300)
  par(mfrow=c(1,1))
    plot(rivers, main = 'Rivers and streams')
  dev.off()
  
  
  
  