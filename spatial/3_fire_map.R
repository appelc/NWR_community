# Make study area map

library(terra)       
library(sf)          
library(ggplot2)     
library(ggspatial)   # For scale bar and north arrow
library(tidyterra)   # For better integration between terra and ggplot2
library(rnaturalearth) # For country boundaries
library(rnaturalearthdata)
library(cowplot)     # For combining plots
library(maptiles) 


## Set CRS ---------------------------------------------------------------------

  nwr_crs <- 'EPSG:32736' #WGS84, UTM zone 36S


# Load data --------------------------------------------------------------------

  #hillshade raster
  hillshade <- rast('data/raw/covariates/Nkhotakota Veg Data/TIFFS/nwr_hillshade.tif')
  
  #study area boundary
  study_area <- st_read('data/raw/covariates/Nkhotakota Veg Data/Shapefiles/Fence_line.shp.zip')
  
  #interior fence
  fence <- st_read('data/raw/covariates/south_fence_shp_south_edited.shp')
    
  # #major rivers
  # bua_river <- st_read('data/raw/covariates/nwr_rivers_bua.shp')
  # kaombe_river <- st_read('data/raw/covariates/nwr_rivers_kaombe.shp')
    
  #grid cells
  grid_cells <- st_read('data/raw/covariates/from_Ray_2024/Nkhotakota_hexagon_summaries/Nkhotakota_hexagon_summaries.shp')
  
  #plot coordinates
  plot_coords <- read.csv('data/raw/covariates/MonitoringPlots_corrected_031825_withFence.csv')
  
    #convert coords to sf object
    plot_points <- st_as_sf(plot_coords, coords = c('UTMXcorrec','UTMYcorrec'), crs = nwr_crs)
    
  # #animal release sites (bomas)
  # boma_sites <- st_read('data/raw/covariates/boma_sites.shp')
  # 
  # #get Malawi country boundary for inset map
  # malawi <- ne_countries(scale = "medium", country = "Malawi", returnclass = "sf")
  # 
  # #get Africa for context in the inset
  # africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
  

# Read fire data and merge with plot coords ------------------------------------
    
  fire_data <- fread('clip_fire/fire_detections.csv')
  fire_sites <- unique(fire_data$site)
  
  plot_points <- plot_points %>% mutate(fire = factor(ifelse(Site %in% fire_sites, 1, 0)))
  
    
# Create map -------------------------------------------------------------------
  
  fire_map <- ggplot() +
    
    #add study area boundary
    geom_sf(data = study_area, fill = NA, color = "black", linewidth = 0.8, alpha = 0.8) +
    
    #add camera plots (OPTIONAL)
    geom_sf(data = plot_points, aes(color = fire, size = fire)) +
    scale_color_manual(values = c('0' = "gray", '1' = "darkorange2"), name = 'fire') +
    # scale_size_manual(values = c('0' = 2, '1' = 5)) +
    scale_size_manual(values = c('0' = NA, '1' = 5)) +
    
    #add north arrow 
    annotation_north_arrow(
      location = "bl", 
      which_north = "true",
      pad_x = unit(0.05, "in"), 
      pad_y = unit(0.3, "in"),
      height = unit(1, 'in'),
      width = unit(1, 'in'),
      style = north_arrow_nautical #or 'north_arrow_fancy_orienteering'
    ) +
    
    #add scale bar
    annotation_scale(
      location = "bl",
      bar_cols = c("black", "white"),
      text_col = "black",
      width_hint = 0.25,
      text_cex = 1
    ) +
    
    #add styling
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_text(size = 12)
    )
  
  fire_map
  
  ggsave("spatial/fire_map.png", fire_map, width = 9, height = 8, dpi = 300)
  ggsave("spatial/fire_map2.png", fire_map, width = 9, height = 8, dpi = 300)
  
  
      