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
    
  #major rivers
  bua_river <- st_read('data/raw/covariates/nwr_rivers_bua.shp')
  kaombe_river <- st_read('data/raw/covariates/nwr_rivers_kaombe.shp')
    
  #grid cells
  grid_cells <- st_read('data/raw/covariates/from_Ray_2024/Nkhotakota_hexagon_summaries/Nkhotakota_hexagon_summaries.shp')
  
  #plot coordinates
  plot_coords <- read.csv('data/raw/covariates/MonitoringPlots_corrected_031825_withFence.csv')
  
    #convert coords to sf object
    plot_points <- st_as_sf(camera_csv, coords = c('UTMXcorrec','UTMYcorrec'), crs = nwr_crs)
  
  #get Malawi country boundary for inset map
  malawi <- ne_countries(scale = "medium", country = "Malawi", returnclass = "sf")
  
  #get Africa for context in the inset
  africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
  

# Get baselayers ---------------------------------------------------------------
  
  
    
# Create main map --------------------------------------------------------------
  
  main_map <- ggplot() +
    
    #add hillshade using tidyterra's geom_spatraster
    geom_spatraster(data = hillshade) +
    scale_fill_gradientn(colors = gray.colors(100, start = 0.2, end = 0.9), guide = "none", na.value = NA) +
    
    #add hexagon grid
    geom_sf(data = grid_cells, fill = NA, color = "darkgrey", size = 0.5, alpha = 0.7) +
    geom_sf(data = grid_cells[grid_cells$Sample == 1,], fill = 'forestgreen', color = "darkgrey", size = 0.5, alpha = 0.7) +
    
    #add fence
    geom_sf(data = fence, fill = NA, color = 'black', linewidth = 0.9, linetype = 'dashed') +
    
    #add rivers (OPTIONAL?)
    geom_sf(data = bua_river, color = 'dodgerblue3', linewidth = 0.3) +
    geom_sf(data = kaombe_river, color = 'dodgerblue3', linewidth = 0.3) +
    
    #add study area boundary
    geom_sf(data = study_area, fill = NA, color = "black", linewidth = 0.8, alpha = 0.8) +
    
    #add camera plots (OPTIONAL)
    # geom_sf(data = plot_points, shape = 21, fill = "black", color = "black", size = 0.5, alpha = 0.8) +
    
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
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_text(size = 12)
    )
  
  
# Create inset map (in Malawi) -------------------------------------------------
  
  #convert study area boundary to polygon so I can fill it in
  study_area_polygon <- sf::st_cast(study_area, 'POLYGON')
  
  #plot
  inset_map1 <- ggplot() +
    geom_sf(data = malawi, fill = "darkgrey", color = "black", size = 0.5) +
    # geom_sf(data = study_area, color = "red", size = 3) + #for study area as 1 point
    geom_sf(data = study_area_polygon, fill = 'red', color = 'red') +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.5)
    )
  
  
# Create inset map (in Africa) -------------------------------------------------
  
  inset_map2 <- ggplot() +
    geom_sf(data = africa, fill = "lightgrey", color = "darkgrey", size = 0.2) +
    geom_sf(data = malawi, fill = "black", color = "black", size = 0.5) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  
# Create inset map (hexagon detail) --------------------------------------------
  
  #select one hex for display, and get plot cords there
  hex <- grid_cells[grid_cells$GRID_ID == 'P-23',] 
  points_in_hex <- plot_points[plot_points$HexIDcorre == 'P23',]
  
  #make a bbox to pad limits
  padding_factor <- 0.18
  bbox <- st_bbox(hex)
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]
  new_xlim <- c(bbox["xmin"] - width * padding_factor, 
                bbox["xmax"] + width * padding_factor)
  new_ylim <- c(bbox["ymin"] - height * padding_factor, 
                bbox["ymax"] + height * padding_factor)
  
  #plot
  inset_map3 <- ggplot() +
    geom_sf(data = hex, fill = 'forestgreen', color = "darkgrey", size = 0.5, alpha = 0.7) +
    geom_sf(data = points_in_hex, shape = 21, fill = "black", color = "black", size = 3, alpha = 0.8) +
    annotation_scale(
      location = "bl",
      pad_y = unit(0, 'cm'),
      pad_x = unit(1, 'cm'),
      # bar_cols = c("black", "white"),
      # text_col = "black",
      width_hint = 0.4,
      height = unit(0.4, 'cm'),
      style = 'ticks',
      text_cex = 1
    ) +
    coord_sf(xlim = new_xlim, ylim = new_ylim) + #zoom out to new bbox limits
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )

    
# Combine ----------------------------------------------------------------------
  
  #combine main map and inset
  final_map <- ggdraw() +
    draw_plot(main_map) +
      draw_plot_label('a', x = 0.15, y = 0.99, size = 16, fontface = 'bold') +
    draw_plot(inset_map3, x = 0.77, y = 0.75, width = 0.25, height = 0.25) + #hexagon
      draw_plot_label('b', x = 0.8, y = 0.99, size = 16, fontface = 'bold') +
    draw_plot(inset_map1, x = 0.7, y = 0.3, width = 0.4, height = 0.4) + #Africa
      draw_plot_label('c', x = 0.8, y = 0.7, size = 16, fontface = 'bold') +
    draw_plot(inset_map2, x = 0.75, y = 0.0, width = 0.28, height = 0.28) + #Malawi
      draw_plot_label('d', x = 0.8, y = 0.3, size = 16, fontface = 'bold')
  
  #display the map
  print(final_map)
  
  #save 
  ggsave("spatial/malawi_study_area_map.png", final_map, width = 9, height = 8, dpi = 300)
    
  
  