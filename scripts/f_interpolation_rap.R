library(mgcv)
library(leaflet)
library(viridis)
#library(wesanderson)

# Interpolation function
# Requires vectors of x and y coordinates
# and values (z)
## changed to sf (simple features) object, but will work with spatialPolygons too

GAM_interpolate <- function(x,
                            y,
                            z,
                            crop_polygon, 
                            minObs,
                            maxObs,
                            k,
                            Month, Year,
                            outputWebmap=TRUE){
  
  # fit model
  gam_mod <- mgcv::gam(z ~ s(x, y, bs="gp", k=k)) # -1 autoselects best k
  
  # generate prediction raster
  max_num_cell <- 600
  range_lng <- range(c(-122.12, -121.3812)) # manually set this to make surface equal for all years
  range_lat <- range(c(37.9, 38.2))
  wide_or_tall <- which.max(c(diff(range_lng), diff(range_lat)))
  cell_size <- abs(c(diff(range_lng)/max_num_cell, diff(range_lat)/max_num_cell)[wide_or_tall])
  
  #browser() #useful tool to look inside function
  x_coords <- seq(range_lng[1], range_lng[2], cell_size)
  y_coords <- seq(range_lat[1], range_lat[2], cell_size)
  grid <- expand.grid(x_coords, y_coords)
  names(grid) <- c("x", "y")
  pred_raster <- rasterFromXYZ(data.frame(x=grid[,1], y=grid[,2], z=NA),
                               crs = CRS("+init=epsg:4326"))
  
  # Predict
  pred_grid <- as.data.frame(coordinates(pred_raster))
  names(pred_grid) <- c("x", "y")
  pred_raster[] <- as.vector(predict(gam_mod, pred_grid))
  
  # Crop using the provided crop raster
  pred_raster <- mask(pred_raster, crop_polygon)
  
  # save out raster for future use
  print("Output raster to global environment...")
  assign(x = "pred_raster", value = pred_raster, envir = .GlobalEnv)
  print("Done!")
  
  # First define colors
  col_pal <- colorNumeric(viridis(64, option = "A"), c(minObs, maxObs), na.color = NA)
  
  if(outputWebmap){
    # Map
    map1 <- leaflet() %>% 
      setView(lng = -121.7961, lat=38.06166, zoom=11) %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenBW") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group="ESRI Canvas") %>%
      
      addRasterImage(pred_raster, group = "Interpolated Abundance",
                     colors = col_pal,
                     opacity = 0.9) %>%
      addCircleMarkers(x, y, popup = as.character(z), group = "Stations", 
                       radius = 3.5, color = "white",
                       weight= 1, 
                       fillOpacity=0.7, fillColor="#008080") %>%
      leaflet::addLegend(pal = col_pal, values = c(minObs, maxObs), title = "log(Cladocerans)") %>% 
      addControl(paste0(Month, "-", Year), position = "bottomleft") %>% 
      addLayersControl(
        baseGroups = c("ESRI Canvas", "OpenBW",
                       "Topo","ESRI Aerial"),
        overlayGroups = c("Interpolated Abundance", "Stations"),
        options = layersControlOptions(collapsed = T))
    
    print("Now printing leaflet!")
    print(map1)
  }
  else {
    print("No map required!")
  }
}

