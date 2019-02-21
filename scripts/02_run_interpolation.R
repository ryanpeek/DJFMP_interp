## Interpolate Data and Plot

# LIBRARIES ---------------------------------------------------------------

library(rgdal)
library(raster)
library(sf)
library(mapview)
library(purrr)
library(tidyverse)
library(lubridate)

# LOAD FUNCTION -----------------------------------------------------------

# First run function so it's ready to go in your environment
source("scripts/f_interpolation_rap.R")

# GET DATA ----------------------------------------------------------------

## this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
## more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON

load("data/stations_sf.rda")
load("data/cleaned_CB_data.rda") # Clarke-Bumpus (CB) sampled mesozooplankton

# select columns of interest, here: ALLCLADOCERA
cb_clad <- cb_wide %>% dplyr::select(SurveyCode:CBVolume, ALLCLADOCERA, ALLCLADOCERA_log, -Core) %>% mutate(MM=month(Date))

# GET SPATIAL DATA --------------------------------------------------------

# Read in shapefile and transform polygon
delta_sf <- st_read("data/Bay_delta_selection.shp") %>% st_transform(crs=4326)
st_crs(delta_sf) # double check projection/crs

# crop to region of interest...warning is ok!
crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.48, ymin = 37.77), 
                    crs = st_crs(4326))

# crop delta
delta_crop <- st_crop(delta_sf, crop_box)
stations_crop <- st_crop(stations_sf, crop_box) # warning is ok!

# test plots
# plot(delta_sf$geometry, col=scales::alpha("blue", 0.5), axes=T)
# plot(delta_crop$geometry, col=scales::alpha("orange", 0.7), axes=T)
# plot(stations_crop$geometry, col="black", add=T)

# quick mapview/leaflet interactive map of the delta
# see here for advanced mapview options: https://environmentalinformatics-marburg.github.io/mapview/advanced/advanced.html

# mapview
# mapview(delta_sf, alpha.regions=0.7, show.layer=F) + 
#   mapview(delta_crop, col.regions="orange") + 
#   mapview(stations_crop, layer.name="CB Stations", col.regions="yellow")

# JOIN SPATIAL STATIONS WITH DATA -----------------------------------------

# Join with station info for lat longs
cb_clad_crop <- dplyr::inner_join(cb_clad, stations_crop, by=c("Station"))

# make into sf spatial object:
cb_clad_crop <- st_as_sf(cb_clad_crop, coords = c("lon","lat"), crs=4326, remove=FALSE)

# mapview(cb_clad_crop, col.regions="yellow") + mapview(delta_crop, col.regions="blue", alpha.regions=0.4)

save(cb_clad_crop, delta_crop, file = "data_output/sf_cb_clad_crop_delta_crop.rda")

# SETUP AND RUN INTERPOLATION FUNCTION -------------------------------------

# Define the legend scale bar with the min/max abundance across all data you are going to show (otherwise the scale bar changes between time steps)
year <- 1977
month <- 3
min_obs <- 0 # log transformed
max_obs <- 14 # log transformed

# filter to specific year or years
cb_yr <- cb_clad_crop %>% filter(Year==year, MM==month)
#mapview(cb_yr, col.regions="maroon") + mapview(cb_clad_crop)

# run function
GAM_interpolate(x=cb_yr$lon, y=cb_yr$lat, 
                z=cb_yr$ALLCLADOCERA_log,
                minObs = min_obs, maxObs = max_obs,
                delta_crop, k=10, Month = month, Year=year,
                outputWebmap = T)

# save out for demo

#clad_2017_06_rast <- pred_raster
#save(clad_2017_06_rast, file = "data_output/pred_raster_clad_2017_06_rast.rda")
#crs(clad_2017_06_rast) <- CRS('+init=EPSG:4326')
#slideView(img1 = clad_2015_06_rast, img2 = clad_2017_06_rast)
#widgetframe::frameWidget(sv2)

# BUILD MANUAL WEBMAP -----------------------------------------------------

col_pal <- colorNumeric(viridis(64, option = "B"), c(min_obs, max_obs), na.color = NA)
 
leaflet() %>%
  setView(lng = -121.7961, lat=38.06166, zoom=11) %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenBW") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group="ESRI Canvas") %>%

  addRasterImage(clad_1977_06_rast, group="Interpolated Abundance",
                 colors = col_pal,
                 opacity = 0.8) %>%
  addCircleMarkers(data=cb_yr, ~lon, ~lat, group="Stations",
                   radius = 3.5, color = "white", fillColor = "#008080",
                   weight= 1, fillOpacity=0.7, stroke=TRUE,
                   popup = paste0("All Cladoceran (log) = ", cb_yr$ALLCLADOCERA_log,
                                  "<br> Station: ", cb_yr$Station,
                                  "<br> Lon: ", cb_yr$lon,
                                  "<br> Lat: ", cb_yr$lat,
                                  "<br> Region: ", cb_yr$Region,
                                  "<br> Temperature: ", cb_yr$Temperature)) %>%
  addLegend(pal = col_pal, values = c(min_obs, max_obs), title = "log(Cladocerans)") %>%
  addControl(paste0(month, "-", year), position = "bottomleft") %>% 
  addLayersControl(
    baseGroups = c("ESRI Canvas", "OpenBW",
                   "Topo","ESRI Aerial"),
    overlayGroups = c("Interpolated Abundance", "Stations"),
    options = layersControlOptions(collapsed = T))

# MAKE STATIC MAP ---------------------------------------------------------

library(rasterVis)
library(RColorBrewer)

static_Rasterplot <- function(Yr, Mon, rasterfile, file_type){
  
  print(paste0("Making figure for: ", Yr, "-", Mon))
  
  # filter to year
  cb_yr <- cb_clad_crop %>% filter(Year==Yr, MM==Mon)
  
  # run interpolation function
  GAM_interpolate(x=cb_yr$lon, y=cb_yr$lat, 
                  z=cb_yr$ALLCLADOCERA_log,
                  minObs = min_obs, maxObs = max_obs,
                  delta_crop, k=6, outputWebmap = F)
  
  
  print("Making plot")
  rastPlot <- levelplot(x = rasterfile, xlab="", ylab="",
                        xlim = c(-122.12, -121.3812),
                        ylim = c(37.9, 38.2),
                        cex=0.8,
                        at=seq(0, 10, length=100), pretty=TRUE,
                        main=paste0("All Cladocera: ", Yr, "-", Mon),
                        colorkey=list(
                          space="bottom", height=0.7,
                          title="log(Abund)",
                          #axis.line=list(col="transparent"),
                          col.regions=viridis),
                        margin=list(draw=FALSE))
  
  print("Saving plot...")
  png(filename = paste0("./figures/", Yr, "_", Mon, "_all_cladocera", file_type), 
      width = 7, height = 5, 
      units = "in", res = 300)
  print(rastPlot)
  dev.off()
  print("Plot saved!")
  # now print again for output
  #print(rastPlot)
}

#test
static_Rasterplot(Yr = year, Mon = month, pred_raster, ".png")


# STITCHING THINGS TOGETHER -----------------------------------------------

# # if you want to filter to specific range of years/months
years <- seq(1993, 2009, 1)
months <- seq(1, 6, 1)
cb_clad_filt <- cb_clad_crop %>% filter(Year %in% years, MM %in% months)

# PURRR MAP by MONTH ------------------------------------------------------

# save function:
saveit <- function(..., string, file) {
  x <- list(...)
  names(x) <- string
  save(list=names(x), file=file, envir=list2env(x))
}

# make function
static_RasterMonth <- function(dataFile, fileN, ggplots=TRUE, saveOut=TRUE){
  
  print(paste0("Making figure for: ", fileN))
  
  cb_yr <- dataFile 
  
  try({
    mm <- unique(month(cb_yr$Date)) # lubridate function
    yyyy <- unique(year(cb_yr$Date)) # lubridate function
    
    # run interpolation function
    GAM_interpolate(x=cb_yr$lon, y=cb_yr$lat, 
                    z=cb_yr$ALLCLADOCERA_log,
                    minObs = min_obs, maxObs = max_obs,
                    delta_crop, k=10, Month = mm, Year=yyyy,
                    outputWebmap = F)
    
    if(ggplots){
      
      print("Making plot with ggplot")
      
      if(!require(ggplot2)){
        install.packages("ggplot2")
        library(ggplot2)}
      
      if(!require(ggspatial)){
        install.packages("ggspatial")
        library(ggspatial)}
      
      # using ggplot
      pred_raster_df <- as.data.frame(pred_raster, xy=TRUE)
      
      rastPlot<-ggplot() +
        geom_sf(data=delta_crop, fill="gray30", alpha=0.5) +
        geom_raster(data = pred_raster_df,
                    aes(x = x, y = y, fill = z)) + 
        geom_sf(data=cb_yr, pch=21, size=2, 
                color="gray20",fill="white", alpha=0.9) +
        scale_fill_viridis_c("log(Clad)", option = "A",
                             na.value = "transparent", limits=c(0,14)) + 
        coord_sf(xlim = c(-122.12, -121.3812), ylim=c(38.2, 37.88), crs=4326) +
        labs(title = paste0("All Cladocera: ", yyyy, "-", mm), x="", y="") +
        annotation_scale(location = "bl") +
        theme_classic(base_size = 9) +
        theme(panel.grid.major = element_line(color = "gray80")) +
        annotation_north_arrow(location = "bl", width = unit(1, "cm"), 
                               pad_y = unit(0.7, "cm"),
                               which_north = "true")
      #print(rastPlot)
      #plotName <- paste0("Clad","_",yyyy, "_", mm)
      #assign(plotName, rastPlot, envir = .GlobalEnv)
      
    }
    else{
      
      print("Making plot with levelplot")
      rastPlot <- levelplot(x = pred_raster, xlab="", ylab="",
                            xlim = c(-122.12, -121.3812),
                            ylim = c(37.9, 38.2),
                            cex=0.8,
                            at=seq(0, 10, length=100), pretty=TRUE,
                            main=paste0("All Cladocera: ", yyyy, "-", mm),
                            colorkey=list(
                              space="bottom", height=0.7,
                              title="log(Abund)",
                              #axis.line=list(col="transparent"),
                              col.regions=viridis),
                            margin=list(draw=FALSE))
    }
    if(saveOut){
      
      print("Save out rasters...")
      rastName <- paste0("rast_clad","_",yyyy, "_", mm)
      saveit(pred_raster, string = rastName, file = paste0("data_output/", rastName, ".rda"))
      
      print("Saving plot...")
      png(filename = paste0("./figures/", fileN, "_all_cladocera.png"), 
          width = 7, height = 5,
          units = "in", res = 300)
      print(rastPlot)
      dev.off()
      print("Plot saved!")
    }
    else{
      # now print again for output
      print(rastPlot)
    }})
}


# color scale based on this range
min_obs <- 0 # log transformed
max_obs <- 14 # log transformed

# make list of data frames, change to cb_clad_filt if you want specific range
dfs_model <- cb_clad_filt %>% group_by(Year, MM) %>% nest

# map over list
filenames <- paste0(dfs_model$Year, "-", dfs_model$MM) # set names
plots <- dfs_model %>% mutate(plot = map2(data, filenames, .f = static_RasterMonth))

#save(list=ls(pattern = "^Clad_"), file = "data_output/ggplots_clad_rast_1972_2018_01_06.rda")

# SINGLE RUN --------------------------------------------------------------


### OR PREP FOR SINGLE RUN

# prep single year and month
year <- 2017
month <- 4
cb_clad_filt <- cb_clad_crop %>% filter(Year %in% year, MM %in% month)

# plot single
static_RasterMonth(cb_clad_filt, paste0("clad_",year,"_",month), ggplots = T, saveOut = F)

# cowplot
cowplot::plot_grid(Clad_2014_4, Clad_2017_4, nrow=2)
library(patchwork)
Clad_2014_4 + Clad_2017_4 + plot_layout(ncol = 1, heights = c(1, 1))


# ANIMATE -----------------------------------------------------------------

# alternatively, use ImageMagick in OSX shell:
# convert -delay 30 -loop 0 *.png animated_allclad.gif

# see this post I wrote about how to do this:
# https://ryanpeek.github.io/2016-10-19-animated-gif_maps_in_R/


