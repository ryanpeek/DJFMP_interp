## Loop Data and Plot

# LIBRARIES ---------------------------------------------------------------

library(sf)
library(glue)
library(leaflet)
library(leaflet.extras)
# update mapview bc of fgb issue (https://github.com/r-spatial/mapview/issues/321)
#remotes::install_github("r-spatial/mapview")
library(mapview)
mapviewOptions(fgb = FALSE)
library(viridis)
library(purrr)
library(tidyverse)
library(lubridate)

# GET DATA ----------------------------------------------------------------

## this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
## more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON
# set year: 
updateYr <- 2019

# load data
load(glue("data/iep_stations_sf_{updateYr}.rda"))
load(glue("data/iep_cleaned_CB_data_{updateYr}.rda")) # Clarke-Bumpus (CB) sampled mesozooplankton

# select columns of interest, here: allcladocera
cb_clad <- cb_wide %>% dplyr::select(survey_code:volume, month, allcladocera, allcladocera_log, -core)

# GET SPATIAL DATA --------------------------------------------------------

# Read in shapefile and transform polygon
delta_sf <- st_read("data/spatial/Bay_delta_selection.shp") %>% st_transform(crs=4326)
st_crs(delta_sf) # double check projection/crs

# crop to region of interest...warning is ok!
crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.48, ymin = 37.77), 
                    crs = st_crs(4326))

# crop delta
delta_crop <- st_crop(delta_sf, crop_box)
stations_crop <- st_crop(stations_sf, crop_box) # warning is ok!


# JOIN SPATIAL STATIONS WITH DATA -----------------------------------------

# Join with station info for lat longs
cb_clad_crop <- dplyr::inner_join(cb_clad, stations_crop, by=c("station_nz"="station"))
cb_clad_sta <- dplyr::inner_join(cb_clad, stations_sf, by=c("station_nz"="station")) # all stations

# make a cropped sites only spatial object
cb_clad_crop_sites_sf <- cb_clad_crop %>% distinct(station_nz, .keep_all = TRUE) %>% 
  st_as_sf(., coords = c("lon","lat"), crs=4326, remove=FALSE) # cropped stations
  
# make all sites only spatial object:
cb_clad_sites_sf <- cb_clad_sta %>% dplyr::distinct(station_nz, .keep_all=TRUE) %>% 
  st_as_sf(., coords = c("lon","lat"), crs=4326, remove=FALSE) # all stations

# Cropped Stations
mapstations <-  #mapview(delta_sf, col.regions="blue3", alpha.regions=0.4, legend=F) +
    mapview(delta_crop, col.regions="steelblue3", alpha.regions=0.8, legend=F) +
    mapview(cb_clad_sites_sf, col.regions="darkblue", alpha=0.5, cex=1.75, layer.name="All Sites") +
    mapview(cb_clad_crop_sites_sf, col.regions="orange", layer.name="Cropped Sites")

# mapofsites
mapview::mapshot(mapstations, url = glue("{here::here()}/figures/map_of_IEP_sites.html"), selfcontained=TRUE)

#save(cb_clad_crop, delta_crop, file = "data_output/sf_cb_clad_delta_crop.rda")

# SETUP AND MAP -----------------------------------------

# add a cut number to the table based on zoop numbers:
cb_clad_crop <- cb_clad_crop %>% #cut(das$anim, 3, labels=FALSE)
  mutate(allclad_cut = as.integer(Hmisc::cut2(allcladocera_log, g=5)))
summary(cb_clad_crop$allclad_cut)
summary(cb_clad_crop$allcladocera_log)


# Define the legend scale bar with the min/max abundance across all data you are going to show (otherwise the scale bar changes between time steps)
YY <- 2019
MM <- 5
min_obs <- 0 # log transformed
max_obs <- 13.1 # log transformed


# filter to specific year or years
cb_yr <- cb_clad_crop %>% filter(year==YY, month==MM) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)

cb_yr1405 <- cb_clad_crop %>% filter(year==2014, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
cb_yr1505 <- cb_clad_crop %>% filter(year==2015, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
cb_yr1605 <- cb_clad_crop %>% filter(year==2016, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
cb_yr1705 <- cb_clad_crop %>% filter(year==2017, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
cb_yr1805 <- cb_clad_crop %>% filter(year==2018, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
cb_yr1905 <- cb_clad_crop %>% filter(year==2019, month==5) %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)

# mapview
mapview(cb_yr1905, zcol="allcladocera_log", cex="allcladocera_log", 
        layer.name=paste0(YY, "-", month.abb[MM],"<br>All Cladocera (log)")) +
  mapview(delta_crop, col.regions="steelblue3", alpha.regions=0.8, legend=F) +
  # add all stations
  mapview(cb_clad_sites_sf, col.regions="gray50", cex=3, alpha=0.2, legend=FALSE)
  


# BUILD MANUAL WEBMAP -----------------------------------------------------
# 
# col_pal <- colorNumeric(viridis(64, option = "B"), c(min_obs, max_obs), na.color = NA)
#  
# # map
# leaflet() %>%
#   setView(lng = -121.7961, lat=38.06166, zoom=11) %>%
#   addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
#   addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
#   addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenBW") %>%
#   addProviderTiles("Esri.WorldGrayCanvas", group="ESRI Canvas") %>%
#  
#   addCircleMarkers(data=cb_yr1405, group="May2014", lng = ~lon, lat= ~lat,
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station_nz,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   
#   addCircleMarkers(data=cb_yr1505, ~lon, ~lat, group="May2015",
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station_nz,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   addCircleMarkers(data=cb_yr1605, ~lon, ~lat, group="May2016",
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station_nz,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   addCircleMarkers(data=cb_yr1705, ~lon, ~lat, group="May2017",
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station_nz,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   addCircleMarkers(data=cb_yr1805, ~lon, ~lat, group="May2018",
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station_nz,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   
#   
#   addLegend(pal = col_pal, values = c(min_obs, max_obs), title = "log(All Clad.)") %>%
#   addFullscreenControl() %>% 
#   #addControl(paste0(month.abb[MM], "-", YY), position = "bottomleft") %>% 
#   addLayersControl(position = "topleft",
#                    baseGroups = c("ESRI Canvas", "OpenBW",
#                                   "Topo","ESRI Aerial"),
#                    overlayGroups = c("May2014","May2015","May2016","May2017","May2018"),
#                    options = layersControlOptions(collapsed = T))
# 


# MAKE STATIC MAP ---------------------------------------------------------

library(ggmap)
library(ggspatial)


location=c(-121.82,38.05) # set the center of the map
# set the background map up
map1 <- get_map(location=location, crop = F,
                color="bw",
                maptype="terrain",
                source="google",
                zoom=10)

yr <- c(2014)
mon <- c(2:5)
min_obs <- 0 # log transformed
max_obs <- 13.1 # log transformed

# for radii
# Define circle radius and color
# Points$Ncat<-as.numeric(cut(Points$Total_N,breaks=c(0,500,50000,250000,10000000,1000000000), include.lowest=TRUE))
# PointsYear$NCat<-as.numeric(cut(PointsYear$Total_N,breaks=c(0,500,50000,250000,10000000,150000000), include.lowest=TRUE))
# Radii<-c(4,8,12,16,22)
# Points$Radius<-Radii[Points$Ncat]
# PointsYear$Radius<-Radii[PointsYear$NCat]

# make cb_clad_crop spatial
cb_clad_crop_sf <- cb_clad_crop %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)


ggmap(map1) +
#ggplot() +
  annotation_scale(location = "bl", height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
  theme_bw(base_size = 9) +#base_family = "Roboto Condensed") +
  theme(panel.grid.major = element_line(color = "gray90")) +
  annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),width = unit(0.8, "cm"), 
                        pad_y = unit(0.6, "cm"),style = north_arrow_fancy_orienteering(text_size = 8),
                        which_north = "true") +
  scale_fill_viridis_c("log(Clad)", option = "A",
                       na.value = "transparent", limits=c(0,14)) +
  scale_size_continuous(guide="legend") +
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5, inherit.aes=F) +
  geom_sf(data=cb_clad_crop_sf %>% dplyr::filter(year %in% yr & month %in% mon), 
          aes(fill=allcladocera_log, size=allclad_cut), pch=21, color="gray20", alpha=0.9) +
  labs(title = glue("All Cladocerans: {yr} {month.abb[min(mon)]}-{month.abb[max(mon)]}"), x="", y="", 
       caption = "Data Source: CDFW IEP Zooplankton Clarke-Bumpus, https://www.wildlife.ca.gov/Conservation/Delta/Zooplankton-Study") +
  #coord_sf(xlim = c(-122.25, -121.35), ylim=c(38.38, 37.85), crs=4326) +
  facet_wrap(.~month)

# w background
# ggsave(filename = paste0("figures/all_cladocera_points_", yr, "_months_", min(mon), "_",max(mon), "_ggmap.pdf"),width = 10, height = 8, units = "in")

# no background
#ggsave(filename = paste0("figures/all_cladocera_points_", yr, "_months_", min(mon), "_",max(mon), ".pdf"),width = 10, height = 8, units = "in")

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
                    z=cb_yr$allcladocera_log,
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


