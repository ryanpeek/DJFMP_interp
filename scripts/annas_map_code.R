# DJFMP sampling locations for gut contents analyses

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, dplyr, scales, lubridate, sf, mapview, leaflet, rgdal, readxl) 


# Map samples locs ---------------------------------------------------------------

# Read in sites and sample sizes created in Clean_up_gut_data.R
sites_df = read.csv("data_outputs/guts.df.taxa_041720.csv")

dist_df = read.csv('data/DISTANCES_FROM_FP_KN.csv') #Kelly-measured distances from FP outlet to sampling site. Done in ArcGIS in rkm 
# remember that some sites are below both floodplains so for these ones there are 2 distances so first remove "dupes" just to plot map

sites_below_fp = unique(dist_df$Site.Name)

outlets = filter(dist_df, Distance_rkm ==0)

sites = sites_df %>%
  filter(!Region_subdivided5 %in% c("excl", "Bay")) %>%
  group_by(Region_subdivided5, Site.Name, Lat, Long, Fish_ID) %>%
  summarize(N_critters = n()) %>%
  group_by(Region_subdivided5, Site.Name, Lat, Long) %>%
  summarize(N_fish = n()) %>%
  mutate(below_FP = case_when(
    Site.Name %in% sites_below_fp ~ 'y',
    TRUE ~ 'n'))

# Make into a spatial object
sites_sf <- sites %>% st_as_sf(coords = c("Long","Lat"), remove = F, crs=4326)

# Quick mapview 
mapview(sites_sf, zcol="Region_subdivided5", layer.name="Region",
        alpha = .5, alpha.regions = .2)  #Can't work out to add shape file to mapview so doing in leaflet

# Separate sites into groups because I couldn't work out how to do different sized symbols by group
below_fp = filter(sites, below_FP == "y")
above_fp = filter(sites, below_FP == "n") 

##--------- Add in zoop sampling sites & Delta river lines
# this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
# more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON
# The Zooplankton Study currently samples 19 stations monthly, including 17 fixed stations and 2 floating entrapment zone stations located at bottom electrical conductivity (EC) of 2 and 6 mS/cm. Three additional stations are sampled in Carquinez Strait and San Pablo Bay during high outflow when surface EC is less than 20mS/cm. Sampling is conducted by CDWR in conjunction with discrete water quality monitoring Three gear types are used for targeting different sized zooplankters: 1) a pump for sampling microzooplankton <1.0 mm long, including rotifers, copepod nauplii, and adult copepods of the genus Limnoithona, 2) a modified Clarke-Bumpus (CB) net for sampling mesozooplankton 0.5-3.0 mm long, including cladocerans, copepodids (immature copepods), and adult copepods, and 3) a macrozooplankton net for sampling zooplankton 1-20 mm long, including mysid shrimp. Samples are fixed in formalin and transported to the CDFG Laboratory in Stockton for processing.

# load zoop sites
# load("../DJFMP_zoop-master/data/cleaned_CB_data.rda")
load("../DJFMP_zoop-master/data/stations_sf.rda") # stations_sf


# MAKE SPATIAL & ADD POLYGONS------------------------------------------------------------

# stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), remove = F, crs=4326)

# make data into sf layers
below_fp_sf <- below_fp %>% st_as_sf(coords = c("Long","Lat"), remove = F, crs=4326)
above_fp_sf <- above_fp %>% st_as_sf(coords = c("Long","Lat"), remove = F, crs=4326)
outlets_sf <- outlets %>% st_as_sf(coords = c("Long","Lat"), remove = F, crs=4326)


#Add shape file of legal delta & transform to be same crs (coord ref system as point data)
delta <- readOGR("../../GIS_maps/Legal Delta shp/Legal_Delta.shp")
delta <- st_as_sf(delta) #change delta shape file to be an sf object
delta <- st_transform(delta, st_crs(sites_sf)) # transform coordinate reference system to match the collecion sites (sites_sf)

#Add shape file of Yolo Bypass & transform to be same crs (coord ref system as point data)
yol_shp <- readOGR("../../GIS_maps/Yolo FP shp/Yolo_bypass.shp")
yol_shp <- st_as_sf(yol_shp) #change delta shape file to be an sf object
yol_shp <- st_transform(yol_shp, st_crs(sites_sf)) # transform coordinate reference system to match the collecion sites (sites_sf)

#Add shape file of Cosumnes floodplain & transform to be same crs (coord ref system as point data)
cos_shp <- readOGR("../../GIS_maps/Cosumnes FP/Analysis_areas3.shp")
cos_shp <- st_as_sf(cos_shp) #change delta shape file to be an sf object
cos_shp <- st_transform(cos_shp, st_crs(sites_sf)) # transform coordinate reference system to match the collecion sites (sites_sf)

##--------------------RYAN PEEK'S RIVER LINES --------------------------------

load('../DJFMP_zoop-master/data_output/delta_crop_sf.rda')
delta_crop <- st_transform(delta_crop, st_crs(sites_sf)) # transform coordinate reference system to match the collecion sites (sites_sf)
mapview(delta_crop, col.regions="steelblue", color="darkblue", alpha.regions=0.8, legend=F, lwd=.5)

# load('../DJFMP_zoop-master/data/cleaned_CB_data.rda') 
# mapview(stations_sf, col.regions="steelblue", color="darkblue", alpha.regions=0.8, legend=F, lwd=.5) 

# Preview Mapview Map -----------------------------------------------------

# preview map
# mapview(delta_crop, color="steelblue2", alpha=0.7, lwd=1, legend=F) + 
#   mapview(yol_shp, col.regions="coral", legend=F, lwd=1.5) + 
#   mapview(cos_shp, col.regions="black", legend=F, lwd=0.5)+
#   mapview(delta, col.regions="grey", alpha.regions=0.1, legend=F) + 
#   mapview(sites_sf, col.regions="orange", cex=5, lwd=1, legend=F) +
#   mapview(stations_sf, color="gray", alpha.regions=.1, legend=F) 


# Create color palettes for map ------------ 

above_cols <- colorFactor(palette = c( "turquoise", "mediumpurple2"), 
                          levels = unique(above_fp_sf$Region_subdivided5))
below_cols <- colorFactor(palette = c("goldenrod2", "skyblue4", "darkorange1", "firebrick2"), 
                          levels = unique(below_fp_sf$Region_subdivided5))

# Make a simple black marker for floodplain outlets
FPIcon_r <- makeIcon(
  iconUrl = "http://icon-park.com/imagefiles/location_map_pin_red5.png",
  iconWidth = 20, iconHeight = 30,
  iconAnchorX = 8, iconAnchorY = 25  )

# green cosumnes outlet marker
FPIcon_b <- makeIcon(
  iconUrl = "http://icon-park.com/imagefiles/location_map_pin_navy_blue5.png",
  iconWidth = 20, iconHeight = 30,
  iconAnchorX = 8, iconAnchorY = 25  )

##---------MAP!!

map1 = leaflet() %>% addProviderTiles("CartoDB.Positron") %>% # or "CartoDB.PositronNoLabels"
  
  addPolygons(data = delta_crop, color = "black", opacity = 0.8,
              fillOpacity = 0.8, weight = 0.4) %>%

  addPolygons(data = delta, col= 'grey',
              opacity = 0.3, fillOpacity = 0.3, weight = 0.4) %>%
  
  addPolygons(data = cos_shp, col='black',  fillColor =  'royalblue',
              opacity = 0.6, fillOpacity = 0.7, weight = 0.6) %>%
  
  addPolygons(data = yol_shp, col='black',  fillColor = 'red',
              opacity = 0.6, fillOpacity = 0.4, weight = 0.6) %>%
  
  addCircleMarkers(data = above_fp_sf, 
                   #radius = sites_sf$n_fish/2,
                   radius = 3,
                   opacity = 1,
                   weight = 1.5, 
                   fillOpacity = 0.95,
                   color = "black",
                   fillColor = ~above_cols(Region_subdivided5),
                   popup = above_fp_sf$Site.Name) %>%
  
  addCircleMarkers(data = below_fp_sf, 
                   radius = 10,
                   opacity = 1,
                   weight = 0.7, 
                   fillOpacity = 0.95,
                   color = "black",
                   fillColor = ~below_cols(Region_subdivided5),
                   popup = below_fp_sf$Site.Name) %>%
  
  addMarkers(data = filter(outlets_sf, Site.Name == "Cosumnes outlet"),
             icon = FPIcon_b) %>%

  addMarkers(data = filter(outlets_sf, Site.Name == "Yolo Bypass outlet"),
             icon = FPIcon_r) %>% #,
             #label = "Yolo Bypass  ",
             #labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "bottom")) %>%
   
  # addLegend(position = "topright",
  #           colors = c("darkslategray4", "turquoise", "firebrick2", "forestgreen", "darkorange1", "goldenrod2"),
  #           labels = c("Lower Sacramento", "Lower San Joaquin", 
  #                      "Below Yolo", "Below Cosumnes", "West Delta", "Chipps Island")) %>%
  
  addLegend(position = "topright",
            colors = c("white","white","white","white","white","white","white","white","white","white"),#c("#528B8B", "turquoise", "firebrick2", "forestgreen", "darkorange1", "goldenrod2"),
            labels = c("Lower Sacramento", "Lower San Joaquin", 
                       "Below Yolo", "Below Cosumnes", "West Delta", "Chipps Island",
                       "Yolo Bypass outlet","", "Cosumnes outlet", "")) %>%
  
  setView(-121.7, 38.2, zoom = 9.4) %>%
  
  addScaleBar(position = c("bottomleft")); map1 #add scale bar to the map
