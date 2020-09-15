# 06 Make a Map


# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(sf)  
library(viridis)
library(grid)
library(USAboundaries)
library(ggspatial)
library(ggthemes)
library(mapview)
library(nhdplusTools)

# download package to interact with natural earth data
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")


# LOAD DATA ---------------------------------------------------------------

# load clad zoop sites: 
load("data_output/clad_filt_sf_2014_2018.rda")
clad_crop <- clad_crop %>% group_by(station, lat, lon) %>% distinct(.keep_all = TRUE)

# load delta
baydelta <- st_read("data/spatial/Bay_delta_selection.shp", quiet = F) %>%
  st_transform(crs=4326)

# load legal_deltas
legdelta <- st_read("data/spatial/Legal_Delta.shp") %>% st_transform(crs=4326)
#secondaryDelta <- st_read("data/spatial/PrimarySecondaryDelta.shp") %>% st_transform(crs=4326)

# load bypass
yolo <- st_read("data/spatial/Yolo_bypass.shp") %>% st_transform(4326)

# cos
#cos_fp_10yr <- st_read("data/spatial/Inun_10yr_fromHECRAS.shp") %>% st_transform(4326)
cos_cnty_fp <- st_read("data/spatial/Analysis_areas3.shp") %>% st_transform(4326)

# sample sites:
fishsites <- read_csv("data/djfmpsites_regions_ams.csv") %>% 
  # filter out exc sites:
  filter(!Region_subdivided5=="excl") %>% 
  # mutate(Notes = iconv(Notes, "latin1", "UTF-8"),
  #        Site.Name = iconv(Notes, "latin1", "UTF-8")) %>% 
  st_as_sf(coords=c("Long","Lat"), crs=4326, remove=F)

# crop to region of interest...warning is ok!
crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.65, ymin = 37.77), crs = st_crs(4326))

# crop delta
delta_crop <- st_crop(baydelta, crop_box)

# lakes
#load("data_output/lakes_ca_hydroshed.rda")
# crop to region of interest...warning is ok!
#crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.65, ymin = 37.77), crs = st_crs(4326))
#lakes_delta <- st_crop(lakes_ca, crop_box)
#save(lakes_delta, file = "data_output/lakes_delta_crop.rda")

load("data_output/lakes_delta_crop.rda")

# Basic Mapview Map -------------------------------------------------------

mapview(yolo, col.regions="coral", layer.name="Yolo Bypass") + 
  mapview(legdelta, col.regions="transparent", alpha.regions=0.1, color="darkblue", lwd=2, legend=FALSE) + 
  mapview(cos_cnty_fp, col.regions="cyan4", layer.name="Cosumnes Bypass") + 
  mapview(fishsites, zcol="Region_subdivided5", layer.name="Fish Sites") +
  mapview(clad_crop, col.regions="orange", layer.name="Zoop") +
  mapview(delta_crop, col.regions="steelblue", layer.name="Delta", legend=FALSE) +
  mapview(lakes_delta, col.regions="steelblue", layer.name="lakes", legend=FALSE)


# GET RIVER DATA ----------------------------------------------------------

# Rivers of US: 
# https://www.r-bloggers.com/how-to-map-geospatial-data-usa-rivers/
#url.river_data <- url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData") # or download first locally
#load(url.river_data)
# convert to sf:
#rivs_us <- st_as_sf(lines.rivers) %>% st_set_crs(4326)
#save(rivs_us, file="data/spatial/riverlines_usa.rda")

# load("data/spatial/riverlines_usa.rda")
# 
# rivs_ca <- rivs_us %>% filter(!(FEATURE %in% 
#                                    c("Shoreline","Shoreline Intermittent",
#                                      "Null", "Closure Line", "Apparent Limit")),
#                                STATE=="CA")


# GET SPECIFIC RIVER LINES ------------------------------------------------

library(nhdplusTools)

# get the COMID for stations to get downstream segments:
fish_coms <- fishsites %>% filter(Station_code %in% c("LP003E", "WD002W", "XC001N", "SJ083W", "GS010E", "SF014E")) %>% 
  split(.$Station_code) %>% # split by ID
  map(~discover_nhdplus_id(.x$geometry))

zoop_coms <- clad_crop %>% 
  filter(station %in% c("NZM10", "NZ032", "NZS42")) %>% 
  split(.$station) %>% # split by ID
  map(~discover_nhdplus_id(.x$geometry))
  
# now have a list of all COMIDs, check for duplicates
fish_coms %>% 
  purrr::map_lgl(~ length(.x)>1) %>% table() # all FALSE

# make a dataframe
fish_coms <- unlist(fish_coms) %>% as_tibble(rownames = "station") %>%
  rename(comid = value)

zoop_coms <- unlist(zoop_coms) %>% as_tibble(rownames = "station") %>% 
  rename(comid = value)

# combine into one:
site_coms <- bind_rows(fish_coms, zoop_coms)

# Make a list to use to download stream segs 
site_list <- map(site_coms$comid, ~list(featureSource = "comid", featureID=.x))

# Now get all stream segments from each fish point
mainstems_DS <- map(site_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="downstreamMain",
                                             #distance_km = 10,
                                             data_source = ""))

# mainstems_US
mainstems_US <- map(site_list, ~navigate_nldi(nldi_feature = .x,
                                              mode="upstreamMain",
                                              distance_km = 10,
                                              data_source = ""))

# make a single flat layer
mainstems_flat_ds <- mainstems_DS %>%
  set_names(., site_coms$station) %>%
  map2(site_coms$station, ~mutate(.x, station=.y)) 

mainstems_flat_us <- mainstems_US %>%
  set_names(., site_coms$station) %>%
  map2(site_coms$station, ~mutate(.x, station=.y))

# bind together
mainstems_ds <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_ds) %>% 
  mutate(direction = "DS")

mainstems_us <- do.call(what = sf:::rbind.sf,
                        args = mainstems_flat_us) %>% 
  mutate(direction = "US")

# bind together all
mainstems <- bind_rows(mainstems_ds, mainstems_us) %>% 
  distinct(nhdplus_comid, .keep_all = TRUE)


# add to map!
mapview(cos_cnty_fp, col.regions="orange", layer.name="Cosumnes Bypass") + 
  mapview(legdelta, col.regions="transparent", alpha.regions=0.1, color="darkblue", lwd=2, legend=FALSE) + 
  mapview(fishsites, zcol="Region_subdivided5", layer.name="Fish Sites") +
  mapview(clad_crop, col.regions="orange", layer.name="Zoop") +
  mapview(delta_crop, col.regions="steelblue", layer.name="Delta", legend=FALSE) + 
  mapview(mainstems, color="dodgerblue", layer.name="Rivers", lwd=1.5)

# save rivers!
save(mainstems, file = "data_output/rivers_mainstems_selected.rda")


# GET CITY DATA -----------------------------------------------------------

#library(rnaturalearth)

# cities of ca
# cities_sf <- ne_download(scale = 10, 
#                          type = 'populated_places', 
#                          category = 'cultural', returnclass = "sf") %>% 
#   filter(ADM1NAME=="California")
# save(cities_sf, file = "data_output/cities_sf.rda")
load(file = "data_output/cities_sf.rda")
cities_sf <- cities_sf %>% filter(NAME %in% c("Stockton", "Modesto","Sacramento", "Vallejo"))


# GET COUNTY DATA ---------------------------------------------------------

library(USAboundaries)

# counties
counties_ca <- us_counties(states = c("CA"))

# get specific counties
counties_select <- counties_ca %>% filter(name %in% c("San Joaquin", "Stanislaus", "Alameda", "Contra Costa", "El Dorado", "Yolo", "Solano", "Sutter", "Napa", "Placer", "Sacramento", "Calaveras", "Amador"))

#rivs_sel <- st_intersection(counties_select, rivs_ca) 

# Preview Mapview Map -----------------------------------------------------

# preview map
mapview(yolo, col.regions="coral", color="coral", lwd=1.5, layer.name="Yolo Bypass") + 
  mapview(legdelta, col.regions="transparent", alpha.regions=0.1, color="darkblue", lwd=2, legend=FALSE) + 
  mapview(cos_cnty_fp, col.regions="cyan4", layer.name="Cosumnes Bypass") + 
  mapview(fishsites, zcol="Region_subdivided5", cex=4, lwd=1,pch=21, layer.name="Fish Sites") +
  mapview(cities_sf, col.regions="black", legend=F, lwd=0.5, cex=3)+
  #mapview(counties_select, lwd=1.4, color="gray", alpha.regions=.1, legend=F) +
  mapview(clad_crop, col.regions="orange", pch=15, cex=4, layer.name="Zoop Sites")+
  mapview(delta_crop, col.regions="steelblue", color="darkblue", alpha.regions=0.8, legend=F, lwd=.5) +
  mapview(lakes_delta, col.regions="steelblue", color="steelblue", alpha.regions=0.8, legend=F, lwd=.5) +
  mapview(mainstems, col.regions="steelblue", color="steelblue", alpha.regions=0.8, legend=F, lwd=2)


# Make a Map --------------------------------------------------------------

# library(ggdark)
# #invert_geom_defaults()
# 
# ggplot() + 
#   #annotation_map_tile(type="stamenbw") + 
#   layer_spatial(data=baydelta, fill="steelblue", color="steelblue", alpha=0.9, lwd=0.5) +
# #  geom_sf(data=baydelta, fill="steelblue", color="steelblue", alpha=0.9, lwd=0.5) +
#   geom_sf(data=legdelta, color="gray30", fill="transparent", lwd=.7, lty=1, alpha=0.3) +
#   geom_sf(data=mainstems, color="steelblue", alpha=0.9, lwd=0.9) +
#   geom_sf(data=yolo, fill="maroon", alpha=0.9, color="red4", lwd=.7) +
#   geom_sf(data=cos_cnty_fp, fill="gold", alpha=0.9, color="gold3", lwd=.7) +
#   geom_point(data=fishsites, aes(x=Long, y=Lat, fill=Region_subdivided5), pch=23, color="gray90", size=3, alpha=0.8)+
#   geom_sf(data=clad_crop, fill="white", color="black", pch=21, cex=2, alpha=0.9, lwd=0.5) +
#   theme_bw() +
#   # annotate("text", y=38.55 , x=-121.3, color="lightgreen", size=3.8, 
#   #          label="Lower \nSacramento") +
#   # annotate("text", y=38.2 , x=-121.23, color="gold", size=3.8,
#   #          label="Below \nCosumnes") +
#   # annotate("text", y=37.95 , x=-121.9, color="gold2", size=3.8, 
#   #          label="Chipps \nIsland") +
#   # annotate("text", y=38.2 , x=-121.85, color="red3", size=3.8, 
#   #          label="Below \nYolo") +
#   # annotate("text", y=37.85 , x=-121.15, color="turquoise2", size=3.8, 
#   #          label="Lower \nSan Joaquin") +
#   dark_theme_gray(base_size = 8) +
#   theme(legend.position = c(.87, .75),
#         legend.text = element_text(size=6)) +
#   labs(x="Lon.", y="Lat.", fill="Fish Sites")+
#   annotation_scale(location = "bl",style = "ticks", pad_y=unit(0.2, "cm")) +
#   coord_sf(ylim = c(37.6, 38.8), xlim = c(-122.5,-121))
#   #guides(fill=F)
# 
# ggsave(filename = "figures/ggmap_dark_sites_nolabels_v2.png", dpi=300, width = 8, height = 6, units="in")


# Make a tmap Map ---------------------------------------------------------

library(tmap)
tmap_mode("plot")
tmap_options(bg.color = "white", legend.text.color = "black")

tm_shape(baydelta, bbox = fishsites) + tm_polygons(col="steelblue", alpha=0.7, border.col = "steelblue", border.alpha = 0.9) +
  tm_shape(legdelta) + tm_polygons(col="gray", alpha=0.2, border.col = "gray40", border.alpha = 0.9) +
  tm_shape(yolo) + tm_polygons(col="coral", alpha=0.7, border.col = "coral",border.alpha = 0.9) +
  tm_shape(cos_cnty_fp) + tm_polygons(col="maroon", alpha=0.7, border.col = "maroon",border.alpha = 0.9) +
  tm_shape(mainstems) + tm_lines(col="steelblue", size=0.9) +
  tm_shape(fishsites) + tm_bubbles(shape=23, col = "Region_subdivided5", size = 0.5) + tm_legend(position=c("left","top"))+
  tm_shape(clad_crop) + tm_dots(col="white", size=0.25, title.size=4) +
  tm_layout(frame = FALSE) +
  tm_compass(type="4star", position=c("right","top"), text.color = "white")+
  tm_scale_bar(position = c(0.01,0.01), text.size = 0.7, text.color = "white")

tmap_save(filename = "figures/tmap_map_example.png", width = 8, height = 7, dpi=300, units="in")


# SAVE MAP STUFF ----------------------------------------------------------

save(baydelta, cities_sf, clad_crop, cos_cnty_fp, 
     delta_crop, lakes_delta, legdelta, mainstems,
     yolo, file = "data_output/final_map_components.rda")
