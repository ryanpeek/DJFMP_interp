# 06 Make a Map


# LOAD LIBRARIES ----------------------------------------------------------

#library(tmap)    # for static and interactive maps
library(dplyr)
library(readr)
library(sf)  
library(ggplot2)
library(viridis)
library(grid)
library(USAboundaries)
library(ggspatial)
library(ggthemes)
library(mapview)

# download package to interact with natural earth data
#devtools::install_github("ropenscilabs/rnaturalearth")
#devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")
library(rnaturalearth)


# LOAD DATA ---------------------------------------------------------------

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

# load sites
load("data_output/clad_filt_2014_2018.rda")
clad_filt <- clad_filt4 %>% group_by(station, lat, lon) %>% distinct()


# Basic Mapview Map -------------------------------------------------------

mapview(yolo, col.regions="coral") + 
  mapview(legdelta, col.regions="transparent", alpha.regions=0.1, color="darkblue", lwd=2) + 
  mapview(cos_cnty_fp, col.regions="cyan4") + 
  mapview(fishsites, zcol="Region_subdivided5")


# LOAD OTHER DATA ---------------------------------------------------------

# Rivers of US: 
# https://www.r-bloggers.com/how-to-map-geospatial-data-usa-rivers/
# url.river_data <- url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData") # or download first locally
# load(url.river_data)
# convert to sf:
# rivs_us <- st_as_sf(lines.rivers) %>% st_set_crs(4326)
# save(rivs_us, file="data/spatial/riverlines_usa.rda")

load("data/spatial/riverlines_usa.rda")
load("data_output/delta_crop_sf.rda")

rivs_ca <- rivs_us %>% filter(!(FEATURE %in% 
                                  c("Shoreline","Shoreline Intermittent",
                                    "Null", "Closure Line", "Apparent Limit")),
                              STATE=="CA")

# cities of ca
# cities_sf <- ne_download(scale = 10, 
#                          type = 'populated_places', 
#                          category = 'cultural', returnclass = "sf") %>% 
#   filter(ADM1NAME=="California")
# save(cities_sf, file = "data_output/cities_sf.rda")
load(file = "data_output/cities_sf.rda")
cities_sf <- cities_sf %>% filter(NAME %in% c("Stockton", "Modesto","Sacramento", "Vallejo"))

# counties
counties_ca <- us_counties(states = c("CA"))

# get specific counties
counties_select <- counties_ca %>% filter(name %in% c("San Joaquin", "Stanislaus", "Alameda", "Contra Costa", "El Dorado", "Yolo", "Solano", "Sutter", "Napa", "Placer", "Sacramento", "Calaveras", "Amador"))

rivs_sel <- st_intersection(counties_select, rivs_ca) 

# load lakes
#lakes <- st_read(dsn="~/Downloads/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb")
#st_layers("~/Downloads/HydroLAKES_polys_v10.gdb/HydroLAKES_polys_v10.gdb", do_count=TRUE)

# select CA
#lakes_ca <- filter(lakes, Country=="United States of America") %>% st_transform(4326)
#save(lakes_ca, file = "data_output/lakes_ca_hydroshed.rda")
load("data_output/lakes_ca_hydroshed.rda")

# select counties of interest
lakes_co <- st_intersection(counties_select, lakes_ca) 

# Preview Mapview Map -----------------------------------------------------

# preview map
mapview(rivs_sel, color="steelblue2", alpha=0.7, lwd=1, legend=F) + 
  mapview(yolo, col.regions="coral", legend=F, lwd=1.5) + 
  mapview(cities_sf, col.regions="black", legend=F, lwd=0.5, cex=3)+
  mapview(legdelta, col.regions="transparent", alpha.regions=0.1, color="darkblue", lwd=2, legend=F) + 
  mapview(fishsites, col.regions="orange", cex=5, lwd=1, legend=F) +
  mapview(counties_select, lwd=1.4, color="gray", alpha.regions=.1, legend=F) +
  mapview(delta_crop, col.regions="steelblue", color="darkblue", alpha.regions=0.8, legend=F, lwd=.5) +
  mapview(lakes_co, col.regions="steelblue", color="steelblue", alpha.regions=0.8, legend=F, lwd=.5)


# Make a Map --------------------------------------------------------------

library(ggdark)
#invert_geom_defaults()

ggplot() + 
  #annotation_map_tile(type="stamenbw") + 
  layer_spatial(data=baydelta, fill="steelblue", color="steelblue", alpha=0.9, lwd=0.5) +
#  geom_sf(data=baydelta, fill="steelblue", color="steelblue", alpha=0.9, lwd=0.5) +
  geom_sf(data=legdelta, color="gray30", fill="transparent", lwd=.7, lty=1, alpha=0.3) +
  geom_sf(data=yolo, fill="maroon", alpha=0.9, color="red4", lwd=.7) +
  geom_sf(data=cos_cnty_fp, fill="gold", alpha=0.9, color="gold3", lwd=.7) +
  geom_point(data=fishsites, aes(x=Long, y=Lat, fill=Region_subdivided5), pch=23, color="gray90", size=3, alpha=0.8)+
  geom_sf(data=clad_filt, fill="white", color="black", pch=21, cex=2, alpha=0.9, lwd=0.5) +
  theme_bw() +
  # annotate("text", y=38.55 , x=-121.3, color="lightgreen", size=3.8, 
  #          label="Lower \nSacramento") +
  # annotate("text", y=38.2 , x=-121.23, color="gold", size=3.8,
  #          label="Below \nCosumnes") +
  # annotate("text", y=37.95 , x=-121.9, color="gold2", size=3.8, 
  #          label="Chipps \nIsland") +
  # annotate("text", y=38.2 , x=-121.85, color="red3", size=3.8, 
  #          label="Below \nYolo") +
  # annotate("text", y=37.85 , x=-121.15, color="turquoise2", size=3.8, 
  #          label="Lower \nSan Joaquin") +
  dark_theme_gray(base_size = 8) +
  theme(legend.position = c(.87, .75),
        legend.text = element_text(size=6)) +
  labs(x="Lon.", y="Lat.", fill="Fish Sites")+
  annotation_scale(location = "bl",style = "ticks", pad_y=unit(0.2, "cm")) +
  coord_sf(ylim = c(37.6, 38.8), xlim = c(-122.5,-121))
  #guides(fill=F)

ggsave(filename = "figures/ggmap_dark_sites_nolabels_v2.png", dpi=300, width = 8, height = 6, units="in")


# Make a tmap Map ---------------------------------------------------------

library(tmap)
tmap_mode("plot")
tmap_options(bg.color = "black", legend.text.color = "white")

tm_shape(baydelta, bbox = fishsites) + tm_polygons(col="steelblue", alpha=0.7, border.col = "steelblue", border.alpha = 0.9) +
  tm_shape(legdelta) + tm_polygons(col="gray", alpha=0.2, border.col = "gray40", border.alpha = 0.9) +
  tm_shape(yolo) + tm_polygons(col="coral", alpha=0.7, border.col = "coral",border.alpha = 0.9) +
  tm_shape(cos_cnty_fp) + tm_polygons(col="maroon", alpha=0.7, border.col = "maroon",border.alpha = 0.9) +
  tm_shape(fishsites) + tm_bubbles(shape=23, col = "Region_subdivided5", size = 0.5) + tm_legend(position=c("left","top"))+
  tm_shape(clad_filt) + tm_dots(col="white", size=0.25, title.size=4) +
  tm_layout(frame = FALSE) +
  tm_compass(type="4star", position=c("right","top"), text.color = "white")+
  tm_scale_bar(position = c(0.01,0.01), text.size = 0.7, text.color = "white")

tmap_save(filename = "figures/tmap_map_example.png", width = 8, height = 7, dpi=300, units="in")


# Nice Composite Map ------------------------------------------------------

library(tidyverse)
library(sf)
library(albersusa) # devtools::install_github('hrbrmstr/albersusa')
library(ggrepel)

usa_sf <-
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0

x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

ix <- usa_sf$name %in% c("New Hampshire", "Vermont", "Massachusetts")
usa_sf$nudge_x[ix] <- -1 * 0.15 * x_range
usa_sf$nudge_y[ix] <- 1 * 0.15 * y_range

ix <- usa_sf$name %in% c(
  "Massachusetts",
  "Rhode Island", "Connecticut", "New Jersey", "West Virginia",
  "Maryland", "Delaware", "District of Columbia"
)
usa_sf$nudge_x[ix] <- 1 * 0.2 * x_range
usa_sf$nudge_y[ix] <- -1 * 0.15 * y_range

ggplot(data = usa_sf) +
  geom_sf() +
  geom_text_repel(
    mapping = aes(
      x = COORDS_X,
      y = COORDS_Y,
      label = name
    ),
    nudge_x = usa_sf$nudge_x,
    nudge_y = usa_sf$nudge_y,
    size = 3,
    min.segment.length = 0,
    point.padding = NA,
    segment.color = "grey50"
  ) +
  coord_sf(crs = st_crs(usa_sf), datum = NA) +
  theme_void() +
  xlim(min(usa_sf$COORDS_X) * 1.1, max(usa_sf$COORDS_X) * 1.15)

