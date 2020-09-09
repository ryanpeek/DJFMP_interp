# Make Figure:

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(ggspatial)

# LOAD DATA ---------------------------------------------------------------

## this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
## more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON

# load("data/stations_sf.rda")
# load("data/cleaned_CB_data.rda") # Clarke-Bumpus (CB) sampled mesozooplankton
# 
# # select columns of interest, here: allcladocera
# cb_clad <- cb_wide %>% 
#   dplyr::select(survey_code:cb_volume, mm, allcladocera, allcladocera_log, -core)

options(scipen=12)

level_key <- c(`1` = "Winter (Jan-Feb)", `2` = "Winter (Jan-Feb)", `3` = "Early Spring (Mar-Apr)", `4` = "Early Spring (Mar-Apr)", `5` = "Late Spring (May-Jun)", `6` = "Late Spring (May-Jun)")

## Get added data:
dwr <- read_csv("data/DWR_invert_SHR_STTD_1998_2017.csv") %>% 
  mutate(Date=mdy(Date)) %>% 
  filter(Year>2013, Month>=1, Month<7, Order=="Cladocera") %>% 
  select(Date, Year, Month, Station.Code, Phylum:Family, CPUE) %>% 
  mutate("season"=recode_factor(Month, !!!level_key)) %>% rename("station"=Station.Code, "year"=Year) %>% 
  group_by(station, year, season) %>% complete(nesting(station), season, year=2014:2017) %>% 
  summarize(allclad_mean = mean(CPUE),
            allclad_sum = sum(CPUE),
            allcladocera_log = log(allclad_mean)) %>% 
  mutate(allcladocera_log = replace_na(allcladocera_log, 0),
         lon=if_else(station=="SHR", -121.5281, -121.643),
         lat=if_else(station=="STTD", 38.35346, 38.53244),
         location=if_else(station=="STTD", "Yolo Bypass - Screw Trap at Toe Drain", "Sacramento River at Sherwood Harbor"))


# add spatial info:
# https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/Environmental-Services/Interagency-Ecological-Program/Files/Data-Portal/DriftInvert_Metadata_02_16_2019.pdf?la=en&hash=23E17EF72D97A5AE2AE1B1B37D11E6650237098C
#  STTD Yolo Bypass - Screw Trap at Toe Drain= c(-121.643, 38.35346)
#  D    M  S       # to convert (D + M/60 + S/3600)=Dec Deg
#  38 + 21/60 + 12.46/3600 = 38.35346
#  -121 + -38/60 + -34.71/3600 = -121.643

#  SHR Sacramento River at Sherwood Harbor = c(-121.5281, 38.53244)
#  D    M  S
#  38 + 31/60 + 56.77/3600
#  -121 + -31/60 + -41.1/3600 
  

# GET SPATIAL DATA --------------------------------------------------------

# Read in shapefile and transform polygon
# delta_sf <- st_read("data/Bay_delta_selection.shp") %>% st_transform(crs=4326)

# crop to region of interest...warning is ok!
# crop_box <- st_bbox(c(xmin = -122.25, xmax = -121.22, ymax = 38.48, ymin = 37.77), crs = st_crs(4326))

# crop delta
# delta_crop <- st_crop(delta_sf, crop_box)
# stations_crop <- st_crop(stations_sf, crop_box) # warning is ok!

# JOIN SPATIAL STATIONS WITH DATA -----------------------------------------

# Join with station info for lat longs
# cb_clad_crop <- dplyr::inner_join(cb_clad, stations_crop, by=c("station"))

# make into sf spatial object:
# cb_clad_crop <- st_as_sf(cb_clad_crop, coords = c("lon","lat"), crs=4326, remove=FALSE)

# FILTER TO 2014-2018 ONLY ------------------------------------------------

# need to filter data to 2014 through 2018, and Jan-May
# clad_filt <- cb_clad_crop %>% filter(year>2013, mm>=1, mm<=6)

# rm extraneous stuff:
# rm(cb_wide, cb_clad, delta_sf, stations_sf)
# save(clad_filt, stations_crop, delta_crop, file = "data_output/clad_iep_2014-2018.rda")


# GROUP DATA --------------------------------------------------------------

load("data_output/clad_iep_2014-2018.rda")

# need to group the data into seasons:
level_key <- c(`1` = "Winter (Jan-Feb)", `2` = "Winter (Jan-Feb)", `3` = "Early Spring (Mar-Apr)", `4` = "Early Spring (Mar-Apr)", `5` = "Late Spring (May-Jun)", `6` = "Late Spring (May-Jun)")

# recode months to summarize
clad_filt2 <- clad_filt %>% 
  select(survey_code, year, date, station, dwr_station, region:allclad_cut) %>% 
  mutate("season"=recode_factor(mm, !!!level_key))
  
# group and summarize by season, station and year
clad_filt3 <- clad_filt2 %>% group_by(station, year, season) %>% 
  summarize(allclad_mean = mean(allcladocera),
            allclad_sum = sum(allcladocera),
            allcladocera_log = log(allclad_mean)) %>% 
  st_join(stations_crop, by=c("station"="station")) %>% rename(station=station.x) %>% 
  select(-station.y)

# add a cut number to the table based on zoop numbers:
clad_filt3 <- clad_filt3 %>% 
  mutate(allcladocera_log = replace(allcladocera_log, !is.finite(allcladocera_log), 0), # replace -Inf w 0
         # replace negative values
         allcladocera_log = replace(allcladocera_log, allcladocera_log<0, .1),
         allclad_cut = as.integer(Hmisc::cut2(allcladocera_log, g=4))) # make breaks

# join with DWR data
clad_filt4 <- bind_rows(clad_filt3[,c(1:6,10, 13,14)], dwr) %>% data.frame() %>% select(-geometry) %>% 
  st_as_sf(coords = c("lon","lat"), crs=4326, remove=FALSE) %>% 
  mutate(allclad_cut = as.integer(Hmisc::cut2(allcladocera_log, g=5))*.8) # make breaks

#save(clad_filt4, file = "data_output/clad_filt_2014_2018.rda")


# Quick Facet -------------------------------------------------------------

# for fancy scale bar & North Arrow
#annotation_scale(location = "bl", height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
#annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),width = unit(0.8, "cm"), pad_y = unit(0.6, "cm"),style = north_arrow_fancy_orienteering(text_size = 8), which_north = "true") +

library(ggtext)
library(ggspatial)
library(basemapR) # https://github.com/Chrisjb/basemapR

### NOTES: Also, could you add 2018 to Fig 7 and add units to the caption please?  If you think that adding in 2018 makes the fig too busy/big I'm wondering if we could plot the mean abundance per station for 2014 and 2015 and call those two years "2014-15" to parallel the fish plots. Thoughts?

# MAP
ggplot() +
  # basemaps, see: #positron #hydda #voyager #mapnik
  #base_map(bbox = st_bbox(clad_filt4), basemap = 'hydda', increase_zoom = 3) + 
  theme_bw(base_family = "Roboto Condensed") + 
  # plot delta
  geom_sf(data=delta_crop, fill="steelblue", color="steelblue", alpha=0.7, inherit.aes=F) +
  # plot sites
  geom_sf(data=clad_filt4, pch=21, color="gray30", fill="gray80", alpha=0.8, size=1) +
  # plot clad data
  geom_sf(data=clad_filt4, aes(fill=allcladocera_log, size=allclad_cut), 
          pch=21, color="gray20", alpha=0.9) +
  # plot scale: expression(paste("line1 \n line2 a" [b]))
  scale_fill_viridis_c(name=ggtext::element_markdown("Abundance (no. indiv m^sq^)"), option = "A", 
                       breaks=c(0,2,4,6,8,10,12,14), 
                       labels=c("0","7","55","400","3,000","22,000","163,000","1,203,000"),
                       na.value = "transparent", limits=c(0,14), 
                       guide=guide_colorbar(draw.ulim = F, draw.llim = F)) +
  guides(size=FALSE) +
  labs(title = paste0("All Cladocerans: 2014-2018"), x="", y="",
       caption = "Data Source: CDFW IEP Zooplankton Clarke-Bumpus, https://www.wildlife.ca.gov/Conservation/Delta/Zooplankton-Study") +
  ggplot2::coord_sf(label_axes = "E", crs=4326) + #xlim = c(-122.25, -121.35), ylim=c(38.6, 37.85),
  facet_grid(year~season) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        strip.background=element_rect(fill="#5D729D"),
        strip.text=element_textbox(size=11, color = "white", box.color = "#5D729D" ))

# save
ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_faceted_A_revised.png", width = 8.5, height = 11, dpi=300, units = "in", type="cairo") # may need to add type="cairo"

ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_faceted_A_revised.pdf", width = 8.5, height = 11, dpi=300, units = "in", device=cairo_pdf)

ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_facet_D.png", width = 11, height = 8.5, dpi=300, units = "in")
#ggsave(filename = "figures/2014_2018_clad_zoop_points_jan_jun_seasonal_facet.pdf", width = 11, height = 8.5, dpi=300, units = "in")

# SETUP AND MAP ----------------------------------------------------------


# Define the legend scale bar with the min/max abundance across all data you are going to show (otherwise the scale bar changes between time steps)
YY <- 2018
MM <- 5
min_obs <- 0 # log transformed
max_obs <- 13 # log transformed


# filter to specific year or years
cb_yr <- cb_clad_crop %>% filter(year==YY, mm==MM)
#cb_yr1805 <- cb_clad_crop %>% filter(year==YY, mm==MM)

# quick map
mapview(cb_yr, zcol="allcladocera_log", cex="allcladocera_log", lwd=1.5,
        layer.name=paste0(YY, "-", month.abb[MM],"\nAll Cladocera (log)")) +
# add all stations
mapview(stations_crop, col.regions="white", lwd=0.7, cex=2, alpha=0.7, legend=FALSE)

# BUILD MANUAL WEBMAP -----------------------------------------------------

col_pal <- colorNumeric(viridis(64, option = "B"), c(min_obs, max_obs), na.color = NA)

leaflet() %>%
  setView(lng = -121.7961, lat=38.06166, zoom=11) %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenBW") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group="ESRI Canvas") %>%
  
  # point layer
  addCircleMarkers(data=cb_yr, ~lon, ~lat, group=paste0(YY, "-",MM),
                   radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
                   weight= 1, fillOpacity=0.9, stroke=TRUE,
                   popup = paste0(
                     "All Cladocerans = ", cb_yr$allcladocera,
                     "<br> All Cladoceran (log) = ", cb_yr$allcladocera_log,
                     "<br> Station: ", cb_yr$station,
                     "<br> Lon: ", cb_yr$lon,
                     "<br> Lat: ", cb_yr$lat,
                     "<br> Region: ", cb_yr$region,
                     "<br> Temperature: ", cb_yr$temperature)) %>%
  
  # all sites
  addCircleMarkers(data=stations_crop, ~lon, ~lat, group="Stations",
                   weight= 0.8, fill = T, fillOpacity=0.7, stroke=TRUE,
                   fillColor = "skyblue", color="black", radius=2.5,
                   popup = paste0(
                     "Station = ", stations_crop$station,
                     "<br> Current = ", stations_crop$current,
                     "<br> Location: ", stations_crop$location,
                     "<br> Yr Start: ", stations_crop$year_start,
                     "<br> Yr End: ", stations_crop$year_end)) %>%

  
# addCircleMarkers(data=cb_yr1405, ~lon, ~lat, group="May2014",
#                  radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                  weight= 1, fillOpacity=0.7, stroke=TRUE,
#                  popup = paste0(
#                    "All Cladocerans = ", cb_yr1405$allcladocera,
#                    "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                    "<br> Station: ", cb_yr1405$station,
#                    "<br> Lon: ", cb_yr1405$lon,
#                    "<br> Lat: ", cb_yr1405$lat,
#                    "<br> Region: ", cb_yr1405$region,
#                    "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   
#   addCircleMarkers(data=cb_yr1505, ~lon, ~lat, group="May2015",
#                    radius = ~allclad_cut+2, color = ~col_pal(allcladocera_log),
#                    weight= 1, fillOpacity=0.7, stroke=TRUE,
#                    popup = paste0(
#                      "All Cladocerans = ", cb_yr1405$allcladocera,
#                      "<br> All Cladoceran (log) = ", cb_yr1405$allcladocera_log,
#                      "<br> Station: ", cb_yr1405$station,
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
#                      "<br> Station: ", cb_yr1405$station,
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
#                      "<br> Station: ", cb_yr1405$station,
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
#                      "<br> Station: ", cb_yr1405$station,
#                      "<br> Lon: ", cb_yr1405$lon,
#                      "<br> Lat: ", cb_yr1405$lat,
#                      "<br> Region: ", cb_yr1405$region,
#                      "<br> Temperature: ", cb_yr1405$temperature)) %>%
#   
  
  addLegend(pal = col_pal, values = c(min_obs, max_obs), title = "log(All Clad.)") %>%
  addFullscreenControl() %>% 
  #addControl(paste0(month.abb[MM], "-", YY), position = "bottomleft") %>% 
  addLayersControl(position = "topleft",
                   baseGroups = c("ESRI Canvas", "OpenBW",
                                  "Topo","ESRI Aerial"),
                   overlayGroups = c(paste0(YY, "-",MM), "Stations"),
                                     #"May2014","May2015","May2016","May2017","May2018"),
                   options = layersControlOptions(collapsed = T))



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

yr <- 2018
mon <- c(2:5)
min_obs <- 0 # log transformed
max_obs <- 13 # log transformed

# for radii
# Define circle radius and color
# Points$Ncat<-as.numeric(cut(Points$Total_N,breaks=c(0,500,50000,250000,10000000,1000000000), include.lowest=TRUE))
# PointsYear$NCat<-as.numeric(cut(PointsYear$Total_N,breaks=c(0,500,50000,250000,10000000,150000000), include.lowest=TRUE))
# Radii<-c(4,8,12,16,22)
# Points$Radius<-Radii[Points$Ncat]
# PointsYear$Radius<-Radii[PointsYear$NCat]


#ggmap(map1) +
ggplot() +
  annotation_scale(location = "bl", height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
  theme_bw(base_size = 9) +#base_family = "Roboto Condensed") +
  theme(panel.grid.major = element_line(color = "gray90")) +
  annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),width = unit(0.8, "cm"), 
                         pad_y = unit(0.6, "cm"),style = north_arrow_fancy_orienteering(text_size = 8),
                         which_north = "true") +
  scale_fill_viridis_c("log(Clad)", option = "A",
                       na.value = "transparent", limits=c(0,14)) +
  #scale_size_continuous(guide = TRUE) +
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5, inherit.aes=F) +
  geom_sf(data=cb_clad_crop[cb_clad_crop$year==yr & cb_clad_crop$mm %in% mon,], 
          aes(fill=allcladocera_log, size=allclad_cut), pch=21, color="gray20", alpha=0.9) +
  labs(title = paste0("All Cladocerans: ", yr, " ", month.abb[min(mon)], "-", month.abb[max(mon)]), x="", y="", 
       caption = "Data Source: CDFW IEP Zooplankton Clarke-Bumpus, https://www.wildlife.ca.gov/Conservation/Delta/Zooplankton-Study") +
  coord_sf(xlim = c(-122.25, -121.35), ylim=c(38.38, 37.85), crs=4326) +
  facet_wrap(.~mm)

# w background
# ggsave(filename = paste0("figures/all_cladocera_points_", yr, "_months_", min(mon), "_",max(mon), "_ggmap.pdf"),width = 10, height = 8, units = "in")

# no background
ggsave(filename = paste0("figures/all_cladocera_points_", yr, "_months_", min(mon), "_",max(mon), ".pdf"),width = 10, height = 8, units = "in")
