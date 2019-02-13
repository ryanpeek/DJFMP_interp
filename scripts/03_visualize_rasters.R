# Visualize



# PLOT IN GGPLOT ----------------------------------------------------------

library(ggspatial)

# load test rasters
load("data_output/pred_raster_clad_1977_06_rast.rda")
load("data_output/pred_raster_clad_1983_06_rast.rda")
load("data_output/pred_raster_clad_2015_06_rast.rda")
load("data_output/pred_raster_clad_2017_06_rast.rda")

# convert to df for ggplot
clad_1977_06_df <- as.data.frame(clad_1977_06_rast, xy=TRUE)
clad_1983_06_df <- as.data.frame(clad_1983_06_rast, xy=TRUE)
clad_2015_06_df <- as.data.frame(clad_2015_06_rast, xy=TRUE)
clad_2017_06_df <- as.data.frame(clad_2017_06_rast, xy=TRUE)

### PLOT IN MAPVIEW?
#sliderMap <- slideView(clad_1977_06_rast, clad_1983_06_rast)
#mapshot(sliderMap, url = "slider_comp_1977_1983.html")

# rastviewmap <- mapview(clad_1977_06_rast) + mapview(clad_1983_06_rast)
# (rast1 <- mapview(clad_1977_06_rast, layer.name="1977") + 
#     mapview(stations_crop, color="gray", col.regions="#008080") )
# 
# rast2 <- addMeasure(rastviewmap)

# make a bounding box
# bbox <- st_bbox(c(xmin=-122.12, xmax=-121.3812, ymax=38.2, ymin=37.9))

p1977<-ggplot() +
  annotation_map_tile(type = "stamenwatercolor") + # osm, stamenwatercolor
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5) +
  geom_raster(data = clad_1977_06_df,
              aes(x = x, y = y, fill = z)) + 
  geom_sf(data=stations_crop, pch=21, size=2, 
          color="gray20",fill="white", alpha=0.9) +
  scale_fill_viridis_c("log(Clad)", option = "A",
                       na.value = "transparent", limits=c(0,14)) + 
  coord_sf(xlim = c(-122.12, -121.3812), ylim=c(38.2, 37.88), crs=4326) +
  labs(title = "All Cladocerans: 1977-June", x="", y="") +
  annotation_scale(location = "bl") +
  theme_classic(base_size = 9) +
  annotation_north_arrow(location = "bl", width = unit(1, "cm"), 
                         pad_y = unit(0.7, "cm"),
                         which_north = "true")

p1977

p1983<-ggplot() +
  annotation_map_tile(type = "stamenwatercolor") +
  #annotation_map_tile(type = "osm", cachedir = system.file("rosm.cache", package = "ggspatial")) +
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5) +
  geom_raster(data = clad_1983_06_df,
              aes(x = x, y = y, fill = z)) + 
  geom_sf(data=stations_crop, pch=21, size=2, 
          color="gray20",fill="white", alpha=0.9) +
  scale_fill_viridis_c("log(Clad)", option = "A",
                       na.value = "transparent", limits=c(0,14)) + 
  coord_sf(xlim = c(-122.12, -121.3812), ylim=c(38.2, 37.88), crs=4326) + 
  labs(title="All Cladocerans: 1983-June", x="",y="") +
  annotation_scale(location = "bl") +
  theme_classic(base_size = 9) +
  annotation_north_arrow(location = "bl", width = unit(1, "cm"), 
                         pad_y = unit(0.7, "cm"),
                         which_north = "true")
p1983

p2015<-ggplot() +
  annotation_map_tile(type = "osm", zoom=12, cachedir = system.file("rosm.cache", package = "ggspatial")) +
  #annotation_map_tile(type = "stamenwatercolor") +
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5) +
  geom_raster(data = clad_2015_06_df,
              aes(x = x, y = y, fill = z)) + 
  geom_sf(data=cb_clad_crop[cb_clad_crop$Year==2015 & cb_clad_crop$MM==6,], pch=21, size=2, 
          color="gray20",fill="white", alpha=0.9) +
  scale_fill_viridis_c("log(Clad)", option="A",
                       na.value = "transparent", limits=c(0,14)) + 
  coord_sf(xlim = c(-122.12, -121.3812), ylim=c(38.2, 37.88), crs=4326) +
  labs(title="All Cladocerans: 2015-June", x="",y="")+
  annotation_scale(location = "bl") +
  theme_classic(base_size = 9) +
  annotation_north_arrow(location = "bl", width = unit(1, "cm"), 
                         pad_y = unit(0.7, "cm"),
                         which_north = "true")
p2015

p2017<-ggplot() +
  annotation_map_tile(type = "osm", zoom=12, cachedir = system.file("rosm.cache", package = "ggspatial")) +
  #annotation_map_tile(type = "osm", zoom=12) +
  geom_sf(data=delta_crop, fill="gray30", alpha=0.5) +
  geom_raster(data = clad_2017_06_df,
              aes(x = x, y = y, fill = z)) + 
  geom_sf(data=cb_clad_crop[cb_clad_crop$Year==2017 & cb_clad_crop$MM==6,], pch=21, size=2, 
          color="gray20",fill="white", alpha=0.9) +
  scale_fill_viridis_c("log(Clad)", option="A",
                       na.value = "transparent", limits=c(0,14)) + 
  coord_sf(xlim = c(-122.12, -121.3812), ylim=c(38.2, 37.88), crs=4326) + theme_classic(base_size = 9) +
  labs(title="All Cladocerans: 2017-June", x="",y="")+ 
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", width = unit(1, "cm"), 
                         pad_y = unit(0.7, "cm"),
                         which_north = "true")
# quartz() have to open this to avoid annoying "Error in grid.call()"
quartz(width = 8, height = 6)
p2017
dev.copy2pdf(file="./figures/all_clad_2017_select_stations.pdf", out.type = "pdf", width=8, height=6)
dev.off()

# try all 
quartz(width=10, height=8)
p1977 + p1983 + p2015 + p2017 + plot_layout(ncol=2, heights = c(1,1))
dev.copy2pdf(file="./figures/all_clad_1977-2017_select_stations.pdf", out.type = "pdf", width=10, height=10)
dev.off()

#devtools::install_github("thomasp85/patchwork")
library(patchwork)

quartz(width=8, height=6)
p1977 + p1983 + plot_layout(ncol = 1, heights = c(1, 1))
dev.copy2pdf(file="test_77-83.pdf")


# cowplot way?
library(cowplot)
plot_grid(p1977, p1983, nrow=2)
plot_grid(p2015, p2017, nrow=2)

ggsave(filename = "figures/comparison_clad_4_panel_no-bg.png", width = 11, height = 8, units = "in", dpi = 300)

