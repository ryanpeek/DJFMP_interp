# Make Final Map


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)  
library(viridis)
library(grid)
library(USAboundaries)
library(ggspatial)
library(ggmap)
library(ggthemes)
library(glue)
library(ggtext)


# LOAD DATA ---------------------------------------------------------------

load("data_output/final_map_components.rda")

mainstems <- st_set_crs(mainstems, 4326)
yolo <- st_set_crs(yolo, 4326)

# fish sites (created in Clean_up_gut_data.R, Box Sync/PROJECTS/DJFMP/sub-projects/DJFMP_guts
fish_df <- read.csv("data_output/guts.df.taxa_041720.csv") %>% 
  # fix non UTF8 characters
  mutate(Site.Name = iconv(Site.Name, "latin1", "ASCII", "'"),
         Notes = iconv(Notes, "latin1", "ASCII", "'"))

dist_df <- read_csv('data_output/DISTANCES_FROM_FP_KN.csv') %>% 
  mutate(Site.Name = iconv(Site.Name, "latin1", "ASCII", "'"))

sites_below_fp <- unique(dist_df$Site.Name)

outlets <- dist_df %>% filter(Distance_rkm ==0) %>% 
  rename(location=Site.Name, lat=Lat, lon=Long) %>% 
  mutate(image = c("http://icon-park.com/imagefiles/location_map_pin_navy_blue5.png",
                   "http://icon-park.com/imagefiles/location_map_pin_red5.png")) %>% 
  st_as_sf(coords = c("lon","lat"), remove = F, crs=4326)

# FISH sites
fishsites <- fish_df %>%
  filter(!Region_subdivided5 %in% c("excl", "Bay")) %>%
  group_by(Region_subdivided5, Station_code, Site.Name, Lat, Long) %>%
  summarize(N_fish = n()) %>%
  mutate(below_FP = case_when(
    Site.Name %in% sites_below_fp ~ 'y',
    TRUE ~ 'n')) %>% 
  rename(station=Station_code, location=Site.Name, region=Region_subdivided5,
         lat=Lat, lon=Long) %>% 
  ungroup() %>% 
  # refactor
  mutate(region=forcats::fct_relevel(region,  "Lower Sac", "Lower SJR"))

# Make into a spatial object
fishsites_sf <- fishsites %>% st_as_sf(coords = c("lon","lat"), remove = F, crs=4326)

# ZOOP sites
zoopsites <- clad_crop %>% 
  # drop cols
  select(-c(year, season, starts_with("allclad")))

# drop unused layers
rm(fish_df, fishsites)

# Setup Color Palettes and Markers ----------------------------------------


# assign color palette based on classifications:
colorPalettes <- data.frame(region=as_factor(unique(fishsites_sf$region)),
                            region_names  = c("Lower Sac", "Lower San Joaquin",
                                              "Chipps Island", "Below Cosumnes",
                                              "West Delta", "Below Yolo"),
                            color = I(c(
                              "turquoise", #lower sac
                              "mediumpurple2", #LSJR
                              "goldenrod2", #chipps
                              "skyblue4", #below cosum
                              "darkorange1", #WestDelta
                              "firebrick2" #yolo
                            )))

levels(colorPalettes$region)

# join with full dataset and get distinct set of lat/long sites
fishsites_sf2 <- left_join(fishsites_sf, colorPalettes, by="region") %>% 
  group_by(region, lat, lon) %>% distinct(.keep_all = TRUE) %>% 
  ungroup()

levels(fishsites_sf2$region)

# add a row number
fishsites_sf2 <- fishsites_sf2 %>% ungroup() %>% mutate(labelID = row_number())

# split into above and below
#fishsites_sf_us <- fishsites_sf2 %>% filter(below_FP=="n") %>% mutate(region=forcats::as_factor(region))
#fishsites_sf_ds <- fishsites_sf2 %>% filter(below_FP=="y") %>% mutate(region=forcats::as_factor(region))


# Setup Google Map Backgrounds ---------------------------------------------

ggbbox <- c(left = -122.25, right = -121.22, top = 38.65, bottom = 37.69)

# set the background map up
map1 <- get_map(location=ggbbox, crop = F,
                color="bw",
                maptype="terrain",
                source="google",
                zoom=10)


# check map?
#ggmap(map1)

# Make GGMAP Version ---------------------------------------------------------

# set background
SiteMap <- ggmap(map1) +
  coord_sf(xlim = c(-122.25, -121.15), ylim=c(38.8, 37.62), expand = FALSE)+
  # add north arrow and scale bar
  annotation_north_arrow(location = "bl",
                         height = unit(0.8, "cm"),
                         width = unit(0.8, "cm"), 
                         pad_y = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering(text_size = 8),
                         which_north = "true") +
  annotation_scale(location = "bl", 
                   height = unit(0.14, "cm"), pad_y = unit(0.2, "cm")) +
  
  # set up theme and base font sizes
  theme_bw(base_size = 9) +
  #guides(size=FALSE) + # turn off the size guide
  
  # set up scales
  scale_size_manual("Floodplain Location", values=c(3, 6), 
                    labels=c("n"="Above Floodplain", "y"="Below Floodplain"), 
                    guide = guide_legend(override.aes = list(alpha=1), order=1)) +
  
  scale_fill_manual(name="Sites", values=colorPalettes$color, 
                    guide = guide_legend(override.aes = list(pch=c(21),
                                                             fill=colorPalettes$color, 
                                                             size=c(3,3,6,6,6,6)), order=2)) +

  scale_color_manual("", values=c("darkseagreen1"), labels="Zooplankton Sites", 
                     guide = guide_legend(override.aes = list(alpha=1), order=2))+
    
  # add spatial Layers:
  # LEGAL DELTA
  geom_sf(data=legdelta, fill="gray10", color="gray10", lwd=0.3, alpha=0.2, inherit.aes = F) +
  
  # FLOODPLAINS
  geom_sf(data=cos_cnty_fp, fill="skyblue2", alpha=0.7, lwd=0.2, inherit.aes = F) +
  geom_sf(data=yolo, fill="firebrick2", alpha=0.5, lwd=0.2, inherit.aes = F) +
  
  # DELTA
  geom_sf(data=delta_crop, fill="steelblue", color="steelblue", lwd=0.2, alpha=0.5, inherit.aes=F) +
  
  # RIVERS
  geom_sf(data=mainstems, fill="steelblue", color="steelblue", lwd=0.5, alpha=0.8, inherit.aes = F) +
  
  # FISH SITES
  geom_sf(data=fishsites_sf2, aes(fill=region, size=as.factor(below_FP)), pch=21, show.legend = T, inherit.aes = F) +

  # ZOOP SITES
  geom_sf(data=zoopsites, pch=23, fill="gray20", aes(color="zoop"), size=3, alpha=0.8) +
  
  # Outlets
  ggimage::geom_image(data=outlets, aes(x=lon, y=lat, image=image), size=0.03, inherit.aes = F, nudge_y = .001) +
  
  # if you want to label with numbers
  #geom_spatial_text_repel(data=fishsites_sf2, aes(label=labelID)) +
    
  # formatting
  labs(x="", y="") +
  theme(#axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks = element_blank(),
        legend.title = element_markdown(),
        panel.grid.major = element_line(colour = 'transparent'))

# plot it
SiteMap

# SAVE!
ggsave(filename="figures/Site_map_all_fish_zoop_sites.pdf", dpi=300, width=8, height = 11.5, units="in", device=cairo_pdf)

ggsave(filename="figures/Site_map_all_fish_zoop_sites.tiff", dpi=300, width=8, height = 11.5, units="in")


# ADD INSET ---------------------------------------------------------------

us <- USAboundaries::us_boundaries(type="state", resolution = "low") %>% 
  filter(!state_abbr %in% c("PR", "AK", "HI"))

ca <- USAboundaries::us_boundaries(type="state", states = "CA")

# make a box around rivers (a grid with an n=1) for inset
ca_box <- st_make_grid(cities_sf, n = 1) #%>% st_centroid()

# Inset map: US
(p2 <- ggplot() + 
    geom_sf(data = us, colour = "grey10", fill = "white", alpha=0.4) +
    coord_sf() +
    theme_minimal() + 
    geom_sf(data=ca_box, color="red3", fill=NA, size=1, alpha=0.7) +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(color = "black", fill="white"),
          #plot.background = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0, 0, 0 ,0), "mm")))


# SAVE FINAL MAP ----------------------------------------------------------

# add diff libraries
library(grid)
library(gridExtra)

# to save:
#start from this first (may need to change to just "pdf" instead of "cairo_pdf")
png(filename = "figures/Site_map_all_fish_zoop_sites_w_inset.png", width = 11, height = 8, units = "in", res = 300)

cairo_pdf(filename = "figures/Site_map_all_fish_zoop_sites_w_inset.pdf", width = 11, height = 8)

# to just view, start from below here
grid.newpage()
mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
insetmap <- viewport(width = 0.20, height = 0.21, x = 0.29, y = 0.89) # inset
print(SiteMap, vp = mainmap) 
print(p2, vp = insetmap)

# make sure to run this if saving out as pdf/png
dev.off()

