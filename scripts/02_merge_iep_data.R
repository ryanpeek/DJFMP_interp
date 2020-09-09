# 02_merge_data

# merge the IEP xlsx data with the IEP csv data (adding SHR and STTD sites)

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(glue)
library(fs)
library(sf)
library(lubridate)
library(janitor)


# GET DATASETS ---------------------------------------------------------

# CB Zoop data (1972-2019)
load("data/iep_cleaned_CB_data_2019.rda") # data
names(cb_wide)

# no site info for these?
# NZEZ2          138
# NZEZ2SJR        63
# NZEZ6          114
# NZEZ6SJR        36

# drop for now
cb_wide <- cb_wide %>% filter(!is.na(lat))

# Zoop IEP SHR/STTD data (2013-2018)
load("data/DWR_zoop_SHR_STTD_2018.rda")
names(clad_zoop)


# FILTER & TIDY TO DATA OF INTEREST ----------------------------------------------

cb_trim <- cb_wide %>% filter(year>2012, year<2019, month %in% c(1:5)) %>% 
  select(station_code=station_nz, year, month, date=sample_date, core, 
         region, location, lat, lon, cpue_bosmina, cpue_daphnia,
         cpue_allcladocera)

# gather species data up into one col
cb_trim <- cb_trim %>% 
  pivot_longer(cols = starts_with("cpue_"), names_prefix="cpue_", names_to="taxa_id", values_to="cpue")

# now the SHR/STTD data
clad_trim <- clad_zoop %>% filter(wy>2012, year<2019, month %in% c(1:5)) %>% 
  select(station_code, year=wy, month, date, location=station_name, lat, lon, cpue, classification:organism) %>% 
  mutate(taxa_id=glue("{classification}_{organism}")) %>% 
  select(-classification, -organism)


# COMBINE! ----------------------------------------------------------------

# can check that all records are unique
anti_join(clad_trim, cb_trim) %>% dim # equal to all rows in clad_trim
anti_join(cb_trim, clad_trim) %>% dim # equal to all rows in cb_trim

# combine two datasets so can use one single one
zoop_comb <- bind_rows(clad_trim, cb_trim)

# MAKE SPATIAL ------------------------------------------------------------

zoop_comb_sf <- st_as_sf(zoop_comb, coords = c("lon", "lat"), remove = F, crs=4326)

# take log cpue
zoop_comb_sf <- zoop_comb_sf %>% 
  mutate(cpue_log = log(cpue + 1))


# Some Plots ---------------------------------------------------------------


library(mapview)
mapviewOptions(fgb=FALSE, platform = "leaflet")
mapview(zoop_comb_sf[zoop_comb_sf$year==2014,], zcol="cpue_log")

library(tmap)
#tmap_mode("view")
tmap_mode("plot")
tm_shape(zoop_comb_sf) + 
  tm_bubbles(col="cpue_log") +
  tm_facets(by = "year")



# Save out ----------------------------------------------------------------

save(zoop_comb_sf, file = "data_output/zoop_combined_sf_2013-2018.rda")
