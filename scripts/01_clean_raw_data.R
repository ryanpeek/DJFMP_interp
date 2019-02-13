# 01_clean_raw_data

# this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/
# more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON
# The Zooplankton Study currently samples 19 stations monthly, including 17 fixed stations and 2 floating entrapment zone stations located at bottom electrical conductivity (EC) of 2 and 6 mS/cm. Three additional stations are sampled in Carquinez Strait and San Pablo Bay during high outflow when surface EC is less than 20mS/cm. Sampling is conducted by CDWR in conjunction with discrete water quality monitoring Three gear types are used for targeting different sized zooplankters: 1) a pump for sampling microzooplankton <1.0 mm long, including rotifers, copepod nauplii, and adult copepods of the genus Limnoithona, 2) a modified Clarke-Bumpus (CB) net for sampling mesozooplankton 0.5-3.0 mm long, including cladocerans, copepodids (immature copepods), and adult copepods, and 3) a macrozooplankton net for sampling zooplankton 1-20 mm long, including mysid shrimp. Samples are fixed in formalin and transported to the CDFG Laboratory in Stockton for processing.

library(tidyverse)
library(readxl)
library(sf)
library(lubridate)


# GET DATA ----------------------------------------------------------------

# see the sheets
excel_sheets("data/1972-2017CBMatrix.xlsx")

# read in data
cbmatrix <- read_xlsx("data/1972-2017CBMatrix.xlsx", sheet = 6)
str(cbmatrix)

# Taxa lookup
taxa <- read_xlsx("data/1972-2017CBMatrix.xlsx", sheet = 4)

# station locations
stations <- read_xlsx("data/1972-2017CBMatrix.xlsx", 
                      sheet = 2, skip = 2) %>% 
  mutate(lat = lat_deg + lat_min/60 + lat_sec/3600,
         lon = lon_deg + lon_min/60 + lon_sec/3600) %>% 
  select(-starts_with("lat_"), -starts_with("lon_")) %>% 
  filter(!is.na(lat)) %>% 
  mutate(lon = lon*-1)

# MAKE SPATIAL ------------------------------------------------------------

stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), remove = F, crs=4326)

library(mapview)
mapview(stations_sf, zcol="Current")


# MAKE WIDE ---------------------------------------------------------------
# make data long not wide
# names(cbmatrix)
# cb_dat <- cbmatrix %>% 
#   gather(key = taxa, value= measurement, -c(SurveyCode:CBVolume))

# FORMAT DATA -------------------------------------------------------------

cb_wide <- cbmatrix
cb_wide$Date <- as.Date(cb_wide$Date)
cb_wide$MM <- lubridate::month(cb_wide$Date) # add month

#class(cb_wide$Date)

# CHECK DATA FOR MISSINGNESS ----------------------------------------------

library(naniar)
#gg_miss_fct(cb_wide, Date)
gg_miss_var(cb_wide, "Date", show_pct = T)
gg_miss_var(cb_wide, "ALLCLADOCERA")
range(cb_wide$ALLCLADOCERA)

# cb_wide %>% group_by(Station) %>% tally() %>% View()

# LOG TRANSFORM -----------------------------------------------------------

# log transform for scaling:
cb_wide$ALLCLADOCERA_log <- log(cb_wide$ALLCLADOCERA+1)
hist(cb_wide$ALLCLADOCERA_log, col="maroon") # check range
range(cb_wide$ALLCLADOCERA_log) # check range
# summary(cb_wide)

# SAVE OUT ----------------------------------------------------------------

save(stations_sf, file= "data/stations_sf.rda")
save(cb_wide, file="data/cleaned_CB_data.rda")

