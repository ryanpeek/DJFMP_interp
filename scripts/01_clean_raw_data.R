# 01_clean_raw_data

# this data came from the CDFW IEP Zooplankton Data: ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-2019CBMatrix.xlsx

# more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON
# The Zooplankton Study currently samples 19 stations monthly, including 17 fixed stations and 2 floating entrapment zone stations located at bottom electrical conductivity (EC) of 2 and 6 mS/cm. Three additional stations are sampled in Carquinez Strait and San Pablo Bay during high outflow when surface EC is less than 20mS/cm. Sampling is conducted by CDWR in conjunction with discrete water quality monitoring Three gear types are used for targeting different sized zooplankters: 1) a pump for sampling microzooplankton <1.0 mm long, including rotifers, copepod nauplii, and adult copepods of the genus Limnoithona, 2) a modified Clarke-Bumpus (CB) net for sampling mesozooplankton 0.5-3.0 mm long, including cladocerans, copepodids (immature copepods), and adult copepods, and 3) a macrozooplankton net for sampling zooplankton 1-20 mm long, including mysid shrimp. Samples are fixed in formalin and transported to the CDFG Laboratory in Stockton for processing.

library(tidyverse)
library(readxl)
library(glue)
library(fs)
library(sf)
library(lubridate)
library(janitor)


# GET DATA ----------------------------------------------------------------

# download file for CB zoop sampling
# also see "MysidMatrix" and "Pump Matrix"
updateYr <- 2019
ftp_link <- glue("ftp://ftp.dfg.ca.gov/IEP_Zooplankton/1972-{updateYr}CBMatrix.xlsx")
(fn <- basename(ftp_link))
download.file(ftp_link, destfile = glue("data/{fn}"))

# set local link (make sure this folder exists on your RStudio proj)
loc_link <- glue("data/{fn}")
# list the sheets in the workbook
excel_sheets(path = loc_link)

# ZOOP DATA
cbmatrix <- read_xlsx(loc_link, sheet = 6) %>%  
  clean_names() %>% 
  # fix col formats:
  mutate_at(.vars = c("ec_bottom_pre_tow"), as.numeric)

str(cbmatrix)

# TAXA LOOKUP DATA
taxa <- read_xlsx(loc_link, sheet = 4, skip=1) %>% 
  # drop last line:
  slice(1:(n()-1)) %>% 
  clean_names()
  
# STATION LOCATION DATA
stations <- read_xlsx(loc_link,
                      sheet = 2, skip = 4,
                      col_names = c("Station","Core", "Current", "Location", 
                                    "lat_dd", "lat_min", "lat_sec", "lat_dec",
                                    "lon_dd", "lon_min", "lon_sec", "lon_dec",
                                    "Year_start", "Year_end")) %>% 
  clean_names() %>% 
  mutate(lat = lat_dd + lat_min/60 + lat_sec/3600,
         lon = lon_dd + lon_min/60 + lon_sec/3600,
         latDD = lat_dec, lonDD = lon_dec) %>% 
  select(-starts_with("lat_"), -starts_with("lon_")) %>% 
  filter(!is.na(lat)) %>% 
  mutate(lon = lon*-1)

# MAKE SPATIAL ------------------------------------------------------------

stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), remove = F, crs=4326)

# library(mapview)
# mapview(stations_sf, zcol="current")

# MAKE WIDE ---------------------------------------------------------------

# make data long not wide, but not necessary
# names(cbmatrix)
# cb_dat <- cbmatrix %>%
#   gather(key = taxa, value= measurement, -c(survey_code:cb_volume))

# FORMAT DATA -------------------------------------------------------------

cb_wide <- cbmatrix
cb_wide$month <- month(cb_wide$sample_date) # add month

# CHECK DATA FOR MISSINGNESS ----------------------------------------------

library(naniar)
# gg_miss_var(cb_wide, "sample_date", show_pct = T)
# gg_miss_var(cb_wide, "allcladocera")
summary(cb_wide$allcladocera)
range(cb_wide$allcladocera)

# count how many records by station
# cb_wide %>% group_by(station_nz) %>% tally()

# LOG TRANSFORM -----------------------------------------------------------

# log transform for scaling:
cb_wide <- cb_wide %>% 
  mutate(allcladocera_log = log(allcladocera+1))
# hist(cb_wide$allcladocera_log, col="maroon") # check range
range(cb_wide$allcladocera_log) # check range

# SAVE OUT ----------------------------------------------------------------

save(stations_sf, file= glue("data/iep_stations_sf_{updateYr}.rda"))
save(cb_wide, file=glue("data/iep_cleaned_CB_data_{updateYr}.rda"))

