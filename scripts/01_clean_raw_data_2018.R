# 01_clean_raw_data


# Overview ----------------------------------------------------------------

# this data came from the CDFW IEP Zooplankton Data
# more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON

# Data Now Lives here:
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.494.1

# The Zooplankton Study currently samples 19 stations monthly, including 17 fixed stations and 2 floating entrapment zone stations located at bottom electrical conductivity (EC) of 2 and 6 mS/cm. Three additional stations are sampled in Carquinez Strait and San Pablo Bay during high outflow when surface EC is less than 20mS/cm. Sampling is conducted by CDWR in conjunction with discrete water quality monitoring Three gear types are used for targeting different sized zooplankters: 1) a pump for sampling microzooplankton <1.0 mm long, including rotifers, copepod nauplii, and adult copepods of the genus Limnoithona, 2) a modified Clarke-Bumpus (CB) net for sampling mesozooplankton 0.5-3.0 mm long, including cladocerans, copepodids (immature copepods), and adult copepods, and 3) a macrozooplankton net for sampling zooplankton 1-20 mm long, including mysid shrimp. Samples are fixed in formalin and transported to the CDFG Laboratory in Stockton for processing.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(glue)
library(fs)
library(sf)
library(lubridate)
library(janitor)


# GET ZOOP DATA ---------------------------------------------------------

## updated data lives here as a csv: https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=9190cd46d697e59aca2de678f4ca1c95

# download file for zoop sampling data
zoop_link <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=9190cd46d697e59aca2de678f4ca1c95"
# filename
fn <- glue::glue("IEP_zooplankton_downloaded_{Sys.Date()}.csv")
download.file(zoop_link, destfile = glue("data/{fn}"))

# ZOOP DATA
cbmatrix <- read_csv(glue("data/{fn}")) %>%  
  clean_names()

str(cbmatrix)


# GET TAXONOMY DATA -------------------------------------------------------

# TAXA LOOKUP DATA
taxa_link <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=d61f889b5d0763cfd7ae8e403928b634"

fnTaxa <- glue("IEP_yb_taxonomytable_downloaded_{Sys.Date()}.csv")

download.file(taxa_link, destfile = glue("data/{fnTaxa}"))

taxa <- read_csv(glue("data/{fnTaxa}")) %>% 
  clean_names()


# GET STATION DATA --------------------------------------------------------

station_link <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=f2332a9a2aad594f61fea525584694da"

fnStations <- glue("IEP_stationcoords_downloaded_{Sys.Date()}.csv")

download.file(station_link, destfile = glue("data/{fnStations}"))

stations <- read_csv(glue("data/{fnStations}")) %>% 
  clean_names()

# MAKE SPATIAL ------------------------------------------------------------

stations_sf <- st_as_sf(stations, coords = c("longitude", "latitude"), remove = F, crs=4326)

library(mapview)
mapview(stations_sf)

# FORMAT DATA -------------------------------------------------------------

cb_wide <- cbmatrix
cb_wide$date <- ymd(cb_wide$date)
cb_wide$mm <- month(cb_wide$date) # add month

# CHECK DATA FOR MISSINGNESS ----------------------------------------------

library(naniar)
gg_miss_var(cb_wide, "date", show_pct = T)
gg_miss_var(cb_wide, "allcladocera")
summary(cb_wide$allcladocera)
range(cb_wide$allcladocera)

# count how many records by station
# cb_wide %>% group_by(station) %>% tally() %>% View()

# LOG TRANSFORM -----------------------------------------------------------

# log transform for scaling:
cb_wide <- cb_wide %>% 
  mutate(allcladocera_log = log(allcladocera+1))
hist(cb_wide$allcladocera_log, col="maroon") # check range
range(cb_wide$allcladocera_log) # check range

# SAVE OUT ----------------------------------------------------------------

save(stations_sf, file= "data/stations_sf.rda")
save(cb_wide, file="data/cleaned_CB_data.rda")

