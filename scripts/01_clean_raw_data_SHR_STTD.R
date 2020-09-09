# 01_clean_raw_data


# Overview ----------------------------------------------------------------

# this data came from the IEP Zooplankton Data
# more info here: http://www.dfg.ca.gov/delta/projects.asp?ProjectID=ZOOPLANKTON

# Interagency Ecological Program: Zooplankton catch and water quality data from the Sacramento River floodplain and tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2018

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
zoop <- read_csv(glue("data/{fn}")) %>%  
  clean_names()

str(zoop)


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


# CHECK DATA FOR MISSINGNESS ----------------------------------------------

library(naniar)
# add year and month
zoop <- zoop %>% 
  mutate(month = month(date))
gg_miss_var(zoop, "date", show_pct = T)
gg_miss_var(zoop)


# Get just Cladocerans ----------------------------------------------------

# filter to mesh size and suborder and after 2012 and cladocerans
cladlist <- taxa %>% 
  filter(classification=="Cladocera" | taxon_name=="Cladocera") %>% 
  pull(taxon_name) # get just taxon_name

# now filter data
clad_zoop <- zoop %>% 
  filter(mesh_size=="150_micron", 
         taxon_name %in% cladlist,  
         wy>2012) %>% 
  select(date, month, station_code:turbidity, mesh_size, organism_id:cpue_ed)

# join with TAXA data
clad_zoop <- left_join(clad_zoop, taxa[,c("organism_id", "common_name","classification","organism")], by="organism_id")

# difference between cpue and cpue_ed?
summary(clad_zoop$cpue)
summary(clad_zoop$cpue_ed)

# count how many records by year/month
clad_zoop %>% group_by(station_code, wy) %>% tally() 


# SAVE OUT ----------------------------------------------------------------

save(clad_zoop, file= glue("data/DWR_zoop_SHR_STTD_2018.rda"))
write_csv(clad_zoop, path = glue("data/DWR_zoop_SHR_STTD_2018.csv"))

