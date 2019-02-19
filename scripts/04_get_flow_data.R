# GET FLOW DATA 

# download flow data from USGS and CDEC

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# FOR USGS DATA
#library(devtools)
#install_github("USGS-R/dataRetrieval")
library(dataRetrieval)

# FOR CDEC DATA
#load custom functions:
source("scripts/f_get_cdec.R")
source("scripts/f_add_WYD.R")

# GET CDEC ----------------------------------------------------------------

# MHB (Cosumnes)
# FPT (Sac at Freeport)
# IST (Sac at I St Bridge)

get.CDEC(station = "FPT", sensor = 20, duration = "D", 
                     start = "2014-10-01", end = "2018-10-01",csv = F)

get.CDEC(station = "MHB", sensor = 20, duration = "H", 
         start = "2014-10-01", end = "2018-10-01",csv = F)


# SUMMARIZE TO DAILY ------------------------------------------------------

# add yday to everything
MHB <- add_WYD(df = MHB, datecolumn = "DATE_TIME")
FPT <- add_WYD(df = FPT, datecolumn = "DATE_TIME")
FPT$VALUE <- as.numeric(FPT$VALUE)

# group and summarize:
MHB %>% mutate(VALUE=as.numeric(VALUE)) %>% 
  filter(!is.na(VALUE)) %>% 
  group_by(WY, DOWY) %>% 
  summarize(VALUE = mean(VALUE, na.rm = T)) -> MHB_day

# PLOT CDEC ---------------------------------------------------------------

# make a quick plot of data:

ggplot() + 
  geom_line(data=FPT, aes(x=DOWY, y=VALUE, color=as.factor(WY)), lty=2) + 
  ylab("log(CFS)") + 
  geom_line(data=MHB_day, aes(x=DOWY, y=VALUE, color=as.factor(WY))) + 
  scale_color_viridis_d("Water Year") + 
  theme_bw() #+ scale_y_log10()

