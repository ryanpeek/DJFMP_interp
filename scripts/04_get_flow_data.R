# GET FLOW DATA 

# download flow data from USGS and CDEC

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)

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
                     start = "2014-10-01", end = "2018-09-30",csv = F)

get.CDEC(station = "MHB", sensor = 20, duration = "H", 
         start = "2014-10-01", end = "2018-09-30",csv = F)


# SUMMARIZE TO DAILY ------------------------------------------------------

# add yday to everything
MHB <- add_WYD(df = MHB, datecolumn = "DATE_TIME")
FPT <- add_WYD(df = FPT, datecolumn = "DATE_TIME")
FPT$VALUE <- as.numeric(FPT$VALUE)

# group and summarize:
MHB %>% mutate(VALUE=as.numeric(VALUE)) %>% 
  filter(!is.na(VALUE)) %>% 
  group_by(WY, DOWY) %>% 
  summarize(VALUE = mean(VALUE, na.rm = T)) %>% 
  mutate(STATION_ID="MHB") %>% 
  dplyr::select(STATION_ID, DOWY, WY, VALUE) -> MHB_day

FPT_day <- FPT %>% dplyr::select(STATION_ID, DOWY, WY, VALUE)


# PLOT CDEC ---------------------------------------------------------------

# make a quick plot of data:

ggplot() + 
  geom_line(data=FPT_day, aes(x=DOWY, y=VALUE, color=STATION_ID), lty=2) + 
  ylab("log(CFS)") +  scale_y_log10() +
  geom_line(data=MHB_day, aes(x=DOWY, y=VALUE, color=STATION_ID)) +
  scale_color_manual("Site", values=c("FPT"=viridis(1), "MHB"=viridis(3)[2])) +
  theme_bw() +
  facet_grid(WY~.)


# single year:
(flow2017 <- ggplot() + 
    geom_line(data=FPT[FPT$WY==2017,], aes(x=DOWY, y=VALUE, color=STATION_ID), lty=2, lwd=1) + 
    ylab("log(CFS)") +  #scale_y_log10() +
    geom_line(data=MHB_day[FPT$WY==2017,], aes(x=DOWY, y=VALUE, color=STATION_ID), lwd=1) +
    scale_color_manual("Site", values=c("FPT"=viridis(1), "MHB"=viridis(3)[2])) +
    theme_bw())

(flow2015 <- ggplot() + 
    geom_line(data=FPT[FPT$WY==2015,], aes(x=DOWY, y=VALUE, color=STATION_ID), lty=2, lwd=1) + 
    ylab("log(CFS)") +  #scale_y_log10() +
    geom_line(data=MHB_day[FPT$WY==2015,], aes(x=DOWY, y=VALUE, color=STATION_ID), lwd=1) +
    scale_color_manual("Site", values=c("FPT"=viridis(1), "MHB"=viridis(3)[2])) +
    theme_bw())


# ADD RASTERS -------------------------------------------------------------

# run the 03_visualize_rasters script


# PLOT TOGETHER -----------------------------------------------------------


# Patchwork/cowplot stuff together?
library(patchwork)
p2017 + flow2017 + plot_layout(ncol = 1, heights = c(1, 1)) 

p2015 + flow2015 + plot_layout(ncol = 1, heights = c(1, 1)) 


# cowplot
library(cowplot)
plot_grid(p2015, flow2015, nrow=2)
ggsave(filename = "figures/flow_cladoceran_2015_jun.png", width = 11, height = 8, units = "in", dpi = 300)


plot_grid(p2017, flow2017, nrow=2)
ggsave(filename = "figures/flow_cladoceran_2017_jun.png", width = 11, height = 8, units = "in", dpi = 300)
