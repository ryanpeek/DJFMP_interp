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

# FRE (Sac Fremont Weir)
# MHB (Cosumnes)
# FPT (Sac at Freeport)
# IST (Sac at I St Bridge)

# Freeport
get.CDEC(station = "FPT", sensor = 20, duration = "D", 
                     start = "2017-01-01", end = "2017-06-30",csv = F)

# add yday to everything and mutate value to numeric
FPT <- add_WYD(df = FPT, datecolumn = "DATE_TIME")
FPT$VALUE <- as.numeric(FPT$VALUE)

# two missing values, use simple avg to interpolate
FPT$VAL_interp <- zoo::na.approx(FPT$VALUE)

# plot
ggplot() + 
  geom_line(data=FPT, aes(x=OBS_DATE, y=VALUE), color="darkblue", lwd=2) +
  geom_line(data=FPT, aes(x=OBS_DATE, y=VAL_interp), color="gray") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
 
# Freemont (need to use daily mean flow, sensor=20 is only at "E/H" intervals)
get.CDEC(station = "FRE", sensor = 41, duration = "D", 
         start = "2017-01-01", end = "2017-06-30",csv = F)

# add yday to everything and mutate value to numeric
FRE <- add_WYD(df = FRE, datecolumn = "DATE_TIME")
FRE$VALUE <- as.numeric(FRE$VALUE)
summary(FRE$VALUE)

# plot
ggplot() + 
  geom_line(data=FRE, aes(x=OBS_DATE, y=VALUE), lty=2) +
  theme_bw() + 
  geom_line(data=FPT, aes(x=OBS_DATE, y=VAL_interp)) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%W")

# Cosumnes
get.CDEC(station = "MHB", sensor = 41, duration = "D", 
         start = "2017-01-01", end = "2017-06-30",csv = F)

# add yday to everything and mutate value to numeric
MHB <- add_WYD(df = MHB, datecolumn = "DATE_TIME")
MHB$VALUE <- as.numeric(MHB$VALUE)
summary(MHB$VALUE)
# missing values, use simple avg to interpolate
MHB$VAL_interp <- zoo::na.approx(MHB$VALUE)


# plot
ggplot() + 
  geom_line(data=FRE, aes(x=OBS_DATE, y=VALUE), color="blue", lty=2) +
  geom_line(data=FPT, aes(x=OBS_DATE, y=VAL_interp), color="darkblue") +
  geom_line(data=MHB, aes(x=OBS_DATE, y=VAL_interp), color="orange") +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%W") +
  theme_bw() + xlab("Week") + ylab("Flow (cfs)")



# SUB ANNUAL PLOT ---------------------------------------------------------

library(viridis)
library(ggthemes)

# combine the datasets:
flowdat <- bind_rows(FRE, FPT)

ggplot() + 
  geom_ribbon(data=filter(flowdat, STATION_ID=="FPT"), aes(x=OBS_DATE, ymin=0, ymax=VAL_interp, group=STATION_ID, fill=STATION_ID), alpha=0.9) +
  geom_line(data=filter(flowdat, STATION_ID=="FPT"), aes(x=OBS_DATE, y=VAL_interp, group=STATION_ID), color="gray50", lwd=1) +
  geom_ribbon(data=filter(flowdat, STATION_ID=="FRE"), aes(x=OBS_DATE, ymin=0, ymax=VALUE, group=STATION_ID, fill=STATION_ID), alpha=.7) +
  geom_line(data=filter(flowdat, STATION_ID=="FRE"), aes(x=OBS_DATE, y=VALUE, group=STATION_ID), color="gray50", lty=1) +
  #scale_fill_viridis_d("CDEC Station", labels=c("FPT: Freeport", "FRE: Fremont Weir"))+
  scale_fill_grey("CDEC Station", labels=c("FPT: Freeport", "FRE: Fremont Weir"))+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "Wk-%W") +
  theme_bw() + xlab("Week") + ylab("Flow (cfs)")+
  theme(legend.position = c(0.8, 0.8))

ggsave(filename = "figures/subannual_flow_FPT_FRE_greyscale.png", width = 7.2, height = 5, units = "in", dpi = 600, type="cairo")

# SUMMARIZE TO WEEKLY/DAILY ------------------------------------------------------

# group and summarize:
MHB %>% mutate(VALUE=as.numeric(VALUE)) %>% 
  filter(!is.na(VALUE)) %>% 
  mutate(yweek = lubridate::week(DATE_TIME)) %>% 
  group_by(WY, yweek) %>% 
  #group_by(WY, DOWY) %>% 
  summarize(VALUE = mean(VALUE, na.rm = T)) %>% 
  mutate(STATION_ID="MHB") %>% 
  #dplyr::select(STATION_ID, DOWY, WY, VALUE) -> MHB_day
  dplyr::select(STATION_ID, yweek, WY, VALUE) -> MHB_week # or switch to DOWY

# FPT_day <- FPT %>% dplyr::select(STATION_ID, DOWY, WY, VALUE)

FPT %>% mutate(VALUE=as.numeric(VALUE)) %>% 
  filter(!is.na(VALUE)) %>% 
  mutate(yweek = lubridate::week(DATE_TIME)) %>% 
  group_by(WY, yweek) %>% 
  summarize(VALUE = mean(VALUE, na.rm = T)) %>% 
  mutate(STATION_ID="FPT") %>% 
  #dplyr::select(STATION_ID, DOWY, WY, VALUE) -> FPT_day
  dplyr::select(STATION_ID, yweek, WY, VALUE) -> FPT_week

FRE %>% mutate(VALUE=as.numeric(VALUE)) %>% 
  filter(!is.na(VALUE)) %>% 
  mutate(yweek = lubridate::week(DATE_TIME)) %>% 
  group_by(WY, yweek) %>% 
  summarize(VALUE = mean(VALUE, na.rm = T)) %>% 
  mutate(STATION_ID="FRE") %>% 
  #dplyr::select(STATION_ID, DOWY, WY, VALUE) -> FRE_day
  dplyr::select(STATION_ID, yweek, WY, VALUE) -> FRE_week

# PLOT WEEK ---------------------------------------------------------------

ggplot() + 
  geom_line(data=FPT_week, aes(x=yweek, y=VALUE, color=STATION_ID), lty=2) + 
  ylab("Flow(cfs)") +
  geom_line(data=FRE_week, aes(x=yweek, y=VALUE, color=STATION_ID)) +
  scale_color_manual("Site", values=c("FPT"=viridis(1), "FRE"=viridis(3)[2])) +
  theme_bw() 

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
