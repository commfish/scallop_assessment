# load ----
library(tidyverse)
library(FNGr)
source("./code./misc/adfg_map_functions.R")

## global options
### ggplot theme (from FNGr)
theme_set(theme_sleek()+
            theme(legend.position = "bottom"))
### color pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# data ----

## haul data
hauls <- read_csv("./data/race_goa_survey/race_goa_hauls.csv")

## registration area polygons
f_shp_prep("./data/maps/mgmt_units", "Scallop_Statewide_Areas_PY", fortify = F) %>%
  spTransform(., CRS(proj4string(raster::getData("GADM", country = c("USA"), 
                                                 level = 1, path = "./data/maps")))) -> reg_areas

## base map
## high resolution map of alaska, canada
usa <- raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")
can <- raster::getData("GADM", country = c("CAN"), level = 1, path = "./data/maps")
bind_rows(fortify(usa), fortify(can)) %>%
  filter(long > -180, long < -129, lat > 45, lat < 70) -> canam

# data mgmt ----

## rename haul data
names(hauls) <- c("Survey", "Year", "Vessel", "Cruise", "haul", "haul_join", "stratum", "stratum_INPFC", 
                  "stratum_regulatory", "stratum_description", "haul_type", "performance", "start_time", "duration", 
                  "distance_fished", "net_width", "net_measured", "net_height",
                  "start_lat", "start_lon", "end_lat", "end_lon", "station", "gear_depth", "bottom_depth", "bottom_type",
                  "surface_temp", "gear_temperature", "wire_length", "gear", "accessories", "subsample", "cruise_join", 
                  "audit_join", "satisfactory_performance", "performance_note")

## add location, registration area, district, and region
hauls %>% 
  mutate(lat = (start_lat + end_lat) / 2,
         lon = (start_lon + end_lon) / 2) %>% 
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(scal_area = f_over(x = ., y = reg_areas, label = "Area_Name")) -> hauls

## bottom temp trend
FNGr::tickr(tibble(Year = 1984:2019), Year, 5) -> x_axis
hauls %>%
  filter(scal_area != "Southeastern Alaska") %>%
  group_by(Year, scal_area) %>%
  summarize(bt = mean(gear_temperature, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(x = Year, y = bt, color = scal_area))+
  geom_line(aes(x = Year, y = bt, group = scal_area, color = scal_area))+
  scale_color_manual(values = cb_palette[c(1:3, 4, 6:7)])+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  labs(x = NULL, y = "Mean Bottom Temperature °C", color = NULL) -> x
ggsave("./figures/fishery_independent/2020/goa_bt_timeseries.png", plot = x, height = 4, width = 6, units = "in")

## bttom temp anomaly
FNGr::tickr(tibble(Year = 1984:2019), Year, 5) -> x_axis
hauls %>%
  filter(scal_area != "Southeastern Alaska") %>%
  group_by(scal_area) %>%
  mutate(baseline = mean(gear_temperature, na.rm = T)) %>%
  group_by(Year, scal_area, baseline) %>%
  summarise(mean_bt = mean(gear_temperature, na.rm = T)) %>%
  mutate(anomaly = mean_bt - baseline) %>%
  ggplot()+
  geom_bar(aes(x = Year, y = anomaly, fill = anomaly > 0), 
           stat = "identity", width = 1, show.legend = F)+
  scale_fill_manual(values = c(4,2))+
  geom_hline(yintercept = 0)+
  labs(x = NULL, y = "Bottom Temperature Anomaly °C")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  facet_wrap(~scal_area, ncol = 2) -> x
ggsave("./figures/fishery_independent/2020/goa_bt_anomaly.png", plot = x, height = 6, width = 6, units = "in")




# map plots ----

## plot of goa survey stations and scallop mgmt areas
fortify(reg_areas, region = "Area_Name") %>%
  filter(!(id %in% c("Adak", "Bristol Bay - Bering Sea"))) %>%
  ggplot()+
  geom_polygon(aes(x = long, y= lat, group = id, fill = id), alpha = 0.5)+
  scale_fill_manual(values = cb_palette[2:8])+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group), 
               color = NA, fill = "grey90")+
  geom_point(data = filter(hauls, Year == 2019), aes(x = lon, y = lat), size = 0.5)+
  labs(x = expression(paste(Longitude^o,~'W')), 
       y = expression(paste(Latitude^o,~'N')),
       fill = NULL)+
  coord_quickmap(xlim = c(-172, -131), ylim = c(51, 62), expand = c(0,0))+
  theme(panel.background = element_rect(fill = "grey70")) -> x
ggsave("./figures/fishery_independent/2020/goa_survey_stations.png", plot = x, height = 4, width = 6, units = "in")



,
         region = ifelse(lon < -157.5, "WGOA", "GOA")) %>%
  group_by(Year, region) %>%
  summarise(bt = mean(gear_temperature, na.rm = T),
            l95 = sqrt(var(gear_temperature, na.rm = T) / n()) * -1.96 + bt,
            u95 = sqrt(var(gear_temperature, na.rm = T) / n()) * 1.96 + bt) 
  
         
         
         
         residual = gear_temperature - mean(gear_temperature),
         sign = residual > 0) %>%
  ggplot()+
  geom_point(aes(x = lon, y = lat, color = factor(gear_temperature)))+
  #scale_color_gradient(low = "blue", high = "red")+
  facet_wrap(~Year)
