# notes ----
## summarise westward region trawl survey scallop catch
## Tyler Jackson
## 10/19/2021

# load ----
library(tidyverse)
library(FNGr)

## plotting options from FNGr
theme_set(theme_sleek())
yr_axis <- tickr(tibble(year = 1988:2021), year, 5)

### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## panel labels
panel_labs <- c("K. Northeast", "K. Southeast", "K. Shelikof", "K. Southwest", 
                "AK Pen. Central", "AK Pen. W. Chignik")

# data ----

## load and clean survey data
source("./code/fishery_independent/trawl_surveys/load_clean_westward_standard_largemesh_data.R")

hauls <- read_csv("./data/westward_standard_lm_survey/haul_info/CRAB2021-haulSummary.csv") 

## standard stations
std_stations <- read_csv("./data/westward_standard_lm_survey/tanner_standard_stations.csv")

## load trawl survey grid shapefile
f_shp_prep("./data/maps/lm_survey_stations", "LMStationMasterAlbers", fortify = F) %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
  dplyr::filter(STATION %in% unique(wslm_catch$Station)) -> lm_stations
  filter(lm_stations, STATION %in% std_stations$station) %>%
  fortify() %>%
  left_join(lm_stations@data, by = "id") %>%
  left_join(std_stations, by = c("STATION" = "station"))%>%
  arrange(order) -> lm_stations
  
## catch data
catch_all <- do.call("rbind", 
                 lapply(list.files(path = "./data/westward_standard_lm_survey/catch_dump",
                                   pattern = ".csv", full.names = T), 
                        read_csv, col_types = cols()))
  

# data mgmt ----
wslm_catch %>%
  rename_all(tolower) %>%
  left_join(std_stations, by = "station") -> catch

wslm_specimen %>%
  rename_all(tolower) %>%
  left_join(std_stations, by = "station") -> spec

hauls %>%
  rename_all(tolower) %>%
  left_join(std_stations, by = "station") -> hauls
  
# map figure ----

## area K
f_base_map+
  geom_polygon(data = filter(lm_stations, 
                             scallop_index_station_dist %in% c("KNE", "KSH", "KSE", "KSW")),
               aes(x = long, y = lat, group = id, fill = scallop_index_station_dist),
               color = 1, alpha = 0.5)+
  geom_polygon(data = filter(lm_stations, 
                             is.na(scallop_index_station_dist)),
               aes(x = long, y = lat, group = id),
               color = 1, fill = "grey40")+
  geom_polygon(data = district_polygons, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.4, alpha = 0.5)+
  coord_quickmap(xlim = c(-155.5, -151), ylim = c(56.5, 59))+
  labs(fill = NULL)+
  theme(legend.position = c(0.1, 0.9))+
  scale_fill_manual(values = cb_palette[2:5]) -> x
ggsave("./figures/fishery_independent/2022/westward_trawl_stations_area_k.png",
       plot = x, height = 5, width = 4, units = "in")

# area M
f_base_map+
  geom_polygon(data = filter(lm_stations, 
                             scallop_index_station_dist %in% c("C", "WC")),
               aes(x = long, y = lat, group = id, fill = scallop_index_station_dist),
               color = 1, alpha = 0.5)+
  geom_polygon(data = filter(lm_stations, 
                             is.na(scallop_index_station_dist)),
               aes(x = long, y = lat, group = id),
               color = 1, fill = "grey40")+
  geom_polygon(data = district_polygons, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.4, alpha = 0.5)+
  coord_quickmap(xlim = c(-164, -157), ylim = c(54, 57))+
  labs(fill = NULL)+
  theme(legend.position = c(0.1, 0.9))+
  scale_fill_manual(values = cb_palette[6:7]) -> x
ggsave("./figures/fishery_independent/2022/westward_trawl_stations_area_m.png",
       plot = x, height = 4, width = 5, units = "in") 

# station number table ----
std_stations %>%
  count(scallop_district) 
        
std_stations %>%
  count(scallop_index_station_dist)

## number of stations towed in recent survey
hauls %>%
  filter(performance <= 4,
         !is.na(scallop_index_dist)) %>%
  count(scallop_index_dist)

# cpue ----

## all districts
catch %>%
  filter(!is.na(scallop_index_station_dist)) %>%
  ## compute average cpue (wt) per season
  group_by(year, scallop_index_station_dist) %>%
  summarise(cpue = mean(final_wt_kg / distance_km),
            se = sqrt(var(final_wt_kg / distance_km) /n()),
            cv = se / cpue) -> cpue
write_csv(cpue, "./output/fishery_independent/2021/westward_trawl_cpue_wt.csv")
### plot w/ lognormal CI
cpue %>%
  mutate(scallop_index_station_dist = factor(scallop_index_station_dist, 
                                             c("KNE", "KSE", "KSH", "KSW", "C", "WC")),
         panel_labs = factor(panel_labs[as.numeric(scallop_index_station_dist)],
                             panel_labs)) %>%
  ggplot()+
  geom_point(aes(x = year, y = cpue))+
  geom_line(aes(x = year, y = cpue))+
  geom_errorbar(aes(x = year, 
                    ymin = cpue * exp(qnorm(0.25) * sqrt(log(1 + cv^2))),
                    ymax = cpue * exp(qnorm(0.75) * sqrt(log(1 + cv^2)))), 
                width = 0) + 
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
  labs(x = NULL, y = "CPUE (kg / km)")+
  facet_wrap(~panel_labs, nrow = 3, scales = "free") -> x
ggsave("./figures/fishery_independent/2022/westward_trawl_cpue_wt.png",
       plot = x, height = 7, width = 7, units = "in") 

# size comp figures ---- 

## commute number of scallops caught in each 1 mm size bin
spec %>%
  group_by(year, scallop_index_station_dist, sh) %>%
  summarise(n = sampfrac) %>%
  ungroup -> sh_comp
write_csv(cpue, "./output/fishery_independent/2021/westward_trawl_cpue_wt.csv")

## plot
sh_comp %>%
  # filter for most recent five years
  filter(year >= (max(year)-4),
         scallop_index_station_dist %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  mutate(scallop_index_station_dist = factor(scallop_index_station_dist, 
                                             c("KNE", "KSE", "KSH", "KSW", "C", "WC")),
         panel_labs = factor(panel_labs[as.numeric(scallop_index_station_dist)],
                             panel_labs)) %>%
  ggplot+
  geom_histogram(aes(x = sh, weight = n), binwidth = 5, color = 1, fill = "grey90")+
  facet_grid(cols = vars(panel_labs), rows = vars(year), scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Scallops Caught") -> area_k_size_comp
ggsave("./figures/fishery_independent/2022/westward_trawl_size_comp_area_k.png",
       plot = area_k_size_comp, height = 5, width = 7, units = "in") 

sh_comp %>%
  # filter for most recent five years
  filter(year >= (max(year)-4),
         scallop_index_station_dist %in% c("C", "WC")) %>%
  mutate(scallop_index_station_dist = factor(scallop_index_station_dist, 
                                             c("KNE", "KSE", "KSH", "KSW", "C", "WC")),
         panel_labs = factor(panel_labs[as.numeric(scallop_index_station_dist)],
                             panel_labs)) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = n), binwidth = 5, color = 1, fill = "grey90")+
  facet_grid(cols = vars(panel_labs), rows = vars(year), scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Scallops Caught") -> area_m_size_comp
ggsave("./figures/fishery_independent/2022/westward_trawl_size_comp_area_m.png",
       plot = area_m_size_comp, height = 5, width = 4, units = "in") 



