# notes ----
## summarise scallop catch from westward region largemesh survey
## author: Tyler Jackson
## last updated 2020/4/8
## data source: Kally Spalinger, Ric Shepard

# load / data ----
library(tidyverse)
library(FNGr)

## load and clean survey data
source("./code/fishery_independent/load_clean_westward_standard_largemesh_data.R")
## custom functions for fishery data and f_base_map
source("./code/misc/general_observer_data_functions.R")

## global options
### ggplot theme (from FNGr)
theme_set(theme_sleek()+
            theme(legend.position = "bottom"))
### color pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## fishery logbook data
### scallop haul data 2009/10 - Present
do.call(bind_rows, lapply(paste0("data/observer_data_summary/raw_data/catch/", 
                                 list.files("data/observer_data_summary/raw_data/catch/")), read_csv)) %>%
  # rename columns (from general_observer_data_functions.R)
  f_catch_rename() %>%
  # add season
  f_add_season() %>%
  # correct districts
  f_revise_district() %>%
  # phantom ranom removal
  filter(Season != "NA/NA") -> logbook

## shapefile of district boundary lines (f_shp_prep, from adfg_map_functions.R)
## loaded in load_clean_westward_standard_largemesh_data.R)
f_shp_prep("./data/maps/mgmt_units", "AreasKM_ScallopDistrictBoundaries", fortify = T) %>%
  f_transform_crs(to = proj4string(raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps"))) %>%
  .[[1]] -> district_boundaries

# map of survey stations by area ----

## create generic map data points tibble
wslm_catch %>%
  dplyr::select(Year, reg_area, district, lon, lat) %>%
  # add data type (i.e. source)
  mutate(data_type = "Trawl Survey") %>%
  # bind with fishery data
  bind_rows(logbook %>%
              mutate(Year = as.numeric(substring(Season, 1, 4)),
                     reg_area = substring(Fishery, 1, 1),
                     data_type = "Fishery") %>%
              rename(lon = set_lon, lat = set_lat, district = District) %>%
              dplyr::select(Year, reg_area, district, lon, lat, data_type)) %>%
  filter(Year >= 2014) -> tmp

## Area K
f_base_map+
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "K"),
             aes(x = lon, y = lat, color = data_type), size = 0.5)+
  labs(color = NULL)+
  scale_color_manual(values = cb_palette[c(4, 6)])+
  coord_quickmap(xlim = c(-156.1, -150), ylim = c(55.7, 59))+
  facet_wrap(~Year, ncol = 2)+
  guides(color = guide_legend(override.aes = list(size = 1.5))) -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_vs_fishery_locations_area_K.png",
       plot = x,
       height = 7, width = 7, units = "in")

## Area O
f_base_map+
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "O"),
             aes(x = lon, y = lat, color = data_type))+
  labs(color = NULL)+
  scale_color_manual(values = cb_palette[c(4, 6)])+
  coord_quickmap(xlim = c(-170, -163), ylim = c(52.5, 55))+
  facet_wrap(~Year, ncol = 2) -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_vs_fishery_locations_area_O.png",
       plot = x,
       height = 7, width = 7, units = "in")

## Area M
### number of hauls in area
tmp %>%
  filter(reg_area == "M",
         data_type == "Trawl Survey",
         Year == 2019) %>%
  nrow
### map
f_base_map+
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "M"),
             aes(x = lon, y = lat, color = data_type))+
  labs(color = NULL)+
  scale_color_manual(values = cb_palette[c(4, 6)])+
  coord_quickmap(xlim = c(-165, -155), ylim = c(53.7, 57))+
  facet_wrap(~Year, ncol = 2) -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_vs_fishery_locations_area_M.png",
       plot = x,
       height = 7, width = 7, units = "in")


# shell height composition ----
# plots of last 5 years, by registration area

## Area O
wslm_specimen %>%
  filter(reg_area == "O",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_O_size_comp.png", plot = x, height = 6, width = 4, units = "in")
  
## Area M
wslm_specimen %>%
  filter(reg_area == "M",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_M_size_comp.png", plot = x, height = 6, width = 4, units = "in")

## Area K
### KNE
wslm_specimen %>%
  filter(district == "KNE",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_KNE_size_comp.png", plot = x, height = 6, width = 4, units = "in")
### KSH
wslm_specimen %>%
  filter(district == "KSH",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_KSH_size_comp.png", plot = x, height = 6, width = 4, units = "in")
### KSW
wslm_specimen %>%
  filter(district == "KSW",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_KSW_size_comp.png", plot = x, height = 6, width = 4, units = "in")
### KSE
wslm_specimen %>%
  filter(district == "KSE",
         Year %in% 2015:2019) %>%
  ggplot()+
  geom_histogram(aes(x = sh, weight = sampfrac), binwidth = 2, fill = cb_palette[3], color = "black")+
  facet_wrap(~Year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Count") -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_KSE_size_comp.png", plot = x, height = 6, width = 4, units = "in")



# nominal scallops cpue (round weight / area_swept) ----

wslm_catch %>%
  # add area swept, net width = 0.0122 km
  # add round weight cpue
  mutate(area_swept = distance_km * 0.0122,
         rw_cpue = final_wt_kg / area_swept) -> tmp

## KNE
tmp %>%
  filter(district == "KNE",
         Year > 2010) %>%
  group_by(Year) %>%
  summarise(cpue = sum(final_wt_kg) / sum(area_swept),
            cv = sqrt(var(rw_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_KNE.csv") %>%
              mutate(Year = as.numeric(substring(Season, 1, 4)),
                     cpue_scaled = as.numeric(scale(std_cpue)),
                     data_type = "Fishery (Standardized)") %>%
              dplyr::select(Year, cpue_scaled, data_type) %>%
              filter(Year > 2010)) %>%
  mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(Year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(Year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(Year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_KNE.png", plot = x, height = 3, width = 5, units = "in")  

## KSH
tmp %>%
  filter(district == "KSH",
         Year > 2010) %>%
  group_by(Year) %>%
  summarise(cpue = sum(final_wt_kg) / sum(area_swept),
            cv = sqrt(var(rw_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_KSH.csv") %>%
              mutate(Year = as.numeric(substring(Season, 1, 4)),
                     cpue_scaled = as.numeric(scale(std_cpue)),
                     data_type = "Fishery (Standardized)") %>%
              dplyr::select(Year, cpue_scaled, data_type) %>%
              filter(Year > 2010)) %>%
  mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(Year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(Year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(Year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_KSH.png", plot = x, height = 3, width = 5, units = "in")  

## KSW
tmp %>%
  filter(district == "KSW",
         Year > 2010) %>%
  group_by(Year) %>%
  summarise(cpue = sum(final_wt_kg) / sum(area_swept),
            cv = sqrt(var(rw_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_KSW.csv") %>%
              mutate(Year = as.numeric(substring(Season, 1, 4)),
                     cpue_scaled = as.numeric(scale(std_cpue)),
                     data_type = "Fishery (Standardized)") %>%
              dplyr::select(Year, cpue_scaled, data_type) %>%
              filter(Year > 2010)) %>%
  mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(Year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(Year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(Year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_KSW.png", plot = x, height = 3, width = 5, units = "in")  

## KSE
tmp %>%
  filter(district == "KSE",
         Year > 2010) %>%
  group_by(Year) %>%
  summarise(cpue = sum(final_wt_kg) / sum(area_swept),
            cv = sqrt(var(rw_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  # bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_KSE.csv") %>%
  #             mutate(Year = as.numeric(substring(Season, 1, 4)),
  #                    cpue_scaled = as.numeric(scale(std_cpue)),
  #                    data_type = "Fishery (Standardized)") %>%
  #             dplyr::select(Year, cpue_scaled, data_type) %>%
  #             filter(Year > 2010)) %>%
  # mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(Year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(Year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(Year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_KSE.png", plot = x, height = 3, width = 5, units = "in") 

## area M
tmp %>%
  filter(reg_area == "M",
         Year > 2010) %>%
  group_by(Year) %>%
  summarise(cpue = sum(final_wt_kg) / sum(area_swept),
            cv = sqrt(var(rw_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_M.csv") %>%
              mutate(Year = as.numeric(substring(Season, 1, 4)),
                     cpue_scaled = as.numeric(scale(std_cpue)),
                     data_type = "Fishery (Standardized)") %>%
              dplyr::select(Year, cpue_scaled, data_type) %>%
              filter(Year > 2010)) %>%
  mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(Year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(Year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(Year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_M.png", plot = x, height = 3, width = 5, units = "in") 




# maps of cpue ----

wslm_catch %>%
  filter(Year >= 2014) %>%
  # add area swept, net width = 0.0122 km
  # add round weight cpue
  mutate(area_swept = distance_km * 0.0122,
         rw_cpue = final_wt_kg / area_swept,
         rw_cpue = ifelse(rw_cpue == 0, NA, rw_cpue)) -> tmp

# area K
f_base_map +
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "K"), 
             aes(x = lon, y = lat, size = rw_cpue), 
             alpha = 0.5, color = cb_palette[6])+
  labs(size = "Round Weight CPUE (kg / sq km)")+
  coord_quickmap(xlim = c(-156.1, -150), ylim = c(55.7, 59))+
  facet_wrap(~Year, ncol = 2)  -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_cpue_map_area_K.png",
       plot = x,
       height = 7, width = 7, units = "in")

# area M
f_base_map +
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "M"), 
             aes(x = lon, y = lat, size = rw_cpue), 
             alpha = 0.5, color = cb_palette[6])+
  labs(size = "Round Weight CPUE (kg / sq km)")+
  coord_quickmap(xlim = c(-165, -155), ylim = c(53.7, 57))+
  facet_wrap(~Year, ncol = 2)  -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_cpue_map_area_M.png",
       plot = x,
       height = 7, width = 7, units = "in")

# area O
f_base_map +
  geom_line(data = district_boundaries, aes(x = long, y = lat, group = group))+
  geom_point(data = filter(tmp, reg_area == "O"), 
             aes(x = lon, y = lat, size = rw_cpue), 
             alpha = 0.5, color = cb_palette[6])+
  labs(size = "Round Weight CPUE (kg / sq km)")+
  coord_quickmap(xlim = c(-169, -164.2), ylim = c(52.5, 54.5))+
  facet_wrap(~Year, ncol = 2)  -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_cpue_map_area_O.png",
       plot = x,
       height = 7, width = 7, units = "in")
