# notes ----
## Summarise scallop catch data from the NMFS EBS RACE Bottom Trawl Survey 
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020-5-29

# load ----
library(tidyverse)
library(FNGr)

source("./code./misc/adfg_map_functions.R")
source("./code/misc/general_observer_data_functions.R")

## global options
### ggplot theme (from FNGr)
theme_set(theme_sleek()+
            theme(legend.position = "bottom"))
### color pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# data ----
ebs <- read_csv("./data/race_ebs_survey/ebs_cpue_by_haul.csv")

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

# data mgmt ----
names(ebs) <- c("survey", "year", "cruise_join", "haul_join", "catch_join", "vessel", "cruise", "haul", "start_lat", "start_lon",
                "end_lat", "end_lon", "stratum", "stratum_INPFC", "stratum_min_depth", "stratum_max_depth", "stratum_description", 
                "stratum_reg_name", "stratum_type", "domain", "density", "gear_performance", "statisfactory_gear_performance", 
                "gear_peformance_note", "gear_depth", "bottom_depth", "distance", "net_width", "species_code", "common_name", 
                "scientific_name", "wt_kg", "number_fish", "effort_km2", "wt_cpue", "num_cpue", "load_date")
ebs %>%
  mutate(lat = (start_lat + end_lat) / 2,
         lon = (start_lon + end_lon) / 2) -> ebs

# map of survey stations by area ----
ebs %>%
  dplyr::select(year, lon, lat) %>%
  # add data type (i.e. source)
  mutate(data_type = "Trawl Survey") %>%
  # bind with fishery data
  bind_rows(logbook %>%
              filter(District == "Q") %>%
              mutate(year = as.numeric(substring(Season, 1, 4)),
                     data_type = "Fishery") %>%
              rename(lon = set_lon, lat = set_lat) %>%
              dplyr::select(year, lon, lat, data_type)) %>%
  filter(year >= 2014) -> tmp


# map of survey stations
f_base_map+
  geom_point(data = tmp, aes(x = lon, y = lat, color = data_type), size = 0.5)+
  labs(color = NULL)+
  scale_color_manual(values = cb_palette[c(4, 6)])+
  coord_quickmap(xlim = c(-163, -170), ylim = c(54.5, 56.5))+
  facet_wrap(~year, ncol = 2)+
  guides(color = guide_legend(override.aes = list(size = 1.5))) -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_vs_fishery_locations_area_Q.png",
       plot = x,
       height = 7, width = 7, units = "in")


ebs %>%
  filter(year > 2013) %>%
  mutate(wt_cpue = ifelse(wt_cpue == 0, NA, wt_cpue)) -> tmp

f_base_map+
  geom_point(data = tmp, aes(x = lon, y = lat, size = wt_cpue), 
             alpha = 0.5, color = cb_palette[6])+
  coord_quickmap(xlim = c(-161, -180), ylim = c(53, 58))+
  labs(size = "Round Weight CPUE (kg / sq km)")+
  facet_wrap(~year, ncol = 2) -> x
ggsave("./figures/fishery_independent/2020/trawl_survey_cpue_map_area_Q.png",
       plot = x,
       height = 7, width = 7, units = "in")


ebs %>%
  filter(year > 2010) %>%
  group_by(year) %>%
  summarise(cpue = sum(wt_kg) / sum(effort_km2),
            cv = sqrt(var(wt_cpue) / n()) / cpue) %>%
  ungroup() %>%
  mutate(cpue_scaled = as.numeric(scale(cpue)),
         l95 = cpue_scaled - 1.96 * abs((cv * cpue_scaled)),
         u95 = cpue_scaled + 1.96 * abs((cv * cpue_scaled)),
         data_type = "Trawl Survey") %>%
  bind_rows(read_csv("./data/observer_data_summary/2020/standardized_cpue_season_Q.csv") %>%
              mutate(year = as.numeric(substring(Season, 1, 4)),
                     cpue_scaled = as.numeric(scale(std_cpue)),
                     data_type = "Fishery (Standardized)") %>%
              dplyr::select(year, cpue_scaled, data_type) %>%
              filter(year > 2010)) %>%
  mutate(data_type = factor(data_type, levels = c("Trawl Survey", "Fishery (Standardized)"))) %>%
  ggplot()+
  geom_errorbar(aes(x = factor(year), ymin = l95, ymax = u95, color = data_type), width = 0.2)+
  geom_point(aes(x = factor(year), y = cpue_scaled, color = data_type))+
  geom_line(aes(x = factor(year), y = cpue_scaled, group = data_type, color = data_type))+
  labs(x = NULL, y = expression(paste("Scaled CPUE ( ", mu, " = 0, ", sigma, " = 1 )")), color = NULL)+
  scale_color_manual(values = cb_palette[c(3,4)]) -> x
ggsave("./figures/fishery_independent/2020/scaled_cpue_Q.png", plot = x, height = 3, width = 5, units = "in")
