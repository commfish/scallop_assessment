# notes ----

## 2021 observer data summary analysis
## organized by statistic
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/5/18

# load libraries and set global options ----

## packages
library(tidyverse)
library(scales)
library(magrittr)
library(FNGr)

## sourced scripts
### general observer data functions
source("./code/misc/general_observer_data_functions.R")
### functions for mapping
source("./code/misc/adfg_map_functions.R")

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))
### cutsom axis ticks for yrs (from FNGr)

# data ----

## metadata
### ghl
ghl <- read_csv("./data/observer/metadata/ghl_revised_timeseries.csv")

## observer/logbook data
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))
## shell heights 2009/10 - Present
shell_height <- do.call(bind_rows,
                   lapply(paste0("data/observer/shell_height/",
                                 list.files("data/observer/shell_height/")), read_csv))
### bycatch by day 2009/10 - Present
bycatch <- do.call(bind_rows,
                   lapply(paste0("data/observer/bycatch/", list.files("data/observer/bycatch/")), read_csv))
### crab bycath size data 2009/10 - Present
crab_size <- do.call(bind_rows,
                     lapply(paste0("data/observer/crab_size/", list.files("data/observer/crab_size/")), read_csv))

### shell height meat weight data
meat <- do.call(bind_rows,
                     lapply(paste0("data/observer/meat_weight/", list.files("data/observer/meat_weight/")), read_csv))

# data mgmt ----

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(Haul_ID) -> catch

## rename fields in current data (2009 - present)
f_catch_rename(catch) %>%
## add Season to data
f_add_season() %>%
## classify Karluk bed as KSW district instead of KSH
f_revise_district() %>% 
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date)) %>%
## remove tows with zero dredge hours (logbook mistake)
filter(dredge_hrs != 0)  %>%
## coerce date to date class
## fix issue with missing basket weight in 2018/19
mutate(round_weight = ifelse(Season == "2018/19" & District == "O",
                               54.1 * rtnd_basket, round_weight)) -> catch

## rename fields in bycatch data (2009 - present)
f_bycatch_rename(bycatch) %>%
## add Season to data
f_add_season() %>%
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date)) %>%
## classify Karluk bed as KSW district instead of KSH
left_join(dplyr::select(catch, Season, Set_date, ADFG, District, haul, set_lat, set_lon), 
          by = c("Season", "Set_date", "ADFG", "District", "haul")) %>%
f_revise_district() -> bycatch

## reason fields in crab size data (2009 - present)
f_crab_size_rename(crab_size) %>%
## drp the empty column
dplyr::select(-7) %>%
## add Season to data
f_add_season() %>%
## revise district
mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
## add Species and Sex
mutate(Species = case_when(RACE_code == 68560 ~ "Tanner crab",
                           RACE_code == 68541 ~ "snow crab"),
       Species = factor(Species, levels = c("Tanner crab", "snow crab")),
       Sex = case_when(sex == 1 ~ "Male",
                       sex == 2 ~ "Female",
                       sex == 3 ~ "Unknown"),
       Sex = factor(Sex, levels = c("Male", "Female", "Unknown"))) -> crab_size


## rename fields in shell_height data (2009 - present)
f_shell_height_rename(shell_height) %>%
## add Season to data
f_add_season() %>%
## revise District as in catch data
mutate(District = catch$District[match(.$Haul_ID, catch$Haul_ID)]) -> shell_height

## add season to meat weight data
f_add_season(meat, fishery_col = "fishery") %>%
## rename haul_id to join to catch
rename(Haul_ID = haul_id) %>%
## create haul id for 2020/21
mutate(Set_date = mdy(set_date),
       Haul_ID = ifelse(Season == "2020/21", 
                        paste0(fishery,
                               sprintf("%06d", adfg),
                               year(Set_date),
                               sprintf("%02d", month(Set_date)),
                               sprintf("%02d", day(Set_date)),
                               sprintf("%04d", haul)), Haul_ID)) %>%
  #filter(Season == "2020/21") %>%
  #select(Haul_ID)
                               
## join with catch data to get location (district, bed)
left_join(catch %>%
            dplyr::select(Haul_ID, District, bed_code, set_lat, set_lon),
          by = c("Haul_ID")) %>%
## add retained - discard factor
mutate(Type = ifelse(shell_num < 11, "retained", "discarded")) -> meat


# fishery catch ----

## fishery performance tables by district (f_fish_stats from general functions)
### KNE
f_fish_stats(catch, c("KNE"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_KNE.csv")
### KSH
f_fish_stats(catch, c("KSH"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_KSH.csv")
### KSW
f_fish_stats(catch, c("KSW"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_KSW.csv")
### KSE
f_fish_stats(catch, c("KSE"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_KSE.csv")
### KSEM
f_fish_stats(catch, c("KSEM"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_KSEM.csv")
### Area M
f_fish_stats(catch, c("UB"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_M.csv")
### Area O
f_fish_stats(catch, c("O"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_O.csv")
### Area Q
f_fish_stats(catch, c("Q"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_Q.csv")
### WKI
f_fish_stats(catch, c("WKI"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_WKI.csv")
### EKI
f_fish_stats(catch, c("EKI"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_EKI.csv")
### YAK
f_fish_stats(catch, c("YAK"), add_ghl = T, 
             path = "./output/observer/2021/fish_stats_YAK.csv")


# standardized cpue plots (f_standard_cpue from general observer functions) ----
catch %>%
  # create fields month and vessel
  mutate(Month = lubridate::month(Set_date),
         Month = factor(Month, levels = c(7:12, 1:6)),
         Vessel = factor(ADFG, levels = c("58200", "40924", "54966", "32554", "303")),
         Bed = factor(ifelse(is.na(bed_code), "Unknown", bed_code))) -> tmp

### KNE
#### plot by season
f_standardize_cpue(filter(tmp, District == "KNE", Bed != "Unknown"), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_KNE.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District == "KNE") %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_KNE.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "KNE"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "KNE"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_KNE.png", 
       plot = p1, height = 3, width = 7, units = "in")

### KSH
#### plot by season
f_standardize_cpue(filter(tmp, District == "KSH", Bed != "Unknown"), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_KSH.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District == "KSH") %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue, se)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_KSH.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "KSH"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "KSH"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_KSH.png", 
       plot = p1, height = 3, width = 7, units = "in")

### KSW (incuding bed)
tmp %>%
  filter(District == "KSW") %>%
  # move unknown beds to KSW1
  mutate(Bed = factor(ifelse(set_lat > 57.1, "KSH 4-6", "KSW1"))) -> tmp_ksw
#### plot by season
f_standardize_cpue(filter(tmp_ksw, District == "KSW", Bed != "Unknown"), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_KSW.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District == "KSW") %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_KSW.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "KSW"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "KSW"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_KSW.png", 
       plot = p1, height = 3, width = 7, units = "in")

### M
#### plot by season
f_standardize_cpue(filter(tmp, District %in% c("WC", "C", "UB")), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_M.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District %in% c("WC", "C", "UB")) %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_M.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District %in% c("WC", "C", "UB")), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District %in% c("WC", "C", "UB")), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_M.png", 
       plot = p1, height = 3, width = 7, units = "in")

### O
#### plot by season
f_standardize_cpue(filter(tmp, District == "O", Bed != "Unknown"), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_O.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District == "O") %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_O.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "O"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "O"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_O.png", 
       plot = p1, height = 3, width = 7, units = "in")


### Q
f_standardize_cpue2(filter(tmp, District %in% c("Q")), 
                    path = "./figures/ghl_supplement/2021/std_cpue_effects_Q.png") -> x
# revise nominal values so that unknown beds are included
filter(tmp, District %in% c("Q")) %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_Q.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "Q"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "Q"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")-> p1
ggsave("./figures/ghl_supplement/2021/standardized_cpue_Q.png", 
       plot = p1, height = 3, width = 7, units = "in")

### WKI
f_standardize_cpue2(filter(tmp, District %in% c("WKI")), 
                    path = "./figures/ghl_supplement/2021/std_cpue_effects_WKI.png") -> x
# revise nominal values so that unknown beds are included
filter(tmp, District %in% c("WKI")) %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_WKI.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "WKI"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "WKI"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")-> p1
ggsave("./figures/ghl_supplement/2021/standardized_cpue_WKI.png", 
       plot = p1, height = 3, width = 7, units = "in")


### YAK
#### combine beds YAK6Y and YAK6D
tmp %>%
  filter(District == "YAK") %>%
  mutate(Bed = factor(ifelse(Bed %in% c("YAK6Y", "YAK6D"), "YAK6", as.character(Bed))),
         Bed = factor(ifelse(Bed == "EK1", "YAKB", as.character(Bed)))) -> tmp_yak
#### plot by season
f_standardize_cpue(filter(tmp_yak, District == "YAK", Bed != "Unknown"), 
                   path = "./figures/ghl_supplement/2021/std_cpue_effects_YAK.png",
                   by = "Season",
                   compute_predicted = F) -> x
# revise nominal values so that unknown beds are included
filter(tmp, District == "YAK") %>%
  group_by(Season) %>%
  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)) %>%
  left_join(dplyr::select(x, c(Season, std_cpue)), by = "Season") %T>%
  write_csv("./output/observer/2021/standardized_cpue_season_YAK.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_violin(data = filter(tmp, District == "YAK"), 
              aes(x = Season, y = round_weight / dredge_hrs), 
              color = "grey80", fill = cb_palette[3], alpha = 0.5)+
  geom_boxplot(data = filter(tmp, District == "YAK"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey60", fill = NA, width = 0.1, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> p1

ggsave("./figures/ghl_supplement/2021/standardized_cpue_YAK.png", 
       plot = p1, height = 3, width = 7, units = "in")


# fishery extent ----

## raster maps of fishering effort by district, year (f_make_grid from adfg_map_functions.R)
### KNE
# build raster grid
catch %>%
  filter(District == "KNE",
         !is.na(set_lat), !is.na(set_lon)) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KNE_proj$limits$x,
                           lat = KNE_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  dplyr::select(Season, grid) %>%
  unnest(grid) -> tmp
# plot map
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  KNE_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 3)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_KNE.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSH
catch %>%
  filter(District == "KSH") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KSH_proj$limits$x,
                           lat = KSH_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.1, 0.2, 0.3))+
  KSH_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 3)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_KSH.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSW
catch %>%
  filter(District == "KSW") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KSW_proj$limits$x,
                           lat = KSW_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5))+
  KSW_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 4, drop = F)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_KSW.png", plot = x, 
       height = 8, width = 7, unit = "in")
### area M
catch %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = areaM_proj$limits$x,
                           lat = areaM_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5, 0.75))+
  areaM_proj+
  facet_wrap(~Season, nrow = 3, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_M.png", plot = x, 
       height = 6, width = 7, unit = "in")
### area O
catch %>%
  filter(District %in% c("O")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = areaO_proj$limits$x,
                           lat = areaO_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5, 0.75))+
  areaO_proj+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_O.png", plot = x, 
       height = 8, width = 7, unit = "in")
### area Q
catch %>%
  filter(District %in% c("Q")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = c(-166, -164),
                           lat = c(55, 56), by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.10, 0.20, 0.30))+
  coord_quickmap(xlim = c(-167, -163), ylim = c(54.5, 56.5))+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_Q.png", plot = x, 
       height = 8, width = 7, unit = "in")
### YAK
catch %>%
  filter(District %in% c("YAK"),
         !is.na(set_lat), !is.na(set_lon)) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = YAK_proj$limits$x,
                           lat = YAK_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.05, 0.10, 0.15))+
  YAK_proj+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/ghl_supplement/2021/effort_map_YAK.png", plot = x, 
       height = 6, width = 7, unit = "in")



## extent of round weight catch (f_extent_catch from general_observer_data_functions.R)
### KNE
catch %>%
  # remove tows with missing lat/lon
  filter(District == "KNE",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_KNE.png", plot = x, 
       height = 3, width = 7, unit = "in")  
### KSH
catch %>%
  filter(District == "KSH") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_KSH.png", plot = x, 
       height = 3, width = 7, unit = "in")
### KSW
catch %>%
  filter(District == "KSW") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_KSW.png", plot = x, 
       height = 3, width = 7, unit = "in")
### areaM
#### UB
catch %>%
  filter(District == "UB") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_UB.png", plot = x, 
       height = 3, width = 7, unit = "in")
#### O
catch %>%
  filter(District == "O") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_O.png", plot = x, 
       height = 3, width = 7, unit = "in")

#### Q
catch %>%
  filter(District == "Q") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_Q.png", plot = x, 
       height = 3, width = 7, unit = "in")

#### YAK
catch %>%
  filter(District == "YAK") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 300) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./300, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/cpue_extent_YAK.png", plot = x, 
       height = 3, width = 7, unit = "in")

# bycatch ----

## get bycatch by day
bycatch %>%
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            opilio_count = sum(opilio_count),
            dungeness_count = sum(dungeness_count),
            halibut_count = sum(halibut_count),
            king_count = sum(king_count)) -> bycatch_by_day

## total crab bycatch and bycatch:meatweight ratio by Season, District
### compute stats in large tibble to subset by district
bycatch_by_day %>%
  group_by(Season, District) %>%
  summarise(total_effort = sum(dredge_hrs),
            total_sample = sum(sample_hrs),
            tanner_rate = sum(bairdi_count) / total_sample,
            total_tanner = tanner_rate * total_effort,
            tanner_ratio = total_tanner / sum(mt_wt, na.rm = T),
            snow_rate = sum(opilio_count) / total_sample,
            total_snow = snow_rate * total_effort,
            snow_ratio = total_snow / sum(mt_wt, na.rm = T),
            dungeness_rate = sum(dungeness_count) / total_sample,
            total_dungeness = dungeness_rate * total_effort,
            dungeness_ratio = total_dungeness / sum(mt_wt, na.rm = T),
            halibut_rate = sum(halibut_count) / total_sample,
            total_halibut = halibut_rate * total_effort,
            halibut_ratio = total_halibut  / sum(mt_wt, na.rm = T),
            total_king = sum(king_count),
            king_ratio = total_king / sum(mt_wt, na.rm = T)) %>%
  left_join(ghl %>%
              dplyr::select(Season, District, ghl, tanner_cbl, snow_cbl, king_cbl)) -> tmp
### extract KNE bycatch summary table
tmp %>%
  filter(District == "KNE") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_halibut, 
         halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_KNE.csv") %>%
  # plot by annual totals in KNE by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[c(1, 2, 4)])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_KNE.png", plot = x,
       height = 6, width = 7, units = "in")   

### KSH bycatch summary table
tmp %>%
  filter(District == "KSH") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_KSH.csv") %>%
  # plot by annual totals in KSH by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_KSH.png", plot = x,
       height = 8, width = 7, units = "in") 

### KSW bycatch summary table
tmp %>%
  filter(District == "KSW") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_KSW.csv") %>%
  # plot by annual totals in KSW by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_KSW.png", plot = x,
       height = 8, width = 7, units = "in") 

tmp %>%
  filter(District == "KSE") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_KSE.csv")

### area M
### UB bycatch summary table
tmp %>%
  filter(District == "UB") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_snow, snow_ratio,
         total_king, king_ratio,  total_dungeness, dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_UB.csv") %>%
  # remove dungeness crab
  dplyr::select(-total_dungeness, -dungeness_ratio, -total_king, -king_ratio) %>%
  # plot by annual totals in Q by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_snow" ~ "Snow crab"),
         Species = factor(species, c("Tanner crab", "Snow crab", "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:5])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_UB.png", plot = x,
       height = 8, width = 7, units = "in")
### O bycatch summary table
tmp %>%
  filter(District == "O") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_O.csv") %>%
  # plot by annual totals in O by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_O.png", plot = x,
       height = 8, width = 7, units = "in")

### Q bycatch summary table
tmp %>%
  filter(District == "Q") %>%
  select(Season, ghl, tanner_cbl, snow_cbl, total_tanner, tanner_ratio, total_snow, snow_ratio,
         total_king, king_ratio,  total_dungeness, dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_Q.csv") %>%
  # remove dungeness crab
  dplyr::select(-total_dungeness, -dungeness_ratio) %>%
  # plot by annual totals in Q by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_snow" ~ "Snow crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Snow crab", "Red king crab", "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:5])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_Q.png", plot = x,
       height = 8, width = 7, units = "in")

### YAK bycatch summary table
tmp %>%
  filter(District == "YAK") %>%
  select(Season, ghl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_king, king_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer/2021/bycatch_summary_YAK.csv") %>%
  # plot by annual totals in YAK by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_totals_YAK.png", plot = x,
       height = 8, width = 7, units = "in") 


## meat weight:tanner crab bycatch ratio
### area K
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE", "KSEM")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:5])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/tanner_bycatch_ratio_area_K.png", plot = x,
       height = 3, width = 7, units = "in")          

### area M
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/tanner_bycatch_ratio_area_M.png", plot = x,
       height = 3, width = 7, units = "in") 

### UB
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("UB")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/tanner_bycatch_ratio_UB.png", plot = x,
       height = 3, width = 7, units = "in")

### area O
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("O")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/tanner_bycatch_ratio_area_O.png", plot = x,
       height = 3, width = 7, units = "in") 

### area Q
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            opilio_count = sum(opilio_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs,
         snow_rate = ifelse(sample_hrs == 0,
                            sum(opilio_count) / sum(sample_hrs),
                            opilio_count / sample_hrs),
         snow_catch = snow_rate * dredge_hrs,) %>%
  summarise(`Tanner crab` = sum(tanner_catch),
            `Snow crab` = sum(snow_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("Q")) %>%
  pivot_longer(c(3, 4), names_to = "species", values_to = "total_catch") %>%
  ggplot(aes(x = Season, y = total_catch / mt_wt, 
             group = species, color = species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:2])+
  labs(x = NULL, y = "Bycatch Ratio \n (crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/tanner_bycatch_ratio_area_Q.png", plot = x,
       height = 3, width = 7, units = "in") 

### YAK
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            dungeness_count = sum(dungeness_count),
            halibut_count = sum(halibut_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs,
         dungeness_rate = ifelse(sample_hrs == 0,
                              sum(dungeness_count) / sum(sample_hrs),
                              dungeness_count / sample_hrs),
         dungeness_catch = dungeness_rate * dredge_hrs,
         halibut_rate = ifelse(sample_hrs == 0,
                              sum(halibut_count) / sum(sample_hrs),
                              halibut_count / sample_hrs),
         halibut_catch = halibut_rate * dredge_hrs) %>%
  summarise(`Tanner crab` = sum(tanner_catch),
            `Dungeness crab` = sum(dungeness_catch),
            `Pacific Halibut`= sum(halibut_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("YAK")) %>%
  pivot_longer(c("Tanner crab", "Dungeness crab", "Pacific Halibut"), names_to = "Species", values_to = "catch") %>%
  mutate(Species = factor(Species, levels = c("Tanner crab", "Dungeness crab", "Pacific Halibut"))) %>%
  ggplot(aes(x = Season, y = catch / mt_wt, group = Species, color = Species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:3])+
  labs(x = NULL, y = "Bycatch Ratio (count : lbs scallop meat)", color = NULL)+
  facet_wrap(~Species, ncol = 1, scales = "free_y")+
  theme(legend.position = "none") -> x
ggsave("./figures/ghl_supplement/2021/bycatch_ratio_YAK.png", plot = x,
       height = 7, width = 7, units = "in") 


## daily bycatch of Tanner crab
### summarise crab and scallop daily catch 
bycatch_by_day %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  dplyr::select(Season, District, Set_date, tanner_catch, mt_wt) %>%
  # join to GHL and CBL
  left_join(ghl, by = c("District", "Season")) %>%
  # add cumulative proportion of GHL or CBL
  arrange(Set_date) %>%
  mutate(ghl_remain = (ghl - cumsum(mt_wt)) / ghl,
         cbl_remain = (tanner_cbl - cumsum(tanner_catch)) / tanner_cbl) %>%
  # pivot longer for plotting
  pivot_longer(c(ghl_remain, cbl_remain), 
               names_to = "bench", values_to = "remain") %>%
  # only retain the last 6 yrs
  filter(Set_date > lubridate::ymd("2015-07-01")) -> ghl_cbl_remain
### plot KNE
ghl_cbl_remain %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_KNE.png", plot = x,
       height = 6, width = 6, units = "in")
### plot KSH
ghl_cbl_remain %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_KSH.png", plot = x,
       height = 6, width = 6, units = "in")
### plot KSW
ghl_cbl_remain %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_KSW.png", plot = x,
       height = 6, width = 6, units = "in")
### plot UB
ghl_cbl_remain %>%
  filter(District == "UB") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = seq(-2, 1, 0.5), limits = c(-2, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_UB.png", plot = x,
       height = 6, width = 6, units = "in")
### plot O
ghl_cbl_remain %>%
  filter(District == "O") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_O.png", plot = x,
       height = 6, width = 6, units = "in")
### plot YAK
ghl_cbl_remain %>%
  filter(District == "YAK") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_YAK.png", plot = x,
       height = 6, width = 6, units = "in")
### plot Q
### recompute summary including snow crab
### summarise crab and scallop daily catch 
bycatch_by_day %>%
  filter(District == "Q") %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            opilio_count = sum(opilio_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs,
         snow_rate = ifelse(sample_hrs == 0,
                            sum(opilio_count) / sum(sample_hrs),
                            opilio_count / sample_hrs),
         snow_catch = snow_rate * dredge_hrs) %>%
  dplyr::select(Season, District, Set_date, tanner_catch, snow_catch, mt_wt) %>%
  # join to GHL and CBL
  left_join(ghl, by = c("District", "Season")) %>%
  # add cumulative proportion of GHL or CBL
  arrange(Set_date) %>%
  mutate(ghl_remain = (ghl - cumsum(mt_wt)) / ghl,
         tan_cbl_remain = (tanner_cbl - cumsum(tanner_catch)) / tanner_cbl,
         snow_cbl_remain = (snow_cbl - cumsum(snow_catch)) / snow_cbl) %>%
  # pivot longer for plotting
  pivot_longer(c(ghl_remain, tan_cbl_remain, snow_cbl_remain), 
               names_to = "bench", values_to = "remain") %>%
  mutate(bench = factor(bench, levels = c("tan_cbl_remain", "snow_cbl_remain", "ghl_remain"))) %>%
  # only retain the last 6 yrs
  filter(Set_date > lubridate::ymd("2014-07-01")) %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[c(1, 3, 2)], labels = c("Tanner CBL", "Snow CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/daily_tanner_cbl_Q.png", plot = x,
       height = 6, width = 6, units = "in")

## Tanner crab bycatch size composition by district
crab_size %>%
  mutate(cw_bin = cut(cw, breaks = seq(0, 250, 5), labels = F),
         cw_bin = seq(5, 250, 5)[cw_bin]) %>%
  group_by(Season, District,  Species, Sex, cw_bin) %>%
  summarise(num_crab = round(sum(samp_frac))) %>%
  mutate(year = as.numeric(substring(Season, 1, 4))) %>%
  filter(year >= 2011) -> tmp
### KNE district 
filter(tmp, District == "KNE") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_KNE.png", plot = x,
      height = 8, width = 6, units = "in")
### KSH district 
filter(tmp, District == "KSH") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_KSH.png", plot = x,
       height = 8, width = 6, units = "in")
### KSW district 
filter(tmp, District == "KSW") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_KSW.png", plot = x,
       height = 8, width = 6, units = "in")
### UB district 
filter(tmp, District == "UB") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_UB.png", plot = x,
       height = 8, width = 6, units = "in")
### O district 
filter(tmp, District == "O") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_O.png", plot = x,
       height = 8, width = 6, units = "in")
### YAK district 
filter(tmp, District == "YAK") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_YAK.png", plot = x,
       height = 8, width = 6, units = "in")
### Q district 
#### tanner crab
filter(tmp, District == "Q", Species == "Tanner crab") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/tanner_size_Q.png", plot = x,
       height = 8, width = 6, units = "in")
#### snow crab
filter(tmp, District == "Q", Species == "snow crab") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/snow_size_Q.png", plot = x,
       height = 8, width = 6, units = "in")

# discards ----

bycatch %>%
  group_by(Season, District, ADFG, Set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            disc_count = sum(disc_count, na.rm = T),
            disc_wt = sum(disc_wt, na.rm = T),
            broken_wt = sum(broken_wt, na.rm = T),
            rem_disc_wt = sum(rem_disc_wt, na.rm = T), .groups = "drop") -> discards_by_day

#### area K
discards_by_day %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE", "KSEM")) %>%
  # add dummy variable for incorrectly collected data
  mutate(nh = (District == "KSH" & Set_date >= "2018-07-14" & Set_date <= "2018-07-27")) %>%
  group_by(Season, District, nh) %>%
  nest() %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  unnest(summary) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = cb_palette[1:5])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:5])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/ghl_supplement/2021/scallop_discards_area_K.png", 
       plot = z, height = 6, width = 7, units = "in")

#### area M
discards_by_day %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/ghl_supplement/2021/scallop_discards_area_M.png", 
       plot = z, height = 6, width = 7, units = "in")

#### area O
discards_by_day %>%
  filter(District %in% c("O")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/ghl_supplement/2021/scallop_discards_area_O.png", 
       plot = z, height = 6, width = 7, units = "in")
#### area Q
discards_by_day %>%
  filter(District %in% c("Q")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/ghl_supplement/2021/scallop_discards_area_Q.png", 
       plot = z, height = 6, width = 7, units = "in")

#### area YAK
discards_by_day %>%
  filter(District %in% c("YAK")) %>%
  # add dummy variable for incorrectly collected data
  mutate(nh = (District == "YAK" & Set_date >= "2018-07-01" & Set_date <= "2018-07-07")) %>%
  group_by(Season, District, nh) %>%
  nest() %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  unnest(summary) %>%
  group_by(Season, District) %>%
  summarise(sum_effort = sum(effort),
            discard_rate_lb = weighted.mean(discard_rate_lb, w = effort),
            discard_lb = sum(discard_lb),
            disc_per_lb = weighted.mean(disc_per_lb, w = effort),
            discard_rate_num = weighted.mean(discard_rate_num, w = effort),
            discard_num = sum(discard_num)) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/ghl_supplement/2021/scallop_discards_area_YAK.png", 
       plot = z, height = 6, width = 7, units = "in")


### export tables by district
discards_by_day %>%
  mutate(nh = ifelse((District == "YAK" & Set_date >= "2018-07-01" & Set_date <= "2018-07-07"), T,
              ifelse((District == "KSH" & Set_date >= "2018-07-14" & Set_date <= "2018-07-27"), T, F))) %>%
  group_by(Season, District, nh) %>%
  nest() %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  unnest(c(summary)) %>%
  group_by(Season, District) %>%
  summarise(sum_effort = sum(effort),
            discard_rate_lb = weighted.mean(discard_rate_lb, w = effort),
            discard_lb = sum(discard_lb),
            disc_per_lb = weighted.mean(disc_per_lb, w = effort),
            discard_rate_num = weighted.mean(discard_rate_num, w = effort),
            discard_num = sum(discard_num)) %>%
  rename(effort = sum_effort) -> tmp
  
# join discard info to catch
catch %>%
  group_by(Season, District) %>%
  summarise(round_weight = sum(round_weight, na.rm = T)) %>%
  # join with 'tmp' (discard estimates)
  right_join(tmp, by = c("District", "Season")) %>%
  # compute discard lbs ratio and overwrite object 'tmp'
  mutate(discard_ratio = discard_lb / round_weight,
         discard_M_lbs = discard_lb * 0.2,
         discard_M_num = discard_num * 0.2) -> tmp
#### KNE
tmp %>%
  filter(District == "KNE") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_KNE.csv")
#### KSH
tmp %>%
  filter(District == "KSH") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_KSH.csv")
#### KSW
tmp %>%
  filter(District == "KSW") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_KSW.csv")
#### KSE
tmp %>%
  filter(District == "KSE") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_KSE.csv")
#### KSEM
tmp %>%
  filter(District == "KSEM") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_KSEM.csv")
#### area K discard ratio plot
tmp %>% 
  filter(District %in% c("KNE", "KSH", "KSW", "KSE", "KSEM")) %>%
  mutate(District = factor(District, levels = c("KNE", "KSH", "KSW", "KSE", "KSEM"))) %>%
  ggplot(aes(x = Season, y = discard_ratio, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 1, linetype = 2)+
  scale_colour_manual(values = cb_palette[1:5])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_ratio_area_K.png", plot = x,
       height = 3, width = 7, units = "in") 

#### UB
tmp %>%
  filter(District == "UB") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>% 
  write_csv("./output/observer/2021/discard_summary_UB.csv")
#### C
tmp %>%
  filter(District == "C") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_C.csv")
#### area M discrad ratio plot
tmp %>% 
  filter(District %in% c("UB", "C", "WC")) %>%
  mutate(District = factor(District, levels = c("UB", "C", "WC")),
         discard_ratio = ifelse(round_weight == 0, NA, discard_ratio)) %>%
  ggplot(aes(x = Season, y = discard_ratio, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 1, linetype = 2)+
  scale_colour_manual(values = cb_palette[1:3])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_ratio_area_M.png", plot = x,
       height = 3, width = 7, units = "in") 

#### O
tmp %>%
  filter(District == "O") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_O.csv")
#### area O discrad ratio plot
tmp %>%
  filter(District == "O") %>%
  mutate(discard_ratio = ifelse(round_weight == 0, NA, discard_ratio)) %>%
  ggplot(aes(x = Season, y = discard_ratio, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_ratio_area_O.png", plot = x,
       height = 3, width = 7, units = "in") 

#### Q
tmp %>%
  filter(District == "Q") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_Q.csv")
#### area Q discrad ratio plot
tmp %>%
  filter(District == "Q") %>%
  ggplot(aes(x = Season, y = discard_ratio, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_ratio_area_Q.png", plot = x,
       height = 3, width = 7, units = "in")


#### YAK
tmp %>%
  filter(District == "YAK") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num, discard_M_lbs, discard_M_num) %T>%
  write_csv("./output/observer/2021/discard_summary_YAK.csv")
tmp %>%
  filter(District == "YAK") %>%
  ggplot(aes(x = Season, y = discard_ratio, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_ratio_area_YAK.png", plot = x,
       height = 3, width = 7, units = "in")

## Ratio of discards (intact vs broken)
### KNE
discards_by_day %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_KNE.png",
       plot = x, height = 3, width = 7, units = "in")

### KSH
discards_by_day %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_KSH.png",
       plot = x, height = 3, width = 7, units = "in")

### KSW
discards_by_day %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_KSW.png",
       plot = x, height = 3, width = 7, units = "in")
### M
discards_by_day %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_M.png",
       plot = x, height = 3, width = 7, units = "in")

### O
discards_by_day %>%
  filter(District %in% c("O")) %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_O.png",
       plot = x, height = 3, width = 7, units = "in")

### Q
discards_by_day %>%
  filter(District %in% c("Q")) %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_Q.png",
       plot = x, height = 3, width = 7, units = "in")

### YAK
discards_by_day %>%
  filter(District %in% c("YAK")) %>%
  ggplot()+
  geom_boxplot(aes(x = Season, y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
  geom_hline(yintercept = 1, linetype = 2)+
  labs(x = NULL, y = "Intact : broken discard ratio") -> x
ggsave("./figures/ghl_supplement/2021/scallop_discard_broken_intact_ratio_YAK.png",
       plot = x, height = 3, width = 7, units = "in")


# clappers ----
## compute clapper number
bycatch %>%
  group_by(Season, District, Set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            clapper_count = sum(clapper_count, na.rm = T), .groups = "drop") -> clapper_by_day 

clapper_by_day %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            clapper_rate = sum(clapper_count) / sum(sample_hrs),
            clapper_est = clapper_rate * effort) -> tmp
### area K
tmp %>%
  filter(District %in% c("KNE", "KSH", "KSE", "KSW", "KSEM")) %T>%
  write_csv("./output/observer/2021/clappers_area_K.csv") %>%
  ggplot(aes(x = Season, y = clapper_rate, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:5])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
ggsave("./figures/ghl_supplement/2021/clappers_area_K.png", plot = x,
       height = 3, width = 7, units = "in")

### area M
tmp %>%
  filter(District %in% c("WC", "C", "UB")) %T>%
  write_csv("./output/observer/2021/clappers_area_M.csv") %>%
  ggplot(aes(x = Season, y = clapper_rate, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:3])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
ggsave("./figures/ghl_supplement/2021/clappers_area_M.png", plot = x,
       height = 3, width = 7, units = "in")

### area O
tmp %>%
  filter(District %in% c("O")) %T>%
  write_csv("./output/observer/2021/clappers_area_O.csv") %>%
  ggplot(aes(x = Season, y = clapper_rate, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
ggsave("./figures/ghl_supplement/2021/clappers_area_O.png", plot = x,
       height = 3, width = 7, units = "in")

### area Q
tmp %>%
  filter(District %in% c("Q")) %T>%
  write_csv("./output/observer/2021/clappers_area_Q.csv") %>%
  ggplot(aes(x = Season, y = clapper_rate, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
ggsave("./figures/ghl_supplement/2021/clappers_area_Q.png", plot = x,
       height = 3, width = 7, units = "in")

### area D
tmp %>%
  filter(District %in% c("YAK")) %T>%
  write_csv("./output/observer/2021/clappers_area_YAK.csv") %>%
  ggplot(aes(x = Season, y = clapper_rate, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
ggsave("./figures/ghl_supplement/2021/clappers_area_YAK.png", plot = x,
       height = 3, width = 7, units = "in")

# shell height composition ----
## create dataset with SH and appropriate weights
bycatch %>%
  # compute a daily discard rate (lbs/dregde hr)
  group_by(Season, District, Set_date) %>%
  summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
            sample = sum(sample_hrs)) %>%
  group_by(Season, District) %>%
  mutate(disc_rate = ifelse(sample != 0, 
                            disc_wt / sample, 
                            sum(disc_wt) / sum(sample))) %>%
  # join to catch data by haul
  dplyr::select(Season, District, Set_date, disc_rate) %>%
  right_join(catch, by = c("Season", "District", "Set_date")) %>%
  # estimate discards by haul
  mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
  # estimate weights for shell height histogram (prop of annual catch)
  dplyr::select(Season, District, Haul_ID, round_weight, disc_est_lbs) %>%
  pivot_longer(c(round_weight, disc_est_lbs), 
               names_to = "Rtnd_disc", values_to = "wt_lbs") %>%
  mutate(Rtnd_disc = ifelse(Rtnd_disc == "round_weight", "R", "D"),
         w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(Haul_ID, Rtnd_disc, w) %>%
right_join(shell_height %>% filter(Rtnd_disc != "M"), by = c("Haul_ID", "Rtnd_disc")) -> tmp
# KNE
tmp %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_KNE.png", plot = x,
       height = 6, width = 7, units = "in")
# KSH
tmp %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_KSH.png", plot = x,
       height = 6, width = 7, units = "in")
# KSW
tmp %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_KSW.png", plot = x,
       height = 6, width = 7, units = "in")
# KSE
tmp %>%
  filter(District == "KSE") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_KSE.png", plot = x,
       height = 3, width = 5, units = "in")
# UB
tmp %>%
  filter(District == "UB") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_UB.png", plot = x,
       height = 6, width = 7, units = "in")
# C
tmp %>%
  filter(District == "C") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_C.png", plot = x,
       height = 6, width = 7, units = "in")
# area M
tmp %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_area_M.png", plot = x,
       height = 6, width = 7, units = "in")
# area O
tmp %>%
  filter(District == "O") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_O.png", plot = x,
       height = 6, width = 7, units = "in")
# area Q
tmp %>%
  filter(District == "Q") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_Q.png", plot = x,
       height = 6, width = 7, units = "in")
# area YAK
tmp %>%
  filter(District == "YAK") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_YAK.png", plot = x,
       height = 6, width = 7, units = "in")
# EKI
tmp %>%
  filter(District == "EKI") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_EKI.png", plot = x,
       height = 2, width = 7, units = "in")
# WKI
tmp %>%
  filter(District == "WKI") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_WKI.png", plot = x,
       height = 3, width = 5, units = "in")



# gonads ----
## KNE
meat %>%
  filter(District == "KNE", 
         !is.na(gonad_cond)) %>%
  group_by(Season) %>%
  mutate(tot = n(),
         Gonad = case_when(gonad_cond == 1 ~ "Empty",
                           gonad_cond == 2 ~ "Intitial Recovery",
                           gonad_cond == 3 ~ "Filling",
                           gonad_cond == 4 ~ "Full"))  %>%
  group_by(Gonad, Season, tot) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / tot) %>%
  ggplot()+
  geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
  scale_fill_manual(values = cb_palette[c(2, 4, 7, 8)])+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave("./figures/ghl_supplement/2021/gonad_KNE.png", plot = x, 
       height = 4, width = 5, units = "in")

## KSH
meat %>%
  filter(District == "KSH", 
         !is.na(gonad_cond)) %>%
  group_by(Season) %>%
  mutate(tot = n(),
         Gonad = case_when(gonad_cond == 1 ~ "Empty",
                           gonad_cond == 2 ~ "Intitial Recovery",
                           gonad_cond == 3 ~ "Filling",
                           gonad_cond == 4 ~ "Full"))  %>%
  group_by(Gonad, Season, tot) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / tot) %>%
  ggplot()+
  geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
  scale_fill_manual(values = cb_palette[c(2, 4, 7, 8)])+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave("./figures/ghl_supplement/2021/gonad_KSH.png", plot = x, 
       height = 4, width = 5, units = "in")

## KSW
meat %>%
  filter(District == "KSW", 
         !is.na(gonad_cond)) %>%
  group_by(Season) %>%
  mutate(tot = n(),
         Gonad = case_when(gonad_cond == 1 ~ "Empty",
                           gonad_cond == 2 ~ "Intitial Recovery",
                           gonad_cond == 3 ~ "Filling",
                           gonad_cond == 4 ~ "Full"))  %>%
  filter(!is.na(Gonad)) %>%
  group_by(Gonad, Season, tot) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / tot) %>%
  ggplot()+
  geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
  scale_fill_manual(values = cb_palette[c(2, 4, 8)])+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave("./figures/ghl_supplement/2021/gonad_KSW.png", plot = x, 
       height = 4, width = 5, units = "in")

# 
# ## M
# meat %>%
#   filter(District %in% c("WC", "C", "UB"), 
#          !is.na(gonad_cond)) %>%
#   group_by(Season) %>%
#   mutate(tot = n(),
#          Gonad = case_when(gonad_cond == 1 ~ "Empty",
#                            gonad_cond == 2 ~ "Intitial Recovery",
#                            gonad_cond == 3 ~ "Filling",
#                            gonad_cond == 4 ~ "Full"))  %>%
#   group_by(Gonad, Season, tot) %>%
#   summarise(prop = n()) %>%
#   mutate(prop = prop / tot) %>%
#   ggplot()+
#   geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
#   scale_fill_manual(values = cb_palette[c(2, 4, 7, 8)])+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   labs(x = NULL, y = "Proportion", fill = NULL) -> x
# ggsave("./figures/ghl_supplement/2021/gonad_M.png", plot = x, 
#        height = 4, width = 3, units = "in")
# 
# ## O
# meat %>%
#   filter(District == "O", 
#          !is.na(gonad_cond)) %>%
#   group_by(Season) %>%
#   mutate(tot = n(),
#          Gonad = case_when(gonad_cond == 1 ~ "Empty",
#                            gonad_cond == 2 ~ "Intitial Recovery",
#                            gonad_cond == 3 ~ "Filling",
#                            gonad_cond == 4 ~ "Full"))  %>%
#   group_by(Gonad, Season, tot) %>%
#   summarise(prop = n()) %>%
#   mutate(prop = prop / tot) %>%
#   ggplot()+
#   geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
#   scale_fill_manual(values = cb_palette[c(2, 4, 7, 8)])+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   labs(x = NULL, y = "Proportion", fill = NULL) -> x
# ggsave("./figures/ghl_supplement/2021/gonad_O.png", plot = x, 
#        height = 4, width = 3, units = "in")
# 
# ## Q
# meat %>%
#   filter(District == "Q", 
#          !is.na(gonad_cond)) %>%
#   group_by(Season) %>%
#   mutate(tot = n(),
#          Gonad = case_when(gonad_cond == 1 ~ "Empty",
#                            gonad_cond == 2 ~ "Intitial Recovery",
#                            gonad_cond == 3 ~ "Filling",
#                            gonad_cond == 4 ~ "Full"))  %>%
#   group_by(Gonad, Season, tot) %>%
#   summarise(prop = n()) %>%
#   mutate(prop = prop / tot) %>%
#   ggplot()+
#   geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
#   scale_fill_manual(values = cb_palette[c(2, 4, 7, 8)])+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE))+
#   labs(x = NULL, y = "Proportion", fill = NULL) -> x
# ggsave("./figures/ghl_supplement/2021/gonad_Q.png", plot = x, 
#        height = 4, width = 3, units = "in")

## YAK
meat %>%
  filter(District == "YAK", 
         !is.na(gonad_cond)) %>%
  group_by(Season) %>%
  mutate(tot = n(),
         Gonad = case_when(gonad_cond == 1 ~ "Empty",
                           gonad_cond == 2 ~ "Intitial Recovery",
                           gonad_cond == 3 ~ "Filling",
                           gonad_cond == 4 ~ "Full",
                           gonad_cond == 5 ~ "Unknown",
                           gonad_cond == 0 ~ "Immature"))  %>%
  group_by(Gonad, Season, tot) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / tot) %>%
  ggplot()+
  geom_bar(aes(x=Season, y = prop, fill=factor(Gonad)), stat = "identity")+
  scale_fill_manual(values = cb_palette[c(2, 4, 7, 1, 8, 3)])+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(x = NULL, y = "Proportion", fill = NULL) -> x
ggsave("./figures/ghl_supplement/2021/gonad_YAK.png", plot = x, 
       height = 4, width = 5, units = "in")


# meat weight ~ shell height ----
## plot meat weight ~ shell height by district
### Area K
#### fit model to linearized data without random effect
meat %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE", "KSEM"),
         Type == "retained",
         !is.na(shell_height), !is.na(meat_weight)) -> k_mw
lm(log(meat_weight) ~ log(shell_height) * Season, data = k_mw) -> k_lm
summary(k_lm)
#### fit model to linearized data with random effect
nlme::lme(log(meat_weight) ~ log(shell_height) * Season, 
          random = ~1|District, data = k_mw, method = "ML") -> k_lme

#### compare models
AIC(k_lm, k_lme) # use k_lme
#### refit k_lme using REML
nlme::lme(log(meat_weight) ~ log(shell_height) + Season, 
          random = ~1 |District, data = k_mw, method = "REML") -> k_lme
summary(k_lme)

# plot
## create fitted data for line
k_mw  %>%
  mutate(fit_mw = predict(k_lme)) %>%
  ggplot()+
  geom_point(aes(x = shell_height, y = meat_weight, color = Season), alpha = 0.2)+
  geom_line(aes(x = shell_height, y = exp(fit_mw), color = Season))+
  scale_color_manual(values = cb_palette[c(2:5, 8)])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL)+
  facet_wrap(~District) -> x
ggsave("./figures/ghl_supplement/2021/mw_sh_area_K.png", plot = x, height = 5, width = 5, units = "in")
    

### Area D
#### fit model to linearized data without random effect
meat %>%
  filter(District %in% c("YAK"),
         Type == "retained",
         !is.na(shell_height), !is.na(meat_weight)) -> d_mw
lm(log(meat_weight) ~ log(shell_height) * Season, data = d_mw) -> d_lm
summary(d_lm)
# plot
## create fitted data for line
d_mw  %>%
  mutate(fit_mw = predict(d_lm)) %>%
  ggplot()+
  geom_jitter(aes(x = shell_height, y = meat_weight, color = Season), alpha = 0.5)+
  geom_line(aes(x = shell_height, y = exp(fit_mw), color = Season))+
  scale_color_manual(values = cb_palette[7:6])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL)+
  facet_wrap(~District) -> x
ggsave("./figures/observer_data_report/2020/mw_sh_area_YAK.png", plot = x, height = 3, width = 3, units = "in")           

# Area D with bed random effect
#### fit model to linearized data with random intercept and/or slope by bed
lme(log(meat_weight) ~ log(shell_height) * Season, random = ~ 1 | bed_code,
    method = "ML", data = d_mw) -> d_lme
summary(d_lme)
lme(log(meat_weight) ~ log(shell_height) * Season, 
    random = ~ shell_height - 1|bed_code, method = "ML", data = d_mw) -> d_lme2
summary(d_lme2)
lme(log(meat_weight) ~ log(shell_height) * Season, 
    random = ~1 + shell_height|bed_code, data = d_mw, method = "ML") -> d_lme3
summary(d_lme3) # really high correlation
AIC(d_lm, d_lme, d_lme2, d_lme3) # due to correlation go with random intercept model

# refit best model with REML
lme(log(meat_weight) ~ log(shell_height) * Season, 
    random = ~1 | bed_code, data = d_mw, method = "REML") -> d_lme
summary(d_lme) 

# main plot
d_mw  %>%
  mutate(fit_mw = predict(d_lme)) %>%
  ggplot()+
  geom_jitter(aes(x = shell_height, y = meat_weight, color = Season), alpha = 0.2)+
  geom_line(aes(x = shell_height, y = exp(fit_mw), color = Season))+
  scale_color_manual(values = cb_palette[c(2:5, 8)])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL)+
  facet_wrap(~bed_code) -> x
ggsave("./figures/ghl_supplement/2021/mw_sh_area_YAK_lme.png", plot = x, 
       height = 7, width = 7, units = "in")           



# meat weight ~ round weight ----
## plot of all data
catch %>%
  ggplot()+
  geom_point(aes(x = round_weight, y = meat_weight), color = cb_palette[1], alpha = 0.2)+
  geom_smooth(aes(x = round_weight, y = meat_weight), color = cb_palette[6], method = "loess", se = F)+
  geom_smooth(aes(x = round_weight, y = 0.1 * round_weight), color = 1, method = "lm", se = F, linetype = 2)+
  coord_cartesian(ylim = c(0, 600), expand = c(0, 0))+
  labs(x = "Round weight (lbs)", y = "Meat weight (lbs)") -> x
ggsave("./figures/ghl_supplement/2021/mw_rw_scatterplot.png", plot = x,
       height = 4, width = 6, units = "in")
# voilin plots by district and season
catch %>%
  filter(!(District %in% c("C", "KSEM")),
         !is.na(District)) %>%
  mutate(District = factor(District, levels = c("KNE", "KSH", "KSW", "KSE", "UB", 
                                                "O", "Q", "WKI", "EKI", "YAK"))) %>%
  ggplot()+
  geom_violin(aes(x = Season, y = meat_weight / round_weight), adjust = 1.5, fill = cb_palette[4])+
  geom_boxplot(aes(x = Season, y = meat_weight / round_weight), outlier.shape = NA, width = 0.2)+
  coord_cartesian(ylim = c(0, 0.2))+
  geom_hline(yintercept = 0.1, linetype = 2)+
  facet_wrap(~District, ncol = 2)+
  labs(x = NULL, y = "Meat weight : Round weight")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./figures/ghl_supplement/2021/mw_rw_violin.png", plot = x,
       height = 8, width = 7, units = "in")
  



# statewide fishing performance ----

## CPUE
## read data
files <- grep("standardized_cpue_season", 
              list.files("./output/observer/2021"), 
              value = T)
lapply(paste0("./output/observer/2021/", files), read_csv) -> tmp

## plot
tibble(data = tmp,
       district = unlist(str_extract_all(files, "([A-Z]+(?=[^a-z]))"))) %>%
  unnest(data) %>%
  mutate(Season = factor(Season)) %>%
  ggplot()+
  geom_hline(aes(yintercept = mean(std_cpue)), linetype = 2, color = cb_palette[1])+
  geom_point(aes(x = Season, y = std_cpue, color = district))+
  geom_smooth(aes(x = as.numeric(Season), y = std_cpue), color = 1, se = F)+
  labs(x = NULL, y = "Standardized CPUE (lbs / dredge hr)", color = NULL) -> x
ggsave("./figures/ghl_supplement/2021/statewide_cpue_trend.png", plot = x,
       height = 4, width = 7, units = "in")

## Discards
## read data
files <- grep("discard_summary", 
              list.files("./output/observer/2021"), 
              value = T)
lapply(paste0("./output/observer/2021/", files), read_csv) -> tmp
## plot
tibble(data = tmp,
       district = unlist(str_extract_all(files, "([A-Z]+(?=[^a-z]))"))) %>%
  unnest(data) %>%
  mutate(Season = factor(Season),
         discard_ratio = ifelse(round_weight == 0, NA, discard_ratio)) %>%
  ggplot()+
  geom_hline(aes(yintercept = mean(discard_ratio)), linetype = 2, color = cb_palette[1])+
  geom_point(aes(x = Season, y = discard_ratio, color = district))+
  geom_smooth(aes(x = as.numeric(Season), y = discard_ratio), color = 1, se = F)+
  labs(x = NULL, y = "Discard ratio (lbs discarded:lbs retained)", color = NULL) -> x
ggsave("./figures/ghl_supplement/2021/statewide_disc_ratio.png", plot = x,
       height = 4, width = 7, units = "in")

## Shell Height Comp
## create dataset with SH and appropriate weights
discards_by_day %>%
  # compute a daily discard rate (lbs/dregde hr)
  group_by(Season, District, Set_date) %>%
  summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
            sample = sum(sample_hrs)) %>%
  group_by(Season, District) %>%
  mutate(disc_rate = ifelse(sample != 0, 
                            disc_wt / sample, 
                            sum(disc_wt) / sum(sample))) %>%
  # join to catch data by haul
  dplyr::select(Season, District, Set_date, disc_rate) %>%
  right_join(catch, by = c("Season", "District", "Set_date")) %>%
  # estimate discards by haul
  mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
  # estimate weights for shell height histogram (prop of annual catch)
  dplyr::select(Season, District, Haul_ID, round_weight, disc_est_lbs) %>%
  pivot_longer(c(round_weight, disc_est_lbs), 
               names_to = "Rtnd_disc", values_to = "wt_lbs") %>%
  mutate(Rtnd_disc = ifelse(Rtnd_disc == "round_weight", "R", "D"),
         w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(Haul_ID, Rtnd_disc, w) %>%
  right_join(shell_height, by = c("Haul_ID", "Rtnd_disc")) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3, dir = "v") -> x
ggsave("./figures/ghl_supplement/2021/sh_comp_statewide.png", plot = x,
       height = 6, width = 7, units = "in")
