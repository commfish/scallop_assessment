# notes ----
## scallop SAFE ecosystem
## tyler jackson
## 11/24/2021

# load ----
library(tidyverse)
library(lubridate)
library(FNGr)
library(patchwork)
library(RColorBrewer)

## set ggplot theme
theme_set(theme_sleek())

## ggplot color pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
yr_axis <- tickr(tibble(year = 1988:2021), year, 5)

### general observer data functions and map functions
source("./code/misc/general_observer_data_functions.R")
source("./code/misc/adfg_map_functions.R")

# custom function to correct timeseries stations for westward trawl survey
f_timeseries_station_correct <- function(data, select){
  
  # data for function
  std_stations <- read_csv("data/westward_standard_lm_survey/tanner_standard_stations.csv")
  
  # change data object name to avoiding masking
  dat = data
  
  # minor function needed within function
  ## pull mean from reference data 
  f_pmfd <- function(input, data, select) {
    # change names to avoid masking
    input %>%
      rename(stn = station, 
             yr = year) -> input
    
    # fill in data for stations two in a given year
    if(input$stn %in% (data %>% filter(year == input$yr) %>% pull(station))) {
      data %>%
        filter(year == input$yr, station == input$stn) %>%
        pull(select) %>%
        .[1] -> out
    }
    # fill in data for stations that need avg from references in given year
    if(!(input$stn %in% (data %>% filter(year == input$yr) %>% pull(station)))) {
      data %>%
        filter(year == input$yr, station %in% c(input$stn, input$reference_station, input$reference_station2, input$reference_station3)) %>%
        pull(select) %>%
        mean -> res
      out <- ifelse(is.nan(res), NA, res)
      
    }
    
    # return 
    return(out)
  }
  
  expand_grid(year = min(data$year):max(data$year), std_stations) %>%
    # remove the text stations
    dplyr::select(-district, -section, -subsection) %>%
    # create dummy column year/station
    mutate(yrstn = paste(year, station, sep = "_")) %>%
    group_by(yrstn) %>%
    nest %>%
    mutate(select = purrr::map_dbl(data, f_pmfd, data = dat, select = select)) %>%
    unnest() %>%
    ungroup() %>%
    dplyr::select(year, station, select) -> out
  
  names(out)[3] <- select
  
  return(out)
  
}

# scallop registration areas
f_shp_prep("./data/maps/mgmt_units", "Scallop_Statewide_Areas_PY", fortify = F) %>%
  spTransform(., CRS(proj4string(raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")))) -> reg_areas

# scallop bed polygons
f_shp_prep("./data/maps/bed_bounding_boxes", "bed_bounding_box", fortify = F) %>%
  spTransform(., CRS(proj4string(raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")))) -> bed_boxes

# westward trawl data ----

## standard stations
std_stations <- read_csv("./data/westward_standard_lm_survey/tanner_standard_stations.csv")

## catch data
wslm_catch <- do.call("rbind", 
                     lapply(list.files(path = "./data/westward_standard_lm_survey/catch_dump",
                                       pattern = ".csv", full.names = T), 
                            read_csv, col_types = cols())) %>%
             rename_all(tolower)
## haul data
wlsm_haul <- do.call("rbind", 
                     lapply(list.files(path = "./data/westward_standard_lm_survey/haul_info",
                                       pattern = ".csv", full.names = T), 
                            read_csv, col_types = cols())) %>%
             rename_all(tolower)


# central region trawl data ----

## chlamys cpue by haul data
read.csv("./data/central_trawl_survey/Central Region LgMesh Trawl Chlamys CPUE 2000_2019.csv") %>%
  as_tibble %>%
  rename_all(tolower) %>%
  # add scallop registration area and district
  mutate(scallop_area = case_when(grepl("Kachemak|Kamishak", project_name) ~ "H",
                          grepl("Prince William Sound", project_name) ~ "E"),
         district = case_when(grepl("Kamishak", project_name) ~ "Kamishak",
                              grepl("Kachemak", project_name) ~ "Southern",
                              grepl("Prince William Sound", project_name) ~ "Inside")) %>%
  group_by(year, scallop_area) %>%
  summarise(cpue_t_km2 = mean(kg.km.2) / 1000,
            se = sqrt((var(kg.km.2) / 1000^2) / n()),
            cv = se / cpue_t_km2) -> central_trawl_chlamys


# noaa race surveys data ----

## hauls
race_goa <- read.csv("./data/race_goa_survey/goa_race_hauls.csv") %>%
  as_tibble() %>%
  rename_all(function(x){gsub(" ", "_", tolower(x))}) %>%
  rename_all(function(x){gsub("\\(|\\)", "", x)}) 
race_ebs <- read.csv("./data/race_ebs_survey/ebs_race_hauls.csv") %>%
  as_tibble() %>%
  rename_all(function(x){gsub(" ", "_", tolower(x))}) %>%
  rename_all(function(x){gsub("\\(|\\)", "", x)})

## bulk cpue data
race_goa_catch <- read.csv("./data/race_goa_survey/goa_race_catch_by_haul.csv", skip = 7) %>%
  as_tibble() %>%
  rename_all(function(x){gsub(" ", "_", tolower(x))}) %>%
  rename_all(function(x){gsub("\\(|\\)", "", x)}) 
race_ebs_catch <- read.csv("./data/race_ebs_survey/ebs_race_catch_by_haul.csv",skip = 7) %>%
  as_tibble() %>%
  rename_all(function(x){gsub(" ", "_", tolower(x))}) %>%
  rename_all(function(x){gsub("\\(|\\)", "", x)}) 

# fishery data ----

## corrected season, district, and haul id data
lapply(list.files("./data/observer/catch", pattern = "CatchByHaul", full.names = T), read.csv) %>% 
  do.call("bind_rows", .) %>% as_tibble() %>%
  drop_na(Haul_ID) %>%
  f_catch_rename %>%
  ## add Season to data
  f_add_season %>%
  ## classify Karluk bed as KSW district instead of KSH
  f_revise_district %>% 
  ## coerce date to date class
  mutate(Set_date = lubridate::mdy(Set_date)) %>%
  ## remove tows with zero dredge hours (logbook mistake)
  filter(dredge_hrs != 0) %>%
  ## select columns to join
  dplyr::select(Season, Haul_ID, District, dredge_hrs, haul_sampled) %>%
  rename_all(tolower) -> obs_haul_info

## fishery catch comp
lapply(list.files("./data/observer/catch_comp", pattern = "haulComp", full.names = T), read.csv) %>% 
  do.call("bind_rows", .) %>% as_tibble() %>%
  dplyr::select(Haul_ID, RACE_Code, Species_Name, `Weight.lbs.`) %>%
  rename_all(tolower) %>%
  right_join(obs_haul_info, by = "haul_id") %>%
  rename(wt_lb = `weight.lbs.`) -> fish_catch_comp
 
  


# bottom temperature ----
## westward trawl survey
wlsm_haul %>%
  left_join(std_stations %>%
              dplyr::select(station, scallop_index_dist_full)) %>%
  mutate(year = year(mdy(haul_date))) %>%
  filter(!is.na(scallop_index_dist_full)) %>%
  group_by(year, scallop_index_dist_full) %>%
  summarise(bt = mean(temperature, na.rm = T)) %>%
  
  ggplot()+
  geom_point(aes(x = year, y = bt, color = scallop_index_dist_full))+
  geom_line(aes(x = year, y = bt, color = scallop_index_dist_full))+
  labs(x = NULL, color = NULL, y = expression("Bottom Temperature  " (degree~C)))+
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels, limits = c(1990, NA))+
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(2, 10))+
  scale_color_manual(values = cb_palette[2:7]) -> westward_trawl_temp_plot

## race goa trawl survey
bind_rows(race_goa, race_ebs) %>%
  mutate(lat = (starting_latitude_dd + ending_latitude_dd) / 2,
         lon = (starting_longitude_dd + ending_longitude_dd) / 2) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(reg_area = f_over(x = ., y = reg_areas, label = "Area_Name"),
         bed = f_over(x = ., y = bed_boxes, label = "poly_name"),
         reg_area = ifelse(reg_area == "Bristol Bay - Bering Sea" & is.na(bed), NA, reg_area),
         reg_area = gsub("Bristol Bay - ", "", reg_area),
         year = as.numeric(substring(cruise_number, 1, 4))) %>%
  filter(!is.na(reg_area)) %>%
  group_by(year, reg_area) %>%
  summarise(bt = mean(gear_temperature_c, na.rm = T)) %>%
  
  ggplot()+
  geom_point(aes(x = year, y = bt, color = reg_area))+
  geom_line(aes(x = year, y = bt, color = reg_area))+
  labs(x = NULL, color = NULL, y = expression("Bottom Temperature  " (degree~C)))+
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels, limits = c(1990, NA))+
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(2, 10))+
  scale_color_manual(values = cb_palette[1:8]) -> race_temp_plot

westward_trawl_temp_plot / race_temp_plot

# non target scallops in adfg trawl surveys ----

## westward trawl survey
wslm_catch %>%
  rename_all(tolower) %>%
  # add year and area swept (km2), cpue
  mutate(year = as.numeric(substring(survey, 5, 8)),
         area_swept = `distance(km)` * 0.0122,
         cpue = `final_wt(kg)` / area_swept / 1000) %>%
  # filter for chlamys scallops
  filter(race_code %in% c(74100, 74103:74109, 74112, 74175)) %>% 
  f_timeseries_station_correct(., select = "cpue") %>%
  replace_na(list(cpue = 0)) %>%
  left_join(std_stations %>%
              dplyr::select(station, scallop_area, area_nmi2), 
            by = "station") %>%
  group_by(year, scallop_area) %>%
  # annual cpue computed as mean (weighted station design)
  summarise(cpue_t_km2 = weighted.mean(cpue, area_nmi2, na.rm = T),
            se = sqrt(modi::weighted.var(cpue, w = area_nmi2, na.rm = T)),
            cv = se / cpue_t_km2) -> westward_trawl_chlamys

## central region trawl survey
head(central_trawl_chlamys)

## print adfg race non-target scallop CPUE by area
bind_rows(westward_trawl_chlamys, central_trawl_chlamys) %>%
  write_csv("./output/fishery_independent/2021/adfg_trawl_non_target_scallop_cpue.csv")



# non target scallops race surveys ----
race_ebs_catch %>%
  filter(species.code %in% c(74100, 74103:74109, 74112, 74175)) %>%
  mutate(wt_t = weight..kg. / 1000) %>%
  group_by(year, haul.join.id) %>%
  summarise(wt_t = sum(wt_t)) -> ebs_survey_catch

race_ebs %>%
  mutate(area_swept_km2 = (net.width..m. / 1000) * distance.fished..km.,
         lat = (starting.latitude..dd. + ending.latitude..dd.) / 2,
         lon = (starting.longitude..dd. + ending.longitude..dd.) / 2) %>%
  dplyr::select(haul.join.id, year, area_swept_km2, lon, lat) %>%
  # join to survey catch
  left_join(ebs_survey_catch, by = c("haul.join.id", "year")) %>%
  replace_na(list(wt_t = 0)) %>%
  mutate(cpue_t = wt_t / area_swept_km2,
         reg_area = "Q") %>%
  # compute cpue (t / km2)
  group_by(year, reg_area) %>%
  summarise(cpue = mean(cpue_t, na.rm = T),
            se = sqrt(var(cpue_t, na.rm = T) / n()),
            cv = se / cpue) -> race_ebs_chlamys

## noaa goa trawl survey
race_goa_catch %>%
  filter(species.code %in% c(74100, 74103:74109, 74112, 74175)) %>%
  mutate(wt_t = weight..kg. / 1000) %>%
  group_by(year, haul.join.id) %>%
  summarise(wt_t = sum(wt_t)) -> goa_survey_catch

race_goa %>%
  mutate(area_swept_km2 = (net.width..m. / 1000) * distance.fished..km.,
         lat = (starting.latitude..dd. + ending.latitude..dd.) / 2,
         lon = (starting.longitude..dd. + ending.longitude..dd.) / 2) %>%
  dplyr::select(haul.join.id, year, area_swept_km2, lon, lat) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  mutate(reg_area = f_over(x = ., y = reg_areas, label = "Area_Lette")) %>%
  # join to survey catch
  left_join(goa_survey_catch, by = c("haul.join.id", "year")) %>%
  replace_na(list(wt_t = 0)) %>%
  mutate(cpue_t = wt_t / area_swept_km2) %>%
  # compute cpue (t / km2)
  group_by(year, reg_area) %>%
  summarise(cpue = mean(cpue_t, na.rm = T),
            se = sqrt(var(cpue_t, na.rm = T) / n()),
            cv = se / cpue) -> race_goa_chlamys

## print NOAA race non-target scallop CPUE by area
bind_rows(race_ebs_chlamys, race_goa_chlamys) %>%
  write_csv("./output/fishery_independent/2021/noaa_race_trawl_non_target_scallop_cpue.csv")
  
  

# chalmys in fishery catch ----
### obs cpue
fish_catch_comp %>%
  filter(race_code %in% c(74103:74109, 74112), haul_sampled == 1) %>%
  group_by(season, district) %>%
  summarise(cpue_lb = sum(wt_lb) / sum(dredge_hrs)) %>%
  # join to total effort
  left_join(obs_haul_info %>%
              group_by(season, district) %>%
              summarise(total_dredge_hr = sum(dredge_hrs)),
            by = c("season", "district")) %>% ungroup %>%
  # total catch in lbs
  mutate(total_catch_t = cpue_lb * total_dredge_hr * 0.000453592) -> fishery_chlamys_catch
  
  
# bycatch composition ----

taxa <- c("Weathervane Scallop", "Rockfishes", "Gadids", "Pacific Halibut", "Flatfishes", "Misc. Roundfishes",
          "Skates", "Sharks", "Commercial Crabs", "Misc. Crabs", "Hermit Crabs", "Shrimp",
          "Misc. Bivalves", "Gastropods", "Sea Stars", "Brittle/Basket Stars", "Misc. Bentic Invert.", "Jellyfishes", 
          "Octopus/Squid", "Natural Debris", "Man-made Debris")
fill_color = c(RColorBrewer::brewer.pal(9, "Blues")[3:7],
               RColorBrewer::brewer.pal(9, "Greens")[3:4],
               RColorBrewer::brewer.pal(9, "Oranges")[3:6],
               RColorBrewer::brewer.pal(9, "Purples")[3:7],
               "peachpuff1", "cornflowerblue", "grey40", "grey60")
               

fish_catch_comp %>%
  mutate(taxon = case_when(race_code %in% 74120 ~ "Weathervane Scallop",
                           race_code %in% 30000:36000 ~ "Rockfishes",
                           race_code %in% 66000:67499 ~ "Shrimp",
                           race_code %in% c(74000:74119, 74121:75799,77011) ~ "Misc. Bivalves",
                           race_code %in% 71000:73999 ~ "Gastropods",
                           race_code %in% c(1:3,21:149, 710, 10400:21598, 21752:29999) ~ "Misc. Roundfishes",
                           race_code %in% 21705:21747 ~ "Gadids",
                           race_code %in% 10120 ~ "Pacific Halibut",
                           race_code %in% c(10001:10119, 10121:10295) ~ "Flatfishes",
                           race_code %in% 400:495 ~ "Skates",
                           race_code %in% c(80000:80159, 80160, 80161:82101) ~ "Sea Stars",
                           race_code %in% 83000:83701 ~ "Brittle/Basket Stars",
                           race_code %in% c(40011, 41100:44122, 82102:82999, 85000:99990, 50000:65210, 70100) ~ "Misc. Bentic Invert.",
                           race_code %in% c(68000:68012, 68050:68521, 68577:68781, 69250:69285, 69312:69320, 69325:69341,
                                            69520:69550, 68040, 68575) ~ "Misc. Crabs",
                           race_code %in% 69000:69200 ~ "Hermit Crabs",
                           race_code %in% c(68020, 68541:68570, 68580:68590, 69290:69310, 
                                            69321:69323, 69400:69401) ~ "Commercial Crabs",
                           race_code %in% 150:355 ~ "Sharks",
                           race_code %in% 0 ~ "Man-made Debris",
                           race_code %in% c(99990:99999) ~ "Natural Debris",
                           race_code %in% 78010:79513 ~ "Octopus/Squid",
                           race_code %in% 40500 ~ "Jellyfishes"),
         taxon = factor(taxon, levels = taxa)) -> byctach_w_taxon

# Bycacth by ecoregion?
byctach_w_taxon %>%
  mutate(ecoregion = case_when(district == "Q" ~ "Area Q (EBS)",
                               district %in% c("O", "UB", "WC", "C", "KSH", "KNE", 
                                               "KSE", "KSW", "KSEM") ~ "Areas O, M, K (Western GOA)",
                               district %in% c("WKI", "EKI", "YAK") ~ "Areas E, D (Eastern GOA)"),
         ecoregion = factor(ecoregion, levels = c("Area Q (EBS)", "Areas O, M, K (Western GOA)", "Areas E, D (Eastern GOA)"))) %>%
  filter(race_code != 74120) %>%
  group_by(season, ecoregion) %>%
  mutate(bag_wt = sum(wt_lb, na.rm = T)) %>%
  group_by(season, ecoregion, taxon) %>%
  summarise(wt_lb = sum(wt_lb, na.rm = T),
            wt_prop = wt_lb / mean(bag_wt),
            bag_wt = mean(bag_wt)) %>%
  
  ggplot()+
  geom_bar(aes(x = season, y = wt_prop, fill = taxon), position = "stack", stat = "identity") +
  scale_fill_manual(values = fill_color) +
  labs(x = NULL, y = "Proportion by Weight", fill = NULL) + 
  facet_wrap(~ecoregion, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./figures/safe/2022/fishery_bycatch_comp.png", plot = x,
       height = 6, width = 5, units = "in")

# stats for text
byctach_w_taxon %>%
  mutate(bag_wt = sum(wt_lb, na.rm = T)) %>%
  group_by(taxon) %>%
  summarise(wt_lb = sum(wt_lb, na.rm = T),
            wt_prop = wt_lb / mean(bag_wt) * 100,
            bag_wt = mean(bag_wt)) %>%
  arrange(-wt_prop)
