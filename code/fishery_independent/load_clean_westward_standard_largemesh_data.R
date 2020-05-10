# notes ----
# cleaning westward region standard large mesh survey catch data (scallops)
# Tyler Jackson
# tyler.jackson@alaska.gov
# last updated: 2020/2/26

# load ---- 
library(tidyverse, verbose = F, quietly = T)
source("./code./misc/adfg_map_functions.R")

# data ----

## registration area polygons
f_shp_prep("./data/maps/mgmt_units", "Scallop_Statewide_Areas_PY", fortify = F) %>%
  spTransform(., CRS(proj4string(raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")))) -> reg_areas

## district polygons (currently only area K and M) for classifying district in survey tows
f_shp_prep("./data/maps/mgmt_units", "Scallop_KM_Districts_wgs84", fortify = F) %>%
  spTransform(., CRS(proj4string(raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")))) -> district_polygons

## load raw catch and specimen dump from standard survey stations 1988 - present
## data contacts: Kally Spallinger, Ric Shepard, Mike Knutsen (ADF&G Kodiak)

## catch data
catch <- do.call("rbind", 
                 lapply(list.files(path = "./data/westward_standard_lm_survey/catch_dump",
                                   pattern = ".csv", full.names = T), 
                        read_csv, col_types = cols()))
## specimen data
spec <- do.call("rbind", 
                 lapply(list.files(path = "./data/westward_standard_lm_survey/specimen_dump",
                                   pattern = ".csv", full.names = T), 
                        read_csv, col_types = cols()))

# data mgmt ----

## catch

catch %>%
  # change haul date to a date format
  mutate(Haul_Date = lubridate::mdy(Haul_Date),
         year = lubridate::year(Haul_Date)) %>%
  # select fields we are interested in 
  select(year, Survey, Tow, Haul_Date, Station, Start_Lat, Start_Lon, End_Lat, End_Lon,
         `Depth_Avg(fm)`, `Bottom_Temp(c)`, `Distance(km)`, RACE_code, Measured_cnt,
         `Measured_wt(kg)`, Unmeasured_cnt, `Unmeasured_wt(kg)`, Final_cnt, `Final_wt(kg)`) %>%
  # rename fields
  rename(Year = year, tow = Tow, haul_date = Haul_Date, start_lat = Start_Lat, 
         start_lon = Start_Lon, end_lat = End_Lat, end_lon = End_Lon, depth_fa = `Depth_Avg(fm)`,
         bottom_temp = `Bottom_Temp(c)`, distance_km = `Distance(km)`, 
         measured_count = Measured_cnt, measured_wt_kg = `Measured_wt(kg)`, 
         unmeasured_count = Unmeasured_cnt, unmeasured_wt_kg = `Unmeasured_wt(kg)`, 
         final_count = Final_cnt, final_wt_kg = `Final_wt(kg)`) %>%
  # compute mid lat and mid lon
  mutate(lon = (start_lon + end_lon) / 2,
         lat = (start_lat + end_lat) / 2,
         tow = as.character(tow)) %>%
  dplyr::select(-start_lon, -end_lon, -start_lat, -end_lat) -> tmp
  
### get a list of all tows
tmp %>%
  dplyr::select(Year, Survey, tow, haul_date, Station, depth_fa, bottom_temp, distance_km, lon, lat) %>%
  # remove all duplicates
  distinct() %>%
  # join to scallop catch tows
  left_join(tmp %>%
              filter(RACE_code == 74120)) %>%
  dplyr::select(-RACE_code) %>%
  # change NA catches to 0
  replace_na(list(measured_count = 0, measured_wt_kg = 0, unmeasured_count = 0, unmeasured_wt_kg = 0, 
                  final_count = 0, final_wt_kg = 0)) %>%
  # add scallop registration area
  filter(!is.na(lon),
         !is.na(lat)) %>%
  mutate(district = f_over(x = ., y = district_polygons, label = "ISCALDIST"),
         district = case_when(district == "Central District" ~ "C",
                              district == "Northeast District" ~ "KNE",
                              district == "Shelikof District" ~ "KSH",
                              district == "Southeast District" ~ "KSE",
                              district == "Southwest District" ~ "KSW",
                              district == "West Chignik District " ~ "WC",
                              is.na(district) ~ "O"),
         district = factor(district, levels = c("KNE", "KSH", "KSW", "KSE", "C", "WC", "O")),
         reg_area = f_over(x = ., y = reg_areas, label = "Area_Name"),
         reg_area = case_when(reg_area == "Kodiak" ~ "K",
                              reg_area == "Alaska Peninsula" ~ "M",
                              reg_area == "Dutch Harbor" ~ "O"),
         reg_area = factor(reg_area)) -> wslm_catch

print("Westward Region standard large mesh survey scallop catch data has been loaded -> wslm_catch")  

## specimen
spec %>%
  # change haul date to a date format
  mutate(haul_date = lubridate::mdy(haul_date),
         year = lubridate::year(haul_date)) %>%
  # chuck all species except for scallops
  filter(RACE_Code == 74120) %>%
  # select fields we are interested in 
  select(year, Survey, Tow, haul_date, Station, SqMi, Start_Lat, Start_Lon, End_Lat, End_Lon,
         Depth_avg, Bottom_temp, Distance, RACE_Code, nSize, sampfrac) %>%
  # rename fields
  rename(Year = year, tow = Tow, start_lat = Start_Lat, start_lon = Start_Lon, 
         end_lat = End_Lat, end_lon = End_Lon, depth_fa = Depth_avg,bottom_temp = Bottom_temp, 
         distance_km = Distance, RACE_code = RACE_Code, sh = nSize) %>%
  # compute mid lat and mid lon
  mutate(lon = (start_lon + end_lon) / 2,
         lat = (start_lat + end_lat) / 2) %>%
  dplyr::select(-start_lon, - end_lon, -start_lat, -end_lat, -RACE_code) %>%
  # add scallop registration area
  filter(!is.na(lon),
         !is.na(lat)) %>%
  mutate(district = f_over(x = ., y = district_polygons, label = "ISCALDIST"),
         district = case_when(district == "Central District" ~ "C",
                              district == "Northeast District" ~ "KNE",
                              district == "Shelikof District" ~ "KSH",
                              district == "Southeast District" ~ "KSE",
                              district == "Southwest District" ~ "KSW",
                              district == "West Chignik District " ~ "WC",
                              is.na(district) ~ "O"),
         district = factor(district, levels = c("KNE", "KSH", "KSW", "KSE", "C", "WC", "O")),
         reg_area = f_over(x = ., y = reg_areas, label = "Area_Name"),
         reg_area = case_when(reg_area == "Kodiak" ~ "K",
                              reg_area == "Alaska Peninsula" ~ "M",
                              reg_area == "Dutch Harbor" ~ "O"),
         reg_area = factor(reg_area)) -> wslm_specimen

print("Westward Region standard large mesh survey scallop specimen data has been loaded -> wslm_specimen")  

## clear catch and spec from the environment 
rm(catch)
rm(spec)
