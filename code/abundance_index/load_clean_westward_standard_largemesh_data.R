# notes ----
# cleaning westward region standard large mesh survey catch data (scallops)
# Tyler Jackson
# tyler.jackson@alaska.gov
# last updated: 2020/2/26

# load ---- 
library(tidyverse, verbose = F, quietly = T)

# data ----

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
  mutate(Haul_Date = mdy(Haul_Date),
         year = year(Haul_Date)) %>%
  # chuck all species except for scallops
  filter(RACE_code == 74120) %>%
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
         final_count = Final_cnt, final_wt_kg = `Final_wt(kg)`) -> wslm_catch

print("Westward Region standard large mesh survey scallop catch data has been loaded -> wslm_catch")  

## specimen
spec %>%
  # change haul date to a date format
  mutate(haul_date = mdy(haul_date),
         year = year(haul_date)) %>%
  # chuck all species except for scallops
  filter(RACE_Code == 74120) %>%
  # select fields we are interested in 
  select(year, Survey, Tow, haul_date, Station, SqMi, Start_Lat, Start_Lon, End_Lat, End_Lon,
         Depth_avg, Bottom_temp, Distance, RACE_Code, nSize, sampfrac) %>%
  # rename fields
  rename(Year = year, tow = Tow, start_lat = Start_Lat, start_lon = Start_Lon, 
         end_lat = End_Lat, end_lon = End_Lon, depth_fa = Depth_avg,bottom_temp = Bottom_temp, 
         distance_km = Distance, RACE_code = RACE_Code, sh = nSize) -> wslm_specimen

print("Westward Region standard large mesh survey scallop specimen data has been loaded -> wslm_specimen")  


## clear catch and spec from the environment 
rm(catch)
rm(spec)
