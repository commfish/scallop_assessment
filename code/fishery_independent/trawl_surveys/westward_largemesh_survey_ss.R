# notes ----
## generate timeseries of data for stock synthesis input
## author: Tyler Jackson
## last updated 2020/10/25
## data source: Kally Spalinger, Ric Shepard

# load / data ----

## load and clean survey data
source("./code/fishery_independent/trawl_surveys/load_clean_westward_standard_largemesh_data.R")


### station areas
station_area <- read_csv("./data/westward_standard_lm_survey/wslm_station_area.csv")


# abundance index ----

## does not produce an abundance for C and O districts

wslm_catch %>%
  left_join(station_area, by = c("Station" = "station", "Year" = "year")) %>%
  mutate(area_swept = distance_km * 0.0122) %>%
  group_by(Year, district) %>%
  summarise(cpue = mean((final_wt_kg / 1000) / area_swept),
            var_cpue = var((final_wt_kg / 1000) / area_swept) / n(),
            tot_area = sum(area_km),
            .groups = "drop") %>%
  mutate(biomass = cpue * tot_area,
         se = sqrt(var_cpue * tot_area^2),
         se_log = log(se)) %>%
  dplyr::select(Year, district, biomass, se_log) -> wslm_cpue



# size composition ----

wslm_specimen %>%
  group_by(Year, district, sh) %>%
  summarise(count = round(sum(sampfrac, na.rm = T)),
            .groups = "drop") %>%
  # join to measured sample size
  left_join(wslm_catch %>%
              group_by(Year, district) %>%
              summarise(Nsamp = sum(measured_count),
                        .groups = "drop"),
            by = c("Year", "district")) %>%
  rename(yr = Year) -> wslm_sh


