# notes ----

## Explore Trends in Scallop Abundance
## westward region large mesh standard survey
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/26

# load ----

library(tidyverse)

# data ----

## load survey catch and specimen data
source("./code/abundance_index/load_clean_westward_standard_largemesh_data.R")

## low resolution map of region
ak <- maps::map("world", region = c('russia', 'usa:alaska', 'canada'), fill = T, plot = F) %>%
  broom::tidy()


# eda ----

# map of scallop catches by year
ggplot()+
  geom_point(data = wslm_catch, aes(x = start_lon, y = start_lat, color = log(final_count)))+
  geom_polygon(data = ak, aes(x = long, y = lat, group = group), 
               color = "grey20", fill = "grey")+
  coord_map(projection = "albers", lat0 = 55, lat = 62, 
            xlim = c(-180, -150), ylim = c(53, 59))+
  #facet_wrap(~Year)+
  theme_bw()
