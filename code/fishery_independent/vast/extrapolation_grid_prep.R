# notes ----
## Create a VAST extrapolation grid for KSH survey area
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)
library(here)
library(rgdal)
library(spdplyr)
library(sf)
library(sp)

# data ----

## load shapefile
readOGR(dsn = "./data/maps/mgmt_units", layer = "Scallop_KM_Districts_wgs84") %>%
  fortify() %>%
  
  ## group corresponding to distrct of interest
  filter(group == 3.1) %>%
  
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(District = "KSH")) -> shp

proj4string(shp) <- "+proj=longlat +ellps=WGS84 +no_def"

## create extrapolation grid ----

## convert preojection to utm
lon <- sum(bbox(shp)[1,])/2 
utmzone <- floor((lon + 180) / 6) + 1 
shp %>%
  spTransform(., CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))) -> tmp

## set grid size in meters
cell_size <- 4000

## make the grid using sf package
st_bbox(tmp) #get bounding box info
grid <- st_make_grid(tmp, cellsize = cell_size, what = "centers") # make grid

as(grid, "Spatial")  %>% 
  as(., "SpatialPointsDataFrame") -> grid_tmp

as(grid, "Spatial")  %>% 
  over(., tmp) -> grid_tmp@data 

grid_tmp %>%
  filter(!is.na(District)) %>%
  spTransform(., "+proj=longlat +ellps=WGS84 +no_defs") %>%
  as_tibble() %>%
  mutate(Lon = coords.x1,
         Lat = coords.x2,
         District,
         Area_km2 = (cell_size / 2000 * cell_size / 2000),
         row = 1:nrow(.)) %>%
  dplyr::select(-coords.x1, -coords.x2) %>%
  dplyr::select(Lon, Lat, District, Area_km2, row) %>%
  as.data.frame() %>%
  saveRDS(., file = here("data/vast", "ksh_4000m_grid.rds"))



### CUT VAST GOA GRID ----
readOGR(dsn = "./data/maps/mgmt_units", layer = "Scallop_KM_Districts_wgs84") %>%
  fortify() %>%
  
  ## group corresponding to distrct of interest
  filter(group == 3.1) %>%
  
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(District = "KSH")) -> shp

proj4string(shp) <- "+proj=longlat +ellps=WGS84 +no_def"

## create extrapolation grid ----



sp::coordinates(gulf_of_alaska_grid) %>%
  SpatialPointsDataFrame(., data = as.data.frame(.[,3:4])) -> grid
proj4string(grid) <- "+proj=longlat +ellps=WGS84 +no_def"

gulf_of_alaska_grid %>%
  bind_cols(over(grid, shp)) %>%
  filter(District == "KSH") -> goa_ksh

