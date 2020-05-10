# notes ----
# create 'crude' polygons for scallop districts / areas
# author: Tyler Jackson
# tyler.jackson@alaska.gov
# last updated: 2020/31/3

# load ----
source("./code/misc/adfg_map_functions.R")

# data ----
## load usa map data for crs
usa <- raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")

## load area K and M lines
f_shp_prep("./data/maps/mgmt_units", "Scallop_KM_Districts_20140627_LN") %>%
  f_transform_crs(., to = proj4string(usa)) %>%
  .[[1]] -> km_districts

#ggplot()+
  #geom_polygon(data = usa, aes(x = long, y = lat, group = group))+
  #geom_line(data = km_districts, aes(x = long, y = lat, group = group, color = id))
  #geom_hline(yintercept = 57.65)+
  #coord_quickmap(ylim = c(54, 57), xlim = c(-175, -156))

# create district polygons ----
## KNE
km_districts %>%
  # grab groups in the KNE district
  filter(id %in% c(18, 17, 10, 7, 6, 4)) %>%
  # filter pieces within district
  filter(!(id == 10 & long <= -151.4864)) %>%
  mutate(long = ifelse(id == 17 & order == 2, -152.3333, long)) %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  concaveman::concaveman() %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
    fortify() %>%
  mutate(district = "KNE") -> kne_poly

## KSE
km_districts %>%
  # grab groups in the KNE district
  filter(id %in% c(10, 7, 0, 8)) %>%
  # cut lines
  mutate(lat = ifelse(id == 0 & lat > min(lat[.$id == 8]), 
                      min(lat[.$id == 8]),
                      lat)) %>%
  filter(!(id == 10 & lat > min(lat[.$id == 7]) & lat >= min(lat[.$id == 0]))) %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  concaveman::concaveman(concavity = 150) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "KSE") -> kse_poly

## KSW (plus a line cutting across at the Karluk bed)
km_districts %>%
  # grab groups in the KSW district
  filter(id %in% c(0, 8)) %>%
  # cut lines
  mutate(lat = ifelse(id == 0 & order == 2, 57.65, lat)) %>%
  add_row(long = c(-156.37, -154.1483), lat = 57.65, order = 1:2, 
          id = 99, piece = 1, group = "99") %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  concaveman::concaveman() %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "KSW") -> ksw_poly

## KSH (cutoff fr the Karluk beds)
km_districts %>%
  # grab groups in the KSH district
  filter(id %in% c(17, 4, 6)) %>%
  # cut lines, add Karluk lines
  mutate(long = ifelse(id == 17 & order == 1, -152.3333, long)) %>%
  add_row(long = c(-156.37, -154.1483), lat = 57.65, order = 1:2, 
          id = 99, piece = 1, group = "99") %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  # adjust concavity to get the desired shape
  concaveman::concaveman(1) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "KSH") -> ksh_poly

## KSEM
km_districts %>%
  # grab groups in the KSEM district
  filter(id %in% c(0, 1, 21)) %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  # adjust concavity to get the desired shape
  concaveman::concaveman(100) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "KSEM") -> ksem_poly

## West Chignik District (WC)
km_districts %>%
  # grab groups in the WC district
  filter(id %in% c(1, 3)) %>%
  # cut district line
  mutate(lat = ifelse(id == 1 & order == 2, 54.101, lat)) %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  # adjust concavity to get the desired shape
  concaveman::concaveman(100) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "WC") -> wc_poly

## Central district (C)
km_districts %>%
  # grab groups in the C district
  filter(id %in% c(15, 20, 1, 3)) %>%
  # cut district line
  mutate(lat = ifelse(id == 1 & order == 1, 54.101, lat)) %>%
  filter(!(id == 20 & !(order %in% range(order[id == 20])))) %>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  # adjust concavity to get the desired shape
  concaveman::concaveman() %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "C") -> c_poly

## Unimak Bight (UB)
km_districts %>%
  # grab groups in the UB district
  filter(id %in% c(15, 2, 19))%>%
  dplyr::select(long, lat) %>%
  coordinates(.) %>%
  # increase concavity to avoid making an odd shape
  # adjust concavity to get the desired shape
  concaveman::concaveman(175) %>%
  Polygon(.) %>%
  list(.) %>%
  Polygons(., 1) %>%
  list(.) %>%
  SpatialPolygons(.) %>%
  SpatialPolygonsDataFrame(., data = data.frame(value = 999)) %>%
  fortify() %>%
  mutate(district = "UB") -> ub_poly

# ggplot()+
#   geom_polygon(data = area_m_poly, aes(x = long, y = lat, group = id))
