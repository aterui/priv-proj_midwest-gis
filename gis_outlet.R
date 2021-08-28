
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
pacman::p_load(raster,
               rgdal,
               tidyverse,
               sf,
               stars,
               mapview)  


# read data ---------------------------------------------------------------

## waterbody data
albers_g1wbm <- st_read("data_gis/epsg4326_g1waterbody_large_buff100.gpkg") %>% 
  st_transform(crs = 5070)

## large river data
albers_channel5k <- st_read("data_gis/epsg4326_channel_5ksqkm_buff100.gpkg") %>% 
  st_transform(crs = 5070)


# outlet line -------------------------------------------------------------

## remove overlapping parts 
albers_mask_layer <- st_union(x = albers_channel5k,
                              y = albers_g1wbm) %>% 
  smoothr::fill_holes(threshold = units::set_units(1000, km^2))

albers_mask_outline <- st_cast(albers_mask_layer,
                               to = "MULTILINESTRING") %>% 
  st_union()

st_write(albers_mask_outline,
         dsn = "data_gis/albers_mask_outline.gpkg",
         append = FALSE)


# outlet ------------------------------------------------------------------

## read layers
albers_channel <- st_read(dsn = "data_gis",
                          layer = "epsg4326_channel_1sqkm") %>%
  dplyr::select(NULL) %>% 
  dplyr::mutate(lineID = seq_len(nrow(.))) %>% 
  st_transform(crs = wkt_jgd_albers)

albers_mask_outline <- st_read(dsn = "data_gis/albers_mask_outline.gpkg") %>%
  dplyr::select(NULL)

## intersect
x <- st_intersects(albers_channel,
                   albers_mask_outline,
                   sparse = FALSE)

albers_outlet <- st_intersection(albers_channel[x,], albers_mask_outline)  
albers_outlet <- albers_outlet %>%
  mutate(gtype = st_geometry_type(.))

## outlet  
albers_outlet_cast <- filter(albers_outlet,
                             gtype == "MULTIPOINT") %>%
  st_cast("POINT") %>% 
  bind_rows(filter(albers_outlet,
                   gtype == "POINT"))

wgs84_outlet_cast <- albers_outlet_cast %>% 
  mutate(pointID = 1:nrow(.)) %>% 
  st_transform(crs = 4326)

st_write(wgs84_outlet_cast,
         dsn = "data_gis",
         layer = "epsg4326_outlet",
         driver = "ESRI Shapefile",
         append = FALSE)
