
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# read mask layer ---------------------------------------------------------

albers_mask <- st_read("data_source/albers_huc2_zone4_7.gpkg") 

albers_mask_buff <- st_buffer(albers_mask,
                              dist = 15000)

wgs84_mask_buff <- st_transform(albers_mask_buff,
                                crs = 4326)


# merge raster files ------------------------------------------------------

## wgs84_g1wbm (waterbody raster layer)
wgs84_g1wbm <- list.files("data_source/data_org_g1wb",
                          full.names = T) %>% 
  lapply(FUN = raster)

wgs84_g1wbm_merge <- do.call(what = merge,
                             args = wgs84_g1wbm) %>% 
  crop(y = extent(wgs84_mask_buff)) %>% 
  mask(mask = wgs84_mask_buff)


# raster to polygon -------------------------------------------------------

wgs84_g1wbm_merge <- calc(wgs84_g1wbm_merge,
                          fun = function(x) ifelse(x %in% c(50, 51), 1, NA))

writeRaster(wgs84_g1wbm_merge,
            filename = "data_fmt/epsg4326_g1wbm_binary",
            format = "GTiff",
            overwrite = TRUE)

wgs84_g1wbm_binary <- raster("data_fmt/epsg4326_g1wbm.tif")

## this code is too slow for large data
wgs84_g1wbm_polygon <- st_as_stars(wgs84_g1wbm_binary) %>%
  st_as_sf(merge = TRUE,
           as_points = FALSE) %>%
  st_cast(to = "MULTIPOLYGON")

saveRDS(wgs84_g1wbm_polygon,
        file = "data_fmt/epsg4326_g1wbm_polygon.rds")


# select large lakes ------------------------------------------------------

albers_g1wbm_polygon <- readRDS("data_fmt/epsg4326_g1wbm_polygon.rds") %>% 
  st_transform(crs = 5070) %>% 
  mutate(area = st_area(.))

wgs84_g1wbm_polygon_large_buff100 <- albers_g1wbm_polygon %>% 
  dplyr::filter(area >= units::set_units(10, km^2)) %>% 
  dplyr::select(NULL) %>% 
  st_buffer(dist = 100) %>% 
  st_union() %>% 
  st_transform(crs = 4326)

saveRDS(wgs84_g1wbm_polygon_large_buff100,
        "data_fmt/epsg4326_g1waterbody_large_buff100.rds")
