
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
pacman::p_load(raster,
               rgdal,
               tidyverse,
               sf,
               stars,
               mapview)  

# read mask layer ---------------------------------------------------------

albers_mask <- st_read("data_gis/albers_huc2_zone4_7.gpkg") %>% 
  dplyr::select(NULL)

albers_mask_buff <- st_buffer(albers_mask,
                              dist = 15000)

wgs84_mask_buff <- st_transform(albers_mask_buff,
                                crs = 4326)


# merge raster files ------------------------------------------------------

## upstream watershed area (MERIT dem)
upa <- list.files("data_org_upa",
                  full.names = T) %>% 
  lapply(FUN = raster)

upa_merge <- do.call(what = merge,
                     args = upa) %>% 
  crop(y = extent(wgs84_mask_buff))

writeRaster(upa_merge,
            filename = "data_gis/epsg4326_upa",
            format = "GTiff",
            overwrite = TRUE)  

## flow direction
fdir <- list.files("data_org_dir",
                   full.names = T) %>% 
  lapply(FUN = raster)

fdir_merge <- do.call(what = merge,
                     args = fdir) %>% 
  crop(y = extent(wgs84_mask_buff)) %>% 
  mask(mask = wgs84_mask_buff)

writeRaster(fdir_merge,
            filename = "data_gis/epsg4326_dir",
            format = "GTiff",
            overwrite = TRUE)  

# stream network ----------------------------------------------------------

# grid stream networks passed to ArcGIS for "Stream to Feature"
# SAGA GIS returns inaccurate stream networks

# binary stream network
# 1 sq km
stream_grid_1sqkm <- calc(upa_merge,
                          fun = function(x) ifelse(x >= 1, 1, NA)) %>% 
  mask(mask = wgs84_mask_buff)

# export
writeRaster(stream_grid_1sqkm,
            filename = "data_gis/epsg4326_stream_grid_1sqkm",
            format = "GTiff",
            overwrite = TRUE)  

# 5000 sq km
stream_grid_5ksqkm <- calc(upa_merge,
                           fun = function(x) ifelse(x >= 5000, 1, NA)) %>% 
  mask(mask = wgs84_mask_buff)

# export
writeRaster(stream_grid_5ksqkm,
            filename = "data_gis/epsg4326_stream_grid_5ksqkm",
            format = "GTiff",
            overwrite = TRUE)  


# buffer ------------------------------------------------------------------

# read 1 sqkm channel - arc output
wgs84_channel_1sqkm <- st_read(dsn = "data_gis",
                               layer = "epsg4326_channel_1sqkm")

# read 5k sqkm channel - arc output
wgs84_channel_5ksqkm <- st_read(dsn = "data_gis",
                                layer = "epsg4326_channel_5ksqkm")

wgs84_channel_5ksqkm_buff100 <- st_transform(wgs84_channel_5ksqkm,
                                             crs = 5070) %>% 
  st_buffer(dist = 100) %>% 
  st_union() %>% 
  st_transform(crs = 4326)


# export
st_write(wgs84_channel_1sqkm, 
         dsn = "data_gis/epsg4326_channel_1sqkm.gpkg",
         append = FALSE)

st_write(wgs84_channel_5ksqkm, 
         dsn = "data_gis/epsg4326_channel_5ksqkm.gpkg",
         append = FALSE)

st_write(wgs84_channel_5ksqkm_buff100, 
         dsn = "data_gis/epsg4326_channel_5ksqkm_buff100.gpkg",
         append = FALSE)

