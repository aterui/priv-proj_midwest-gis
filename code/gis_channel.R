
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
source("code/library.R")
source("code/function_arc2d8.R")


# read mask layer ---------------------------------------------------------

albers_mask <- st_read("data_gis/albers_huc2_zone4_7.gpkg") %>% 
  dplyr::select(NULL)

albers_mask_buff <- st_buffer(albers_mask,
                              dist = 15000)

wgs84_mask_buff <- st_transform(albers_mask_buff,
                                crs = 4326)


# merge raster files ------------------------------------------------------

## upstream watershed area (MERIT dem)
upa <- list.files("data_source/data_org_upa",
                  full.names = T) %>% 
  lapply(FUN = raster)

upa_merge <- do.call(what = merge,
                     args = upa) %>% 
  crop(y = extent(wgs84_mask_buff))

writeRaster(upa_merge,
            filename = "data_fmt/epsg4326_upa",
            format = "GTiff",
            overwrite = TRUE)  


## flow direction
fdir <- list.files("data_source/data_org_dir",
                   full.names = T) %>% 
  lapply(FUN = raster)

fdir_merge <- do.call(what = merge,
                      args = fdir) %>% 
  crop(y = extent(wgs84_mask_buff)) %>% 
  mask(mask = wgs84_mask_buff)

fdir_d8_merge <- arc2d8(fdir_merge)

writeRaster(fdir_d8_merge,
            filename = "data_fmt/epsg4326_dir_d8",
            format = "GTiff",
            overwrite = TRUE)  


# stream network ----------------------------------------------------------

## source raster - upstream catchment area
upa_merge <- raster("data_fmt/epsg4326_upa.tif")

## temporary files
upa_name <- paste0(tempdir(), "\\upa.tif")
str_name <- paste0(tempdir(), "\\str.tif")
v_str_name <- paste0(tempdir(), "\\str.shp")

writeRaster(upa_merge,
            filename = upa_name,
            format = "GTiff",
            overwrite = TRUE)  

## binary stream network, 1 sq km
### extract grid streams
whitebox::wbt_extract_streams(flow_accum = upa_name,
                              output = str_name,
                              threshold = 1)

### vectorize streams
whitebox::wbt_raster_streams_to_vector(streams = str_name,
                                       d8_pntr = "data_fmt/epsg4326_dir_d8.tif",
                                       output = v_str_name)

### export - raster
r_str_1sqkm <- raster(str_name)

writeRaster(str_1sqkm,
            filename = "data_fmt/epsg4326_stream_grid_1sqkm",
            format = "GTiff",
            overwrite = TRUE)  

### export - vector
v_str_1sqkm <- st_read(dsn = str_remove(v_str_name, "\\\\str.shp"),
                       layer = "str",
                       drivers = "ESRI Shapefile") %>% 
  st_set_crs(4326)

saveRDS(v_str_1sqkm, file = "data_fmt/epsg4326_channel_1sqkm.rds")

## binary stream network, 5000 sq km
### extract grid streams
whitebox::wbt_extract_streams(flow_accum = upa_name,
                              output = str_name,
                              threshold = 5000)

### vectorize streams
whitebox::wbt_raster_streams_to_vector(streams = str_name,
                                       d8_pntr = "data_fmt/epsg4326_dir_d8.tif",
                                       output = v_str_name)

### export - raster
str_5ksqkm <- raster(str_name)
writeRaster(str_5ksqkm,
            filename = "data_fmt/epsg4326_stream_grid_5ksqkm",
            format = "GTiff",
            overwrite = TRUE)  

### export - vector
v_str_5ksqkm <- st_read(dsn = str_remove(v_str_name, "\\\\str.shp"),
                        layer = "str",
                        drivers = "ESRI Shapefile") %>% 
  st_set_crs(4326)

saveRDS(v_str_5ksqkm, file = "data_fmt/epsg4326_channel_5ksqkm.rds")


# buffer ------------------------------------------------------------------

wgs84_channel_5ksqkm <- readRDS(file = "data_fmt/epsg4326_channel_5ksqkm.rds")

wgs84_channel_5ksqkm_buff100 <- st_transform(wgs84_channel_5ksqkm,
                                             crs = 5070) %>% 
  st_buffer(dist = 100) %>% 
  st_union() %>% 
  st_transform(crs = 4326)

## export
saveRDS(wgs84_channel_5ksqkm_buff100,
        "data_fmt/epsg4326_channel_5ksqkm_buff100.rds")

