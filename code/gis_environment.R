
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# read mask layer ---------------------------------------------------------

albers_mask <- st_read("data_gis/albers_huc2_zone4_7.gpkg") 

albers_mask_buff <- st_buffer(albers_mask,
                              dist = 15000)

wgs84_mask_buff <- st_transform(albers_mask_buff,
                                crs = 4326)


# land use ----------------------------------------------------------------

## Copernicus
## global landuse raster 100 m resl, year 2015
wgs84_lu <- list.files("data_source/data_org_lu",
                       pattern = "LC100_global",
                       full.names = T) %>% 
  lapply(FUN = terra::rast)

wgs84_lu_merge <- do.call(what = terra::merge,
                          args = wgs84_lu) %>% 
  terra::crop(y = terra::ext(wgs84_mask_buff))

writeRaster(wgs84_lu_merge, 
            filename = "data_fmt/epsg4326_lu.tif",
            overwrite = TRUE)

## reprojection
wgs84_lu <- terra::rast("data_fmt/epsg4326_lu.tif")

albers_lu <- terra::project(x = wgs84_lu,
                            y = "epsg:5070",
                            method = "near")

terra::writeRaster(albers_lu, 
                   filename = "data_fmt/albers_lu.tif", 
                   overwrite = TRUE)

## NCLD
## US only raster 30 m resl, year 2016
wgs84_nlcd <- terra::rast(here::here("data_source/data_org_lu/epsg4326_nlcd_clip.tif"))

albers_nlcd <- terra::project(x = wgs84_nlcd,
                              y = "epsg:5070",
                              method = "near")

terra::writeRaster(albers_nlcd, 
                   filename = "data_fmt/albers_nlcd.tif", 
                   overwrite = TRUE)


# elevation ---------------------------------------------------------------

# Adjusted DEM from MERIT Hydro
wgs84_dem <- list.files("data_source/data_org_dem",
                        full.names = T) %>% 
  lapply(FUN = terra::rast)

wgs84_dem_merge <- do.call(what = terra::merge,
                           args = wgs84_dem) %>% 
  terra::crop(y = terra::ext(wgs84_mask_buff))

terra::writeRaster(wgs84_dem_merge,
                   filename = "data_fmt/epsg4326_dem.tif",
                   overwrite = TRUE)


# reprojection
wgs84_dem <- terra::rast("data_fmt/epsg4326_dem.tif")

albers_dem <- terra::project(x = wgs84_dem,
                             y = "epsg:5070",
                             method = 'bilinear')

terra::writeRaster(albers_dem, 
                   filename = "data_fmt/albers_dem.tif",
                   overwrite = TRUE)


# chelsa ------------------------------------------------------------------

wgs84_chelsa <- list.files("data_source/data_org_chelsa",
                           full.names = T) %>% 
  lapply(FUN = terra::rast)

wgs84_chelsa <- c(wgs84_chelsa[[1]], wgs84_chelsa[[2]]) %>% 
  terra::crop(y = terra::ext(wgs84_mask_buff))

# reprojection
albers_chelsa <- terra::project(x = wgs84_chelsa,
                                y = "epsg:5070",
                                method = "bilinear")

terra::writeRaster(albers_chelsa, 
                   filename = "data_fmt/albers_chelsa.tif",
                   overwrite = TRUE)
