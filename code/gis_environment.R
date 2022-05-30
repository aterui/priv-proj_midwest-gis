
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
pacman::p_load(raster,
               rgdal, 
               tidyverse, 
               sf, 
               stars, 
               mapview, 
               exactextractr)  


# read mask layer ---------------------------------------------------------

albers_mask <- st_read("data_gis/albers_huc2_zone4_7.gpkg") 

albers_mask_buff <- st_buffer(albers_mask,
                              dist = 15000)

wgs84_mask_buff <- st_transform(albers_mask_buff,
                                crs = 4326)


# land use ----------------------------------------------------------------

# global landuse raster 100 m resl, year 2015

wgs84_lu <- list.files("data_source/data_org_lu",
                       full.names = T) %>% 
  lapply(FUN = raster)

wgs84_lu_merge <- do.call(what = merge,
                          args = wgs84_lu) %>% 
  crop(y = extent(wgs84_mask_buff))

writeRaster(wgs84_lu_merge, 
            filename = "data_gis/epsg4326_lu",
            format = "GTiff",
            overwrite = TRUE)

# reprojection
wgs84_lu <- raster("data_gis/epsg4326_lu.tif")

albers_lu <- projectRaster(from = wgs84_lu,
                           crs = st_crs(5070)$proj4string,
                           method = 'ngb',
                           res = 100)

writeRaster(albers_lu, 
            filename = "data_gis/albers_lu", 
            format = "GTiff",
            overwrite = TRUE)


# elevation ---------------------------------------------------------------

# Adjusted DEM from MERIT Hydro
wgs84_dem <- list.files("data_source/data_org_dem",
                        full.names = T) %>% 
  lapply(FUN = raster)

wgs84_dem_merge <- do.call(what = merge,
                           args = wgs84_dem) %>% 
  crop(y = extent(wgs84_mask_buff))

writeRaster(wgs84_dem_merge,
            filename = "data_gis/epsg4326_dem",
            format = "GTiff",
            overwrite = TRUE)


# reprojection
wgs84_dem <- raster("data_gis/epsg4326_dem.tif")

albers_dem <- projectRaster(from = wgs84_dem,
                            crs = st_crs(5070)$proj4string,
                            method = 'bilinear',
                            res = 90)

writeRaster(albers_dem, 
            filename = "data_gis/albers_dem",
            format = "GTiff",
            overwrite = TRUE)


