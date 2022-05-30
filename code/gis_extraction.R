
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
pacman::p_load(raster,
               rgdal, 
               tidyverse, 
               sf, 
               stars, 
               mapview, 
               exactextractr)  

# extract values ----------------------------------------------------------

## watershed polygon ####
albers_wsd_polygon <- st_read(dsn = "data_gis",
                              layer = "epsg4326_watershed_simple") %>%
  st_make_valid() %>% 
  st_transform(crs = 5070) %>% 
  dplyr::select(NULL) %>% 
  dplyr::mutate(wsd_id = seq_len(nrow(.))) %>%
  dplyr::mutate(area = st_area(.)) %>% 
  dplyr::mutate(area = units::set_units(area, km^2))

## land use ####

albers_lu <- raster("data_gis/albers_lu.tif")

albers_forest <- calc(albers_lu, 
                      fun = function(x) ifelse(dplyr::between(x, 111, 126), 1, 0))
albers_urban <- calc(albers_lu, 
                     fun = function(x) ifelse(x == 50, 1, 0))
albers_agri <- calc(albers_lu, 
                    fun = function(x) ifelse(x == 40, 1, 0))
albers_fua <- stack(albers_forest, 
                    albers_urban, 
                    albers_agri)
names(albers_fua) <- c("forest", "urban", "agri")

p_lu <- exact_extract(albers_fua, 
                      albers_wsd_polygon,
                      "mean") %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(wsd_id = as.numeric(rownames(.))) %>% 
  dplyr::rename(frac_forest = mean.forest,
                frac_urban = mean.urban,
                frac_agri = mean.agri)

## climate ####

# download worldclim, 30-arc second (0.5 minute of degree)
wgs84_clim1 <- getData("worldclim",
                       var = "bio",
                       res = 0.5,
                       lon = -119,
                       lat = 31,
                       path = here::here("data_source/data_org_wc"))

wgs84_clim2 <- getData("worldclim",
                       var = "bio",
                       res = 0.5,
                       lon = -89,
                       lat = 31,
                       path = here::here("data_source/data_org_wc"))

wgs84_clim <- merge(wgs84_clim1, 
                    wgs84_clim2)
wgs84_dem <- raster("data_gis/epsg4326_dem.tif") # for extent

wgs84_clim <- wgs84_clim[[c(1,12)]] %>% # extract temperature (bio1) and precipitation (bio12)
  crop(extent(wgs84_dem))

# reprojection
albers_clim <- projectRaster(from = wgs84_clim,
                             crs = st_crs(5070)$proj4string,
                             method = 'bilinear',
                             res = 1000)

names(albers_clim) <- c("temp", "ppt")

mu_clim <- exact_extract(albers_clim, 
                         albers_wsd_polygon,
                         "mean") %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(wsd_id = as.numeric(rownames(.)),
                mean_temp = mean.temp * 0.1,
                mean_ppt = mean.ppt) %>% 
  dplyr::select(-mean.temp,
                -mean.ppt)

## dem ####

albers_dem <- raster("data_gis/albers_dem.tif")

mu_dem <- exact_extract(albers_dem, 
                        albers_wsd_polygon,
                        "mean") %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(wsd_id = as.numeric(rownames(.))) %>% 
  dplyr::rename(mean_dem = value)

## dam/reservoir ####
n_dam <- st_read(dsn = "data_source/data_org_dam",
                 layer = "GRanD_dams_v1_1",
                 crs = 4326) %>% 
  st_transform(crs = 5070) %>% 
  st_join(albers_wsd_polygon) %>% 
  dplyr::group_by(wsd_id) %>% 
  dplyr::summarize(n_dam = n_distinct(GRAND_ID)) %>% 
  dplyr::as_tibble() %>% 
  tidyr::drop_na(wsd_id) %>% 
  dplyr::select(-geometry)

## channel ####

# channel intersection with watershed polygons

albers_channel <- st_read(dsn = "data_gis",
                          layer = "epsg4326_channel_1sqkm") %>% 
  dplyr::select(NULL) %>% 
  st_transform(crs = 5070)

albers_channel_clip <- albers_channel %>% 
  st_intersection(albers_wsd_polygon) %>% 
  dplyr::mutate(geom_type = st_geometry_type(.))

albers_channel_clip_cast <- albers_channel_clip %>% 
  dplyr::filter(geom_type == "MULTILINESTRING") %>% 
  st_cast(to = "LINESTRING") %>% 
  dplyr::mutate(geom_type = st_geometry_type(.)) %>% 
  dplyr::bind_rows(dplyr::filter(albers_channel_clip,
                                 geom_type == "LINESTRING")) %>% 
  dplyr::mutate(length = units::set_units(st_length(.), km)) %>% 
  dplyr::filter(length >= units::set_units(45, m))

# read channel intersected with watershed polygons
# re-run the above script when making changes to the source files
#albers_channel_clip_cast <- st_read(dsn = "data_gis/albers_channel.gpkg")

# select watersheds with >= 3 branches
wsd_id_3branch <- albers_channel_clip_cast %>% 
  dplyr::group_by(wsd_id) %>% 
  dplyr::summarize(n_branch = n()) %>% 
  dplyr::filter(n_branch >= 3) %>% 
  dplyr::pull(wsd_id)

p_branch <- albers_channel_clip_cast %>% 
  dplyr::filter(wsd_id %in% wsd_id_3branch) %>% 
  dplyr::group_by(wsd_id) %>% 
  summarize(rate = fitdistrplus::fitdist(as.numeric(length), "exp")$estimate,
            n_branch = n()) %>% 
  mutate(p_branch = pexp(q = 1, rate = rate)) %>% 
  as_tibble() %>% 
  select(wsd_id,
         rate,
         n_branch,
         p_branch)


# spatial join ------------------------------------------------------------

albers_wsd_polygon <- albers_wsd_polygon %>% 
  dplyr::left_join(p_lu, by = "wsd_id") %>% 
  dplyr::left_join(mu_clim, by = "wsd_id") %>% 
  dplyr::left_join(mu_dem, by = "wsd_id") %>% 
  dplyr::left_join(p_branch, by = "wsd_id") %>% 
  dplyr::left_join(n_dam, by = "wsd_id") %>% 
  dplyr::mutate(n_dam = replace_na(n_dam, 0))


# output ------------------------------------------------------------------

## channel  
st_write(albers_channel_clip_cast,
         dsn = "data_gis/albers_channel.gpkg",
         append = FALSE)
st_write(albers_channel_clip_cast,
         normalizePath("../public-proj_stream-diversity/code_empirical/data_gis/albers_channel_mw.gpkg"),
         append = FALSE)

## watershed  
st_write(albers_wsd_polygon,
         "data_gis/albers_watershed_final.gpkg",
         append = FALSE)
st_write(albers_wsd_polygon,
         normalizePath("../public-proj_stream-diversity/code_empirical/data_gis/albers_watershed_mw_final.gpkg"),
         append = FALSE)
