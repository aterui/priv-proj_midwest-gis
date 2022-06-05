
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# watershed delineation ---------------------------------------------------

## save input files to tempdir

shp <- paste(tempdir(), "temp.shp", sep = "\\")
tif <- sapply(1:2, function(i) paste0(tempdir(), "\\temp", i, ".tif"))

st_read(dsn = here::here("data_fmt/whitebox"),
        layer = "epsg4326_outlet",
        drivers = "ESRI Shapefile") %>% 
  st_write(shp, append = F)

raster(here::here("data_fmt/epsg4326_dir_d8.tif")) %>% 
  writeRaster(filename = tif[1],
              overwrite = TRUE)

wbt_watershed(d8_pntr = tif[1],
              pour_pts = shp,
              output = tif[2])

wgs84_wsd_polygon <- raster(tif[2]) %>% 
  st_as_stars() %>% 
  st_as_sf(merge = TRUE,
           as_point = FALSE) %>% 
  rmapshaper::ms_simplify(keep = 0.4) %>% 
  st_make_valid()

## export

saveRDS(wgs84_wsd_polygon,
        file = here::here("data_fmt/epsg4326_watershed.rds"))

st_write(wgs84_wsd_polygon,
         here::here("data_fmt/epsg4326_watershed.gpkg"),
         append = FALSE)

raster(tif[2]) %>% 
  writeRaster(filename = here::here("data_fmt/whitebox/epsg4326_watershed.tif"),
              overwrite = TRUE)

## remove temporary files
list.files(path = str_remove(shp, "\\\\temp.shp"),
           full.names = T) %>% 
  file.remove()


# extract values ----------------------------------------------------------

## watershed polygon ####
albers_wsd_polygon <- readRDS(here::here("data_fmt/epsg4326_watershed.rds")) %>%
  st_make_valid() %>% 
  st_transform(crs = 5070) %>% 
  dplyr::select(NULL) %>% 
  dplyr::mutate(wsd_id = seq_len(nrow(.)))

## land use ####
## NCLD data - 30 m resolution, US only
num_cores <- detectCores() - 1
num_in_group <- floor(nrow(albers_wsd_polygon) / num_cores)

albers_wsd_polygon <- albers_wsd_polygon %>% 
  mutate(group_id = wsd_id %/% num_in_group + 1) # assign group id

cl <- makeCluster(detectCores())
registerDoSNOW(cl)

tic()
p_nlcd <- foreach(i = 1:num_cores,
                  .combine = bind_rows,
                  .packages = c("exactextractr",
                                "terra",
                                "sf",
                                "dplyr")) %dopar% {
                                  
                                  albers_fua_nlcd <- terra::rast("data_fmt/albers_nlcd_binary.tif")
                                  
                                  x <- exactextractr::exact_extract(x = albers_fua_nlcd, 
                                                                    y = dplyr::filter(albers_wsd_polygon, group_id == i),
                                                                    fun = "mean",
                                                                    append_cols = T) %>% 
                                    dplyr::as_tibble() %>% 
                                    dplyr::rename(frac_forest_30l = mean.forest_30l,
                                                  frac_urban_30l = mean.urban_30l,
                                                  frac_agri_30l = mean.agri_30l)
                                  
                                  return(x)
                                  
                                }
toc()
stopCluster(cl)


## Copernicus data - 100 resolution, global
albers_fua <- terra::rast("data_fmt/albers_lu_binary.tif")

p_lu <- exact_extract(x = albers_fua, 
                      y = albers_wsd_polygon,
                      fun = "mean",
                      append_cols = T) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(frac_forest_100l = mean.forest_100l,
                frac_urban_100l = mean.urban_100l,
                frac_agri_100l = mean.agri_100l)


## climate ####
albers_clim <- terra::rast("data_fmt/albers_chelsa.tif")
names(albers_clim) <- c("temp", "ppt")

mu_clim <- exact_extract(x = albers_clim, 
                         y = albers_wsd_polygon,
                         fun = "mean",
                         append_cols = T) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(mean_temp = mean.temp * 0.1,
                mean_ppt = mean.ppt) %>% 
  dplyr::select(-mean.temp,
                -mean.ppt)

## dem ####

albers_dem <- terra::rast("data_fmt/albers_dem.tif")

mu_dem <- exact_extract(x = albers_dem, 
                        y = albers_wsd_polygon,
                        fun = "mean",
                        append_cols = T) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(mean_dem = mean)

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
# 
# # channel intersection with watershed polygons
# 
# albers_channel <- st_read(dsn = "data_gis",
#                           layer = "epsg4326_channel_1sqkm") %>% 
#   dplyr::select(NULL) %>% 
#   st_transform(crs = 5070)
# 
# albers_channel_clip <- albers_channel %>% 
#   st_intersection(albers_wsd_polygon) %>% 
#   dplyr::mutate(geom_type = st_geometry_type(.))
# 
# albers_channel_clip_cast <- albers_channel_clip %>% 
#   dplyr::filter(geom_type == "MULTILINESTRING") %>% 
#   st_cast(to = "LINESTRING") %>% 
#   dplyr::mutate(geom_type = st_geometry_type(.)) %>% 
#   dplyr::bind_rows(dplyr::filter(albers_channel_clip,
#                                  geom_type == "LINESTRING")) %>% 
#   dplyr::mutate(length = units::set_units(st_length(.), km)) %>% 
#   dplyr::filter(length >= units::set_units(45, m))
# 
# # read channel intersected with watershed polygons
# # re-run the above script when making changes to the source files
# #albers_channel_clip_cast <- st_read(dsn = "data_gis/albers_channel.gpkg")
# 
# # select watersheds with >= 3 branches
# wsd_id_3branch <- albers_channel_clip_cast %>% 
#   dplyr::group_by(wsd_id) %>% 
#   dplyr::summarize(n_branch = n()) %>% 
#   dplyr::filter(n_branch >= 3) %>% 
#   dplyr::pull(wsd_id)
# 
# p_branch <- albers_channel_clip_cast %>% 
#   dplyr::filter(wsd_id %in% wsd_id_3branch) %>% 
#   dplyr::group_by(wsd_id) %>% 
#   summarize(rate = fitdistrplus::fitdist(as.numeric(length), "exp")$estimate,
#             n_branch = n()) %>% 
#   mutate(p_branch = pexp(q = 1, rate = rate)) %>% 
#   as_tibble() %>% 
#   select(wsd_id,
#          rate,
#          n_branch,
#          p_branch)


# spatial join ------------------------------------------------------------

albers_wsd_polygon <- albers_wsd_polygon %>% 
  dplyr::select(wsd_id) %>% 
  dplyr::left_join(p_lu, by = "wsd_id") %>% 
  dplyr::left_join(p_nlcd, by = "wsd_id") %>% 
  dplyr::left_join(mu_clim, by = "wsd_id") %>% 
  dplyr::left_join(mu_dem, by = "wsd_id") %>% 
  #dplyr::left_join(p_branch, by = "wsd_id") %>% 
  dplyr::left_join(n_dam, by = "wsd_id") %>% 
  dplyr::mutate(n_dam = replace_na(n_dam, 0)) %>% 
  mutate(area = st_area(.),
         area = round(units::set_units(area, km^2), 1)) %>% 
  dplyr::select(-starts_with("group_id"))


# output ------------------------------------------------------------------

## watershed

saveRDS(albers_wsd_polygon,
        file = "data_fmt/albers_watershed.rds")

saveRDS(albers_wsd_polygon,
        file = normalizePath("../public-proj_positive_interaction_metacommunity/data_raw_gis/albers_watershed.rds"))

# ## channel  
# st_write(albers_channel_clip_cast,
#          dsn = "data_gis/albers_channel.gpkg",
#          append = FALSE)
# st_write(albers_channel_clip_cast,
#          normalizePath("../public-proj_stream-diversity/code_empirical/data_gis/albers_channel_mw.gpkg"),
#          append = FALSE)
# 
# ## watershed  
# st_write(albers_wsd_polygon,
#          "data_gis/albers_watershed_final.gpkg",
#          append = FALSE)
# st_write(albers_wsd_polygon,
#          normalizePath("../public-proj_stream-diversity/code_empirical/data_gis/albers_watershed_mw_final.gpkg"),
#          append = FALSE)
