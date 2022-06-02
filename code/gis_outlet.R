
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

# read data ---------------------------------------------------------------

## waterbody data
albers_g1wbm <- readRDS("data_fmt/epsg4326_g1waterbody_large_buff100.rds") %>% 
  st_transform(crs = 5070)

## large river data
albers_channel5k <- readRDS("data_fmt/epsg4326_channel_5ksqkm_buff100.rds") %>% 
  st_transform(crs = 5070)


# outlet line -------------------------------------------------------------

## remove overlapping parts 
albers_mask_polygon <- st_union(x = albers_channel5k,
                                y = albers_g1wbm) %>% 
  smoothr::fill_holes(threshold = units::set_units(1000, km^2))

st_write(albers_mask_polygon,
         dsn = "data_fmt/albers_mask_polygon.gpkg",
         append = FALSE)

## convert polygons to lines
albers_mask_outline <- st_cast(albers_mask_polygon,
                               to = "MULTILINESTRING") %>% 
  st_union()

st_write(albers_mask_outline,
         dsn = "data_fmt/albers_mask_outline.gpkg",
         append = FALSE)

saveRDS(albers_mask_outline,
        file = "data_fmt/albers_mask_outline.rds")


# outlet ------------------------------------------------------------------

## read layers
albers_channel <- readRDS("data_fmt/epsg4326_channel_1sqkm.rds") %>%
  dplyr::select(NULL) %>% 
  dplyr::mutate(line_id = seq_len(nrow(.))) %>% 
  st_transform(crs = 5070)

albers_mask_outline <- readRDS("data_fmt/albers_mask_outline.rds") %>% 
  rmapshaper::ms_simplify()

## parallel processing

num_cores <- detectCores() - 1
num_in_group <- floor(nrow(albers_channel) / num_cores)

albers_channel <- albers_channel %>% 
  mutate(group_id = line_id %/% num_in_group + 1) # assign group id

### foreach - intersection parallelized

cl <- makeCluster(detectCores())
registerDoSNOW(cl)

tic()
albers_outlet <- foreach(i = 1:num_cores,
                         .combine = bind_rows,
                         .packages = c("sf", "dplyr")) %dopar% {
                           
                           st_intersection(filter(albers_channel, group_id == i),
                                           albers_mask_outline) %>% 
                             mutate(gtype = st_geometry_type(.))
                           
                         }
toc()
stopCluster(cl)

### outlet
albers_outlet_cast <- filter(albers_outlet,
                             gtype == "MULTIPOINT") %>%
  st_cast(to = "POINT") %>%
  dplyr::bind_rows(filter(albers_outlet,
                          gtype == "POINT"))

wgs84_outlet_cast <- albers_outlet_cast %>%
  dplyr::mutate(point_id = seq_len(nrow(.))) %>%
  st_transform(crs = 4326)


saveRDS(wgs84_outlet_cast,
        file = "data_fmt/epsg4326_outlet.rds")

st_write(wgs84_outlet_cast,
         dsn = "data_fmt/whitebox",
         layer = "epsg4326_outlet",
         driver = "ESRI Shapefile",
         append = FALSE)
