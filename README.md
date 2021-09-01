README
================
Akira Terui
2021-09-01

## Description

GIS project for watershed delineation in Midwest, USA. Note that GIS
files could not be stored in online repository due to large file size.

## R File

-   `gis_channel.R` Channel network delineation based on MERIT Hydro
    (<http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/index.html>).
    Channel delineation was performed in ArcMap 10.7 using
    `Stream to Feature`
-   `gis_environment` Merge environmental raster layers (Adjusted DEM
    and Copernics land use)
-   `gis_extraction` Extract environmental values based on delinated
    watershed polygons
-   `gis_g1waterbody.R` Delination of lentic water bodies based on G1WBM
    (<http://hydro.iis.u-tokyo.ac.jp/~yamadai/G3WBM/index.html>)
-   `gis_outlet.R` Delineation of outlet point feature
-   `data_gis/` Subdirectory for GIS layers. Prefix indicates CRS
    -   `channel` Channel network (1 sq-km threshold) intersected with
        watershed polygons
    -   `dem` Adjusted DEM merged
    -   `huc2_zone4_7` HUC2 Zone 4 & 7
    -   `lu` Copernics land use merged
    -   `mask_outline` Mask layer outline for outlet identification
    -   `watershed_final` Watershed polygons with environmental data
    -   `channel_1sqkm` Channel network with threshold value of 1 sq km
    -   `channel_5ksqkm_buff100` Channel network with threshold value of
        5000 sq km plus 100 m buffer
    -   `channel_5ksqkm` Channel network with threshold value of 5000 sq
        km
    -   `dir` Flow direction (source: MERIT Hydro)
    -   `g1waterbody_large_buff100` Lentic waterbody exceeding 10 sq km
        in area
    -   `outlet` Outlet points
    -   `stream_grid_1sqkm` Stream grid with threshold value of 1 sq km.
        Passed to ArcMap 10.7 for channel network delineation.
    -   `stream_grid_5ksqkm` Stream grid with threshold value of 5000 sq
        km. Passed to ArcMap 10.7 for channel network delineation.
    -   `upa` Upstream catchment area (source: MERIT Hydro)
    -   `watershed` Delineated watershed polygons. ArcGIS output
    -   `watershed_simple` Delineated watershed polygons (simplified).
        ArcGIS output

## Source file

`data_org_*` contains source files:

-   Dam (`dam`) [GRanD
    v1](https://sedac.ciesin.columbia.edu/data/set/grand-v1-dams-rev01)
-   DEM (`dem`) [MERIT Hydro Adjusted
    DEM](http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/)
-   Flow direction (`dir`) [MERIT
    Hydro](http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/)
-   Waterbody (`g1wb`)
    [G1WBM](http://hydro.iis.u-tokyo.ac.jp/~yamadai/G3WBM/index.html)
-   Land use (`lu`)
    [Copernics](https://land.copernicus.eu/global/products/lc)
-   Upstream watershed area (`upa`) [MERIT
    Hydro](http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/)
-   Climate (`wc`) [WorldClim
    v1.4](https://www.worldclim.org/data/v1.4/worldclim14.html)

## ArcGIS process

Stream and watershed delineation was preformed in ArcMap 10.7.
Procedures are saved as model builders (in `data_gis/Toolbox.tbx`)

## Session information

    ## R version 4.1.0 (2021-05-18)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18363)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] exactextractr_0.6.1 mapview_2.10.0      stars_0.5-3        
    ##  [4] abind_1.4-5         sf_0.9-7            forcats_0.5.1      
    ##  [7] stringr_1.4.0       dplyr_1.0.6         purrr_0.3.4        
    ## [10] readr_2.0.0         tidyr_1.1.3         tibble_3.1.2       
    ## [13] ggplot2_3.3.5       tidyverse_1.3.1     rgdal_1.5-23       
    ## [16] raster_3.4-13       sp_1.4-5           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         jsonlite_1.7.2     modelr_0.1.8       assertthat_0.2.1  
    ##  [5] stats4_4.1.0       cellranger_1.1.0   yaml_2.2.1         pillar_1.6.2      
    ##  [9] backports_1.2.1    lattice_0.20-44    glue_1.4.2         digest_0.6.27     
    ## [13] rvest_1.0.1        colorspace_2.0-1   htmltools_0.5.1.1  pkgconfig_2.0.3   
    ## [17] broom_0.7.9        haven_2.4.1        webshot_0.5.2      scales_1.1.1      
    ## [21] satellite_1.0.2    tzdb_0.1.2         proxy_0.4-26       generics_0.1.0    
    ## [25] ellipsis_0.3.2     pacman_0.5.1       withr_2.4.2        cli_3.0.1         
    ## [29] magrittr_2.0.1     crayon_1.4.1       readxl_1.3.1       evaluate_0.14     
    ## [33] fs_1.5.0           fansi_0.5.0        xml2_1.3.2         lwgeom_0.2-6      
    ## [37] class_7.3-19       tools_4.1.0        hms_1.1.0          lifecycle_1.0.0   
    ## [41] munsell_0.5.0      reprex_2.0.0       compiler_4.1.0     e1071_1.7-7       
    ## [45] rlang_0.4.11       classInt_0.4-3     units_0.7-2        grid_4.1.0        
    ## [49] rstudioapi_0.13    htmlwidgets_1.5.3  crosstalk_1.1.1    leafem_0.1.6      
    ## [53] base64enc_0.1-3    rmarkdown_2.9      gtable_0.3.0       codetools_0.2-18  
    ## [57] DBI_1.1.1          R6_2.5.0           lubridate_1.7.10   knitr_1.33        
    ## [61] utf8_1.2.1         KernSmooth_2.23-20 stringi_1.6.1      parallel_4.1.0    
    ## [65] Rcpp_1.0.7         png_0.1-7          vctrs_0.3.8        leaflet_2.0.4.1   
    ## [69] dbplyr_2.1.1       tidyselect_1.1.1   xfun_0.23
