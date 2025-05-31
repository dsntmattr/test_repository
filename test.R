# Getting the raw MODIS scenes for our study area, aggregated to one picture for each month.
# 0.0 Packages ----
# Load packages 
library(rstac)
library(magrittr)
library(terra)
library(sf)
library(gdalcubes)

# Gettin ready ----
# removing all variables from the current environment.
rm(list=ls())

# link to collection
s.obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

# Load boundig box vector
load("data/bbox.vector.RData")
#set area of interest
aoi   <- bbox.vector
#set time periods of interest
toi   <- list("2000-05-01/2000-09-30", "2001-05-01/2001-09-30")
# set the bands you want to get
bands <-  c("Nadir_Reflectance_Band1","Nadir_Reflectance_Band2", "Nadir_Reflectance_Band3")


get.the.data = function(area, time, bands) {
  #filter collection to find the elements we want containing the coordinates of interest
  it.obj <- s.obj %>%
    stac_search(collections = "modis-43A4-061",
                datetime=time,
                bbox = aoi) %>%
    get_request() %>%
    items_sign(sign_fn = sign_planetary_computer())
  
  #get the coordinate system
  wkt2 <- it.obj$features[[1]]$properties$`proj:wkt2`
  #get the acquisition date
  img.dates <- NULL
  for (i in 1:length(it.obj$features)) {
    img.dates <- c(img.dates,substr(it.obj$features[[i]]$properties$datetime, 1, 10))
  }
  img.dates <- rev(unique(img.dates))
  #define the Bands you want to extract
  assets      <- bands
  collection  <- stac_image_collection(it.obj$features, asset_names = assets)
  #define the study area for the cut
  xmin        <- aoi[1]
  ymin        <- aoi[2]
  xmax        <- aoi[3]
  ymax        <- aoi[4]
  aoi.extent  <- st_bbox(c(xmin = xmin, xmax = xmax,
                           ymin = ymin, ymax = ymax),
                         crs = 4326)
  aoi.extent  <- aoi.extent %>% st_as_sfc() %>% st_as_sf()
  
  #project aoi to satellite image projection
  aoi.extent  <- st_bbox(st_transform(st_as_sfc(aoi.extent),wkt2))
  
  #datacube for images at acquisition time  
  v     = cube_view(srs = wkt2,  extent = list(t0 = substr(toi, 1, 10), t1 = substr(toi, 12, 22),
                                               left = aoi.extent$xmin, right = aoi.extent$xmax,  top = aoi.extent$ymax, bottom = aoi.extent$ymin),
                    dx = 500, dy = 500, dt="P1M", aggregation = "mean", resampling = "bilinear")
  cube = raster_cube(collection, v)
  
  # Saving
  write_tif((cube),
            dir="work/P1M",
            prefix='MODIS_')
}

get.the.data(aoi, toi[1], bands)


