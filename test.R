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
toi   <- c("2000-05-01/2000-09-30", "2001-05-01/2001-09-30")
# set the bands you want to get
bands <-  c("Nadir_Reflectance_Band1","Nadir_Reflectance_Band2", "Nadir_Reflectance_Band3")
out.path <- "data/work"
own.prefix <- "MoDiS"
aggr.time <- "P1M"
aggr.method <- "mean"
# set the function to dowload the data
# area: Area of interest (numeric vector?, Xmin, Ymin, Xmax, Ymax)
# toi: list of character vectors of datetimes (format: "YYYY-MM-DD/YYYY-MM-DD)
# bands: bands to download from the product as character vector
# out.path: path to save the data as character vector
# own.prefix: prefix for the saved files as character vector
# aggr.time: dt argument of cube.view ("P1M or sth)
# aggr.method: methd argument of cube.view ("mean" or sth)
get.the.data = function(area, time, bands, out.path, own.prefix, aggr.time, aggr.method) {
  #filter collection to find the elements we want containing the coordinates of interest
  it.obj <- s.obj %>%
    stac_search(collections = "modis-43A4-061",
                datetime = time,
                bbox     = area) %>%
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
  xmin        <- area[1]
  ymin        <- area[2]
  xmax        <- area[3]
  ymax        <- area[4]
  aoi.extent  <- st_bbox(c(xmin = xmin, xmax = xmax,
                           ymin = ymin, ymax = ymax),
                         crs = 4326)
  aoi.extent  <- aoi.extent %>% st_as_sfc() %>% st_as_sf()
  
  #project aoi to satellite image projection
  aoi.extent  <- st_bbox(st_transform(st_as_sfc(aoi.extent),wkt2))
  
  #datacube for images at acquisition time  
  v = cube_view(   srs = wkt2, 
                extent = list(  t0 = substr(toi, 1, 10), t1 = substr(toi, 12, 22),
                              left = aoi.extent$xmin, right = aoi.extent$xmax,  
                               top = aoi.extent$ymax, 
                            bottom = aoi.extent$ymin),
                    dx = 500, 
                    dy = 500, 
                    dt = aggr.time, 
           aggregation = aggr.method,
           resampling  = "bilinear")
  
  cube = raster_cube(collection, v)
  
  # Saving
  write_tif((cube),
            dir    = out.path,
            prefix = own.prefix)
}

get.the.data(area = area, 
             time = toi[1], 
            bands = bands, 
         out.path = out.path, 
       own.prefix = own.prefix, 
        aggr.time = aggr.time, 
      aggr.method = aggr.method)


