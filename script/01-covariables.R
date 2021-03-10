rm(list = ls())

library(raster)
library(tidyverse)

# dem <- raster("data/raster/dem/srtm_90m.tif")
dem <- raster("data/raster/dem/srtm_10m_marcona.tif")
path <- "data/raster/sentinel/sentinel2018_utm.tif"
sen2 <- brick(path) %>% "*"(.0001)

# dem_reproj <- raster::projectRaster(dem, sen2)
# dem_resample <- crop(dem_reproj, sen2) %>% resample(sen2)
# writeRaster(
#   dem_resample, "data/raster/dem/srtm_10m_marcona.tif", overwrite = T
# )

ndvi <- (sen2[[4]] - sen2[[3]])/(sen2[[4]] + sen2[[3]])
ndwi <- (sen2[[2]] - sen2[[4]])/(sen2[[2]] + sen2[[4]])

stack <- raster::stack(sen2, ndvi, ndwi, dem)
names(stack) <- c("blue", "green", "red", "nir", "ndvi", "ndwi", "elev")

name <- basename(path) %>% str_sub(9, -9)

writeRaster(
  stack,
  sprintf("data/raster/stack/stack_%1$s.tif", name),
  overwrite = T
)