rm(list = ls())

library(raster)
library(tidyverse)
library(sf)
library(rgdal)

list <- list.files("data/raster/stack/", full.names = T, recursive = T)
study_area <- st_read("data/vector/study_area.shp") %>%
  sf::st_zm()

for (i in 1:length(list)) {
  # i = 3
  img <- raster::mask(brick(list[i]), study_area)
  writeRaster(img, sprintf("data/raster/stack/masked/%1$s", basename(list[i])))
}

# raster::hist(brick("data/raster/stack/masked/stack_2016.tif")[[1]])
# raster::hist(brick("data/raster/stack/masked/stack_2018.tif")[[1]])
# raster::hist(brick("data/raster/stack/masked/stack_2020.tif")[[1]])