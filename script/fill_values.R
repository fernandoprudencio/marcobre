rm(list = ls())
library(raster)
library(tidyverse)
library(sf)

part1 <- brick("rapideye/part1_resample.tif")
part1[part1 == 255] <- NA
writeRaster(part1, "rapideye/part1_resample_filled.tif", datatype = "INT1U")


part2 <- brick("rapideye/part2_resample.tif")
part2[part2 == 255] <- NA
writeRaster(part2, "rapideye/part2_resample_filled.tif", datatype = "INT1U")

part3 <- brick("rapideye/part3_resample.tif")
part3[part3 == 255] <- NA
writeRaster(part3, "rapideye/part3_resample_filled.tif", datatype = "INT1U", overwrite = T)

part4 <- brick("data/raster/kompsat/mosaic.tif")
part4_banb1 <- part4[[1]]
part4_banb1[part4_banb1 == 255] <- NA
part4_banb1[!is.na(part4_banb1)] <- 1

mosaic <- brick("data/raster/kompsat/mosaic_toa.tif")
mosaic_fill <- mosaic * part4_banb1
writeRaster(mosaic_fill, "data/raster/kompsat/mosaic_filled.tif", overwrite = T)
mosaic_fill_b1 <- mosaic_fill[[1]]
mosaic_fill_b1[!is.na(mosaic_fill_b1)] <- 2
mosaic_fill_b1[is.na(mosaic_fill_b1)] <- 1
mosaic_fill_b1[mosaic_fill_b1 == 2] <- NA

sen2020 <- brick("data/raster/sentinel/sentinel2020_utm_norm.tif")
# sen2020_resample <- raster::resample(sen2020, mosaic_fill_b1)
sen2020_resample_fill <- sen2020 * mosaic_fill_b1
writeRaster(sen2020_resample_fill, "data/raster/sentinel/sentinel2020_utm_switch.tif", overwrite = T)

#' after mosaicking in ENVI
mosaic_sen <- brick("data/raster/kompsat/mosaic_with_sentinel2.dat") %>% "*"(1)
plot(mosaic_sen[[1]])
mosaic_sen[mosaic_sen == 0] <- NA
writeRaster(mosaic_sen, "data/raster/kompsat/mosaic_with_sentinel_filled.tif", overwrite = T)
