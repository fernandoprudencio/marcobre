rm(list = ls())

library(raster)
library(sf)
library(tidyverse)

poly <- st_read("data/vector/norm_poly.shp")

mosaic <- brick("data/raster/kompsat/mosaic_toa.tif")
b1_mosaic <- mosaic[[1]]
b2_mosaic <- mosaic[[2]]
b3_mosaic <- mosaic[[3]]
b4_mosaic <- mosaic[[4]]

b1_mosaic_xtc <- raster::extract(b1_mosaic, poly, df = T)
b2_mosaic_xtc <- raster::extract(b2_mosaic, poly, df = T)
b3_mosaic_xtc <- raster::extract(b3_mosaic, poly, df = T)
b4_mosaic_xtc <- raster::extract(b4_mosaic, poly, df = T)


sen2020 <- brick("data/raster/sentinel/sentinel2020_utm.tif") %>% "*" (0.0001)
sen2020_rspl <- resample(sen2020, mosaic)
b1_sen2020 <- sen2020_rspl[[1]]
b2_sen2020 <- sen2020_rspl[[2]]
b3_sen2020 <- sen2020_rspl[[3]]
b4_sen2020 <- sen2020_rspl[[4]]

b1_sen2020_xtc <- raster::extract(b1_sen2020, poly, df = T)
b2_sen2020_xtc <- raster::extract(b2_sen2020, poly, df = T)
b3_sen2020_xtc <- raster::extract(b3_sen2020, poly, df = T)
b4_sen2020_xtc <- raster::extract(b4_sen2020, poly, df = T)

#' lineal regression
b1 <- lm(b1_mosaic_xtc$mosaic_toa.1 ~ b1_sen2020_xtc$sentinel2020_utm.1)
b2 <- lm(b2_mosaic_xtc$mosaic_toa.2 ~ b2_sen2020_xtc$sentinel2020_utm.2)
b3 <- lm(b3_mosaic_xtc$mosaic_toa.3 ~ b3_sen2020_xtc$sentinel2020_utm.3)
b4 <- lm(b4_mosaic_xtc$mosaic_toa.4 ~ b4_sen2020_xtc$sentinel2020_utm.4)

summary(b1)
summary(b2)
summary(b3)
summary(b4)

#' apply lineal regression
b1_sen2020_norm <- (b1$coefficients[2] * b1_sen2020) + b1$coefficients[1]
b2_sen2020_norm <- (b2$coefficients[2] * b2_sen2020) + b2$coefficients[1]
b3_sen2020_norm <- (b3$coefficients[2] * b3_sen2020) + b3$coefficients[1]
b4_sen2020_norm <- (b4$coefficients[2] * b4_sen2020) + b4$coefficients[1]

sen2020_norm <-
  raster::stack(
    b1_sen2020_norm, b2_sen2020_norm,
    b3_sen2020_norm, b4_sen2020_norm
  )

writeRaster(sen2020_norm, "data/raster/sentinel/sentinel2020_utm_norm.tif", overwrite = T)

x <- 1:10
y <- x*.5

lm(y ~ x)
