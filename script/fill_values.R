rm(list = ls())
library(raster)

part1 <- brick("rapideye/part1_resample.tif")
part1[part1 == 255] <- NA
writeRaster(part1, "rapideye/part1_resample_filled.tif", datatype = "INT1U")


part2 <- brick("rapideye/part2_resample.tif")
part2[part2 == 255] <- NA
writeRaster(part2, "rapideye/part2_resample_filled.tif", datatype = "INT1U")

part3 <- brick("rapideye/part3_resample.tif")
part3[part3 == 255] <- NA
writeRaster(part3, "rapideye/part3_resample_filled.tif", datatype = "INT1U", overwrite = T)
