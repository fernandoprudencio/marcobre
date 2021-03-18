#' @title
#' classification model
#'
#' @description
#' building of the classification model
#'
#' @author Fernando Prudencio and Ida Vilca
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "sf", "stringr", "randomForest", "caret", "doParallel",
  "modeest"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(raster)
library(sf)
library(stringr)
library(randomForest)
library(caret)
library(doParallel)
library(modeest)
library(rgdal)

#' READ VECTORIAL DATA
samples <- st_read(
  dsn = "data/vector/s_final_2020.gpkg",
  layer = "s_final_2020", as_tibble = T
) %>%
  rename(
    "Name" = "class",
    "clase" = "value"
  ) %>%
  drop_na() %>%
  st_as_sf()

#' BUILDING OF MODEL
#'   Location of predictors by year
sen2 <-
  list.files(
    "data/raster/stack/masked",
    recursive = F,
    full.names = T
  )

# for (i in 1:length(fldrs)) {
  i <- 3
  #' Read of predictors stacked
  cov.raster <- brick(sen2[i])
  names(cov.raster) <-
    c("blue", "green", "red", "nir", "ndvi", "ndwi", "elev")

  #' Extract predictor values from samples
  df <- raster::extract(cov.raster, samples, df = T) %>%
    as_tibble() %>%
    left_join(
      dplyr::mutate(samples, ID = 1:n()),
      by = "ID"
    ) %>%
    dplyr::select(-geom, -Name, ID) %>%
    dplyr::rename(id.cls = clase) %>%
    dplyr::mutate(id.cls = as.factor(id.cls)) %>%
    drop_na()
  
  #' Select training and testing data
  set.seed(2020)
  random.sample <- createDataPartition(y = df$id.cls, p = 0.7, list = FALSE)
  training <- df[random.sample, ]
  testing <- df[-random.sample, ]

  #' Building of model
  #'   control parameters for train
  fitControl <- trainControl(method = "cv", number = 3)
  #'   train model
  rf.model <- train(
    id.cls ~ nir + red + green + blue + ndvi + ndwi + elev,
    data = training,
    method = "parRF",
    ntree = 500,
    na.action = na.exclude,
    trControl = fitControl
  )
  #'   save trainned model
  save(rf.model, file = "data/rf.model_2018.RData")
  #'   test model
  rf.prdct <- predict(rf.model, newdata = dplyr::select(testing, -id.cls))
  #'   build confusion matrix
  con.mtx <- confusionMatrix(
    data = rf.prdct, testing$id.cls, dnn = c("original", "predicted")
  )
  #'   calculate kappa value
  kappa <- kappa(con.mtx$table)
  #'   predict raster dataset
  result.raster <- raster::predict(cov.raster, rf.model)

  #' Apply focal function to classified raster
  #'   build function
  # fill.na <- function(x, i = 13) {
  #   if (is.na(x)[i]) {
  #     return(mlv(x, method = "mfv", na.rm = T)[1])
  #   } else {
  #     return(x[i])
  #   }
  # }
  #'   apply function
  # result.fill <- raster::focal(
  #   result.raster,
  #   w = matrix(1, 5, 5),
  #   fun = fill.na,
  #   pad = TRUE, na.rm = FALSE
  # )

  #' If you want to apply majority filter
  #'   run it
  # result.fill <- raster::focal(
  #   result.fill,
  #   w = matrix(1, 3, 3),
  #   fun = function(x) {
  #     mlv(x, method = "mfv", na.rm = T)[1]
  #   },
  #   pad = TRUE, na.rm = FALSE
  # )

  #' Write kappa value
  write.table(
    kappa$coef,
    sprintf(
      "data/text/kappa_%1$s.txt",
      basename(sen2[i]) %>% str_sub(-8, -5)
    )
  )
  #' Write confusion matrix
  write.csv(
    con.mtx$table,
    sprintf(
      "data/text/testing/cmatrix_%1$s.csv",
      basename(sen2[i]) %>% str_sub(-8, -5)
    )
  )
  #' Write classified raster
  writeRaster(
    result.raster,
    sprintf(
      "data/raster/landcover/cover_%1$s.tif",
      basename(sen2[i]) %>% str_sub(-8, -5),
      datatype = "INT2S", overwrite = T
    )
  )
# }