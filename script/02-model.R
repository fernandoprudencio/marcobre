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

#' READ VECTORIAL DATA
samples <- st_read("data/vector/polygons.gpkg",
  layer = "polygons", quiet = T, as_tibble = T
) %>%
  dplyr::select(-new_id) %>%
  rename(
    "Name" = "class",
    "clase" = "value"
  )

#' BUILDING OF MODEL
#'   Location of predictors by year
sen2 <-
  list.files(
    "data/raster/sentinel",
    recursive = F,
    full.names = T
  )

# for (i in 1:length(fldrs)) {
  i <- 1
  #' Read of predictors stacked
  cov.raster <- brick(sen2[i])

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
  
  names(df)[2:5] <- c("blue", "green", "red", "nir")

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
    id.cls ~ nir + red +blue + green,
    data = training,
    method = "parRF",
    ntree = 500,
    na.action = na.exclude,
    trControl = fitControl
  )
  #'   test model
  rf.prdct <- predict(rf.model, newdata = testing)
  #'   build confusion matrix
  con.mtx <- confusionMatrix(
    data = rf.prdct, testing$id.cls, dnn = c("original", "predicted")
  )
  #'   calculate kappa value
  kappa <- kappa(con.mtx$table)
  #'   predict raster dataset
  names(cov.raster) <- c("blue", "green", "red", "nir")
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
      "data/text/%1$s.txt",
      basename(sen2[i]) %>% str_sub(1, -5)
    )
  )
  #' Write confusion matrix
  write.csv(
    con.mtx$table,
    sprintf(
      "data/text/testing/%1$s.csv",
      basename(sen2[i]) %>% str_sub(1, -5)
    )
  )
  #' Write classified raster
  writeRaster(
    result.raster,
    sprintf(
      "data/raster/landcover/%1$s.tif",
      basename(sen2[i]) %>% str_sub(1, -5),
      datatype = "INT2S", overwrite = T
    )
  )
# }