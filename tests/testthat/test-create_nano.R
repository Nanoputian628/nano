context("check contructor functions")
library(testthat)
library(nano)
library(h2o)
library(data.table)

data("property_prices")

h2o.init()
train <- as.h2o(property_prices)

hyper_params = list(ntrees = c(1, 2))
grid <- h2o.grid(x               = setdiff(colnames(property_prices), "sale_price"),
                 y               = "sale_price",
                 training_frame  = train,
                 algorithm       = "randomForest",
                 grid_id         = "grid_1",
                 hyper_params    = hyper_params,
                 nfolds          = 3,
                 seed            = 628)
grid_1 <- h2o.getGrid("grid_1", sort_by = "mse", decreasing = FALSE)
model  <- h2o.getModel(grid_1@model_ids[[1]])

nano <- try(create_nano())
test_that("no error in creating nano object from 'create_nano' function with defaults", {
  expect_false(inherits(nano, "try-error"))
})

nano <- try(create_nano(grid = list(grid), model = list(model), data = list(property_prices)))
test_that("no error in creating nano object from 'create_nano' function with inputs", {
  expect_false(inherits(nano, "try-error"))
})

test_that("error message produced for different lengths", {
  expect_that(create_nano(grid = list(grid, grid), model = list(model), data = list(property_prices)),
              throws_error("number of `grids`, `models` and `datasets` must be equal"))
})

h2o.shutdown(prompt = FALSE)

