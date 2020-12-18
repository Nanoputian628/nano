context("check contructor functions")
library(testthat)
library(nano)
library(h2o)
library(data.table)

data("property_prices")

h2o.init()
train <- as.h2o(property_prices)

hyper_params1 = list(ntrees = 1:2)
hyper_params2 = list(ntrees = 3:4)
hyper_params3 = list(ntrees = 5:6)

create_rf <- function(hyper_params, grid_id){
  grid <- h2o.grid(x               = setdiff(colnames(property_prices), "sale_price"),
                   y               = "sale_price",
                   training_frame  = train,
                   algorithm       = "randomForest",
                   grid_id         = grid_id,
                   hyper_params    = hyper_params,
                   nfolds          = 3,
                   seed            = 628)
}

grid_1 <- create_rf(hyper_params1, "grid_1")
grid_2 <- create_rf(hyper_params2, "grid_2")
grid_3 <- create_rf(hyper_params3, "grid_3")

model_1 <- grid_1@model_idsa[[1]]
model_2 <- grid_2@model_idsa[[1]]
model_3 <- grid_3@model_idsa[[1]]


nano <- try(create_nano())
test_that("no error in creating nano object from 'create_nano' function with defaults", {
  expect_false(inherits(nano, "try-error"))
})

nano <- try(create_nano(grid = list(grid_1), model = list(model_1), data = list(property_prices)))
test_that("no error in creating nano object from 'create_nano' function with inputs", {
  expect_false(inherits(nano, "try-error"))
})

test_that("error message produced for different lengths", {
  expect_that(create_nano(grid  = list(grid_1, grid_2), 
                          model = list(model_1), 
                          data  = list(property_prices)),
              throws_error("number of `grids`, `models` and `datasets` must be equal"))
})

test_that("error message produced for different length of grid and n_model", {
  expect_that(create_nano(grid    = list(grid_1, grid_2), 
                          model   = list(model_1), 
                          data    = list(property_prices), 
                          n_model = 1),
              throws_error("`n_model` must equal number of build models"))
})

test_that("error message produced for wrong type of grid", {
  expect_that(create_nano(grid    = list(1), 
                          model   = list(model_1), 
                          data    = list(property_prices)),
              throws_error("All `grid` values must be H2OGrid class"))
})

test_that("error message produced for wrong type of grid", {
  expect_that(create_nano(grid    = list(grid_1), 
                          model   = list(1), 
                          data    = list(property_prices)),
              throws_error("All `model` values must be a H2O model"))
})

test_that("error message produced for wrong type of grid", {
  expect_that(create_nano(grid    = list(grid_1), 
                          model   = list(model_1), 
                          data    = list(1)),
              throws_error("All `data` values must be data.table class"))
})

h2o.shutdown(prompt = FALSE)

