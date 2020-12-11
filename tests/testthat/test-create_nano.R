context("check contructor functions")
library(testthat)
library(nano)
library(h2o)

data("property_prices")

h2o.init()
grid_1 <- h2o.loadGrid("./model/grid_1")
grid_2 <- h2o.loadGrid("./model/grid_2")
grid_3 <- h2o.loadGrid("./model/grid_3")

model_1 <- h2o.getModel(grid_1@model_ids[[2]])
model_2 <- h2o.getModel(grid_2@model_ids[[2]])
model_3 <- h2o.getModel(grid_3@model_ids[[2]])


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

