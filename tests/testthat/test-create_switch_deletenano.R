context("check contructor, switch and delete functions")
library(testthat)
library(nano)
library(h2o)
library(data.table)

data("property_prices")
var <- setdiff(colnames(property_prices), c("sale_price"))

h2o.init()
train <- as.h2o(property_prices)

hyper_params1 = list(ntrees = 1:2)
hyper_params2 = list(ntrees = 3:4)

create_rf <- function(hyper_params){
  grid <- h2o.grid(x               = var,
                   y               = "sale_price",
                   training_frame  = train,
                   algorithm       = "randomForest",
                   hyper_params    = hyper_params,
                   nfolds          = 3,
                   seed            = 628)
}

grid_1 <- create_rf(hyper_params1)
grid_2 <- create_rf(hyper_params2)

model_1 <- h2o.getModel(grid_1@model_ids[[1]])
model_2 <- h2o.getModel(grid_2@model_ids[[1]])

# test constructor functions ----
nano <- try(create_nano())
test_that("no error in creating nano object from 'create_nano' function with defaults", {
  expect_false(inherits(nano, "try-error"))
})

nano <- try(create_nano(grid = list(grid_1), 
                        model = list(model_1), 
                        data = list(property_prices)))
test_that("no error in creating nano object from 'create_nano' function with inputs", {
  expect_false(inherits(nano, "try-error"))
})

nano <- try(create_nano(grid  = list(grid_1, grid_2), 
                        model = list(model_1), 
                        data  = list(property_prices)))
test_that("no error when number of models is less than number of grids", {
  expect_false(inherits(nano, "try-error"))
})

test_that("error message produced for different length of grid and n_model", {
  expect_that(create_nano(grid    = list(grid_1, grid_2), 
                          model   = list(model_1), 
                          data    = list(property_prices), 
                          n_model = 1),
              throws_error("`n_model` must equal number of build models"))
})

## This check works but doesn't work when running devtools::test() since doesn't 
## correctly read the "". 
# test_that("error message produced for wrong type of grid", {
#   expect_that(create_nano(grid    = list(1), 
#                           model   = list(model_1), 
#                           data    = list(property_prices)),
#               throws_error("no applicable method for 'create_Grid' applied to an object of class \"c('double', 'numeric')\""))
# })

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




# test switch_model -----
nano <- create_nano(grid  = list(grid_1, grid_2),
                    model = list(model_1, model_2),
                    data  = list(property_prices, property_prices))


nano_try <- try(switch_model(nano, grid_2@model_ids[[2]], 2))
test_that("no error in switching models in a grid", {
  expect_false(inherits(nano_try, "try-error"))
})

test_that("error message produced for model_id not in specified grid", {
  expect_that(switch_model(nano, "grid_2_model_3", 2),
              throws_error("`model_id` is not in the selected grid."))
})

test_that("error message produced for incorrect model_no", {
  expect_that(switch_model(nano, grid_2@model_ids[[2]], 4),
              throws_error("`model_no` is greater than number of models in object."))
})

nano_switch <- switch_model(nano, grid_2@model_ids[[2]], 2)
test_that("check if model is correctly switched", {
  expect_equal(nano_switch$model[[2]]@model_id, grid_2@model_ids[[2]])
})



# test delete_grid -----
nano_try <- try(delete_grid(nano, 2))
test_that("no error in detelting grid from nano object", {
  expect_false(inherits(nano_try, "try-error"))
})

test_that("error message produced for incorrect format for model_no", {
  expect_that(delete_grid(nano, "grid_2"),
              throws_error("`model_no` must be numeric type."))
})

nano_delete <- delete_grid(nano, 2)
test_that("check if end grid is correctly deleted", {
  expect_equal(names(nano_delete$grid), c("grid_1", rep("", 9)))
})

test_that("check if n_model is correctly updated", {
  expect_equal(nano_delete$n_model, 1)
})

nano_delete <- delete_grid(nano, 1)
test_that("check if middle grid is correctly deleted", {
  expect_equal(nano_delete$grid[[1]]@Grid_id, grid_2@grid_id)
})

test_that("check if names are correctly updated", {
  expect_equal(names(nano_delete$grid), c("grid_1", rep("", 9)))
})

h2o.removeAll()
h2o.shutdown(prompt = FALSE)

