context("check switch_model and delete_grid functions")
library(testthat)
library(nano)
library(h2o)
library(data.table)

# setup -----
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



nano <- create_nano(grid = list(grid_1, grid_2, grid_3),
                    model = list(model_1, model_2, model_3),
                    data = list(property_prices, property_prices, property_prices))


# test switch_model -----
nano_try <- try(switch_model(nano, "grid_2_model_1", 2))
test_that("no error in switching models in a grid", {
  expect_false(inherits(nano_try, "try-error"))
})

test_that("error message produced for model_id not in specified grid", {
  expect_that(switch_model(nano, "grid_2_model_3", 2),
              throws_error("`model_id` is not in the selected grid."))
})

test_that("error message produced for incorrect model_no", {
  expect_that(switch_model(nano, "grid_2_model_1", 4),
              throws_error("`model_no` is greater than number of models in object."))
})

nano_switch <- switch_model(nano, "grid_2_model_1", 2)
test_that("check if model is correctly switched", {
  expect_equal(nano_switch$model[[2]]@model_id, "grid_2_model_1")
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

nano_delete <- delete_grid(nano, 3)
test_that("check if end grid is correctly deleted", {
  expect_equal(names(nano_delete$grid), c("grid_1", "grid_2", rep("", 8)))
})

test_that("check if n_model is correctly updated", {
  expect_equal(nano_delete$n_model, 2)
})

nano_delete <- delete_grid(nano, 2)
test_that("check if middle grid is correctly deleted", {
  expect_equal(nano_delete$grid[[2]]@grid_id, "grid_3")
})

test_that("check if names are correctly updated", {
  expect_equal(names(nano_delete$grid), c("grid_1", "grid_2", rep("", 8)))
})

h2o.shutdown(prompt = FALSE)
