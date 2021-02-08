context("check vif_step function")
library(testthat)
library(nano)
library(data.table)
library(fmsb)

data("property_prices")
data <- copy(property_prices)


vif_try <- try(vif_step(data))
test_that("no error in running function", {
  expect_false(inherits(vif_try, "try-error"))
})

test_that("data check", {
  expect_that(vif_step(as.matrix(data)),
              throws_error("`data` must be a dataset."))
})

test_that("ignore check", {
  expect_that(vif_step(data, ignore = c("response")), 
              throws_error("'ignore` must be column names in `data`."))
})

test_that("thresh check", {
  expect_that(vif_step(data, thresh = "2"),
              throws_error("`thresh` must be numeric."))
})

ignore <- c("sale_price", "dist_to_highway")
vif <- vif_step(data, ignore = ignore, thresh = 1.5, trace = FALSE, remove = FALSE)
test_that("correct output for remove = FALSE", {
  expect_equal(nrow(vif), ncol(data) - 1)
})

vif <- vif_step(data, ignore = ignore, thresh = 1.5, trace = TRUE, remove = TRUE)
test_that("variables are correctly ignored", {
  expect_equal(all(ignore %in% names(vif$data)), TRUE)
})

test_that("variables are correctly removed", {
  expect_equal(!any(c("dist_to_coastline", "dist_to_rail_station") %in% vif$vif$var), TRUE)
})

test_that("all VIF below threshold", {
  expect_equal(max(vif$vif$vif) < 1.5, TRUE)
})
