context("check impute function")
library(testthat)
library(nano)
library(data.table)
library(mice)

data("property_prices")
data <- copy(property_prices)
data$sale_price[sample(1:nrow(data)     , 20)] <- NA
data$lot_size[sample(1:nrow(data)       , 20)] <- NA
data$dist_to_highway[sample(1:nrow(data), 20)] <- NA
data$air_noise[sample(1:nrow(data)      , 20)] <- NA

impute_try <- try(impute(data))
test_that("no error in running function", {
  expect_false(inherits(impute_try, "try-error"))
})

test_that("length of mice_method check", {
  expect_that(impute(data, mice_method = c("pmm", "pmm")), 
              throws_error("`mice_method` must either be a vector of length 1 or length equal to number of columns in `data`. If specifying the full vector, ensure to include blanks for columns which are not to imputed."))
})

test_that("pred_ignore check", {
  expect_that(impute(data, pred_ignore = 1:3), 
              throws_error("'pred_ignore` must be column names in `data`."))
})

test_that("impute_ignore check", {
  expect_that(impute(data, impute_ignore = c("LOT_SIZE")), 
              throws_error("'impute_ignore` must be column names in `data`."))
})

impute_dat <- impute(data, impute_ignore = "lot_size")
test_that("impute_ignore works correctly", {
  expect_equal(sum(is.na(impute_dat$imputed_data[["lot_size"]])), 20) 
})

data$factor <- rep(c("A", "B", "C", "A"), 125)
data$factor[sample(1:nrow(data), 20)] <- NA
test_that("column type check", {
  expect_that(impute(data, method = "mean/mode"), 
              throws_error("Columns to be imputed using `mean/mode` method must either be factor or numeric type."))
})

data$factor <- as.factor(data$factor)
impute_dat <- impute(data, method = "mean/mode")
test_that("impute all values", {
  expect_equal(sum(is.na(impute_dat)), 0)
})







