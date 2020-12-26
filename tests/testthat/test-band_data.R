context("check band function")
library(testthat)
library(nano)
library(data.table)

data("property_prices")

data <- copy(property_prices)

band_try <- try(band_data(data, intervals = list(sale_price = 1:10)))
test_that("no error in running function", {
  expect_false(inherits(band_try, "try-error"))
})

test_that("test message for incorrect type for `intervals`.", {
  expect_that(band_data(data, intervals = 1:10), 
              throws_error("`intervals` must be a list."))
})

test_that("test message for incorrect names for `intervals`.", {
  expect_that(band_data(data, intervals = list(1:10)), 
              throws_error("names of 'intervals` must be column names in `data`."))
})

test_that("test message for different names for `intervals` and `buckets.", {
  expect_that(band_data(data, intervals = list(sale_price = 1:10), buckets = list(lot_size = 1:10)), 
              throws_error("`intervals` and `buckets` must have the same names."))
})

test_that("test message for different lengths for `intervals` and `buckets.", {
  expect_that(band_data(data, intervals = list(sale_price = 1:10), buckets = list(sale_price = 1:10)),
              throws_error("number of `buckets` must be equal to the number of desired intervals."))
})

test_that("test message for different names for `intervals` and `na_bucket.", {
  expect_that(band_data(data, intervals = list(sale_price = 1:10), na_bucket = list(lot_size = 1:10)),
              throws_error("`intervals` and `na_bucket` must have the same names."))
})

test_that("test message for incorrect length for `unmatched_buckets", {
  expect_that(band_data(data, 
                        intervals = list(sale_price = 1:10), 
                        unmatched_bucket = list(sale_price = "miss", lot_size = "miss")),
              throws_error("'unmatched_bucket` must be either be a character of length 1 or a list with length equal to length of `intervals`."))
})

data <- copy(property_prices)
intervals <- list(income               = seq(500,1000, 100), 
                  crime_rate           = seq(0.2, 0.5, 0.1),
                  dist_to_coastline    = seq(0, 30, 5),
                  dist_to_rail_station = seq(0, 20, 4))
buckets <- list(income               = as.character(1:6),
                crime_rate           = as.character(11:14),
                dist_to_coastline    = as.character(1:7),
                dist_to_rail_station = as.character(1:6))
na_bucket <- list(income               = "income",
                  crime_rate           = "crime",
                  dist_to_coastline    = "coast",
                  dist_to_rail_station = "rail")
unmatched_bucket <- list(income               = "income_u",
                         crime_rate           = "crime_u",
                         dist_to_coastline    = "coast_u",
                         dist_to_rail_station = "rail_u")
data_band1 <- try(band_data(data, 
                            intervals        = intervals, 
                            buckets          = buckets, 
                            na_bucket        = na_bucket, 
                            unmatched_bucket = unmatched_bucket, 
                            include_left     = TRUE, 
                            trunc_left       = TRUE))
data_band2 <- try(band_data(data, 
                            intervals        = intervals, 
                            na_bucket        = na_bucket, 
                            unmatched_bucket = unmatched_bucket, 
                            include_left     = TRUE, 
                            trunc_left       = TRUE))

test_that("no error in running function with all arguments", {
  expect_false(inherits(data_band1, "try-error"))
})

test_that("no error in running function with `all arguments`buckets` missing.", {
  expect_false(inherits(data_band2, "try-error"))
})
