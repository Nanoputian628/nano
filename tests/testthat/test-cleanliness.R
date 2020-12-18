context("check cleanliness function")
library(testthat)
library(nano)
library(data.table)

data("property_prices")

clean_smry <- try(cleanliness_smry(property_prices))
test_that("no error in running function", {
  expect_false(inherits(clean_smry, "try-error"))
})

test_that("number of blanks are correct", {
  expect_equal(sum(clean_smry$blanks), 0)
})

test_that("number of nas are correct", {
  expect_equal(sum(clean_smry$nas), 0)
})

test_that("number of duplicates are correct", {
  expect_equal(clean_smry$duplicates, "Dataset has no duplicates!")
})

test_that("number of special characters are correct", {
  expect_equal(sum(clean_smry$special_chars), 0)
})

test_that("number of outliers are correct", {
  expect_equal(sum(clean_smry$outliers), 82)
})

clean_smry2 <- cleanliness_smry(property_prices, outlier_sd = 5)
test_that("number of outliers decrease", {
  expect_equal(sum(clean_smry$outliers) > sum(clean_smry2$outliers), TRUE)
})


