context("check missing_pattern function")
library(testthat)
library(nano)
library(plotly)


data("property_prices")
data <- copy(property_prices)

data$sale_price[sample(1:nrow(data)     , 20)] <- NA
data$lot_size[sample(1:nrow(data)       , 20)] <- NA
data$dist_to_highway[sample(1:nrow(data), 20)] <- NA
data$air_noise[sample(1:nrow(data)      , 20)] <- NA
data$dist_to_school[sample(1:nrow(data) , 20)] <- NA


miss_try <- try(missing_pattern(data))
test_that("no error in running function", {
  expect_false(inherits(miss_try, "try-error"))
})




