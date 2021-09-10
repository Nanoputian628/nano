library(testthat)
library(nano)
library(data.table)

context("check data_prep function")

data("property_prices_unclean")

data <- copy(property_prices_unclean)
set.seed(2020)
data <- data[sample(nrow(data), 100)]

data_try <- try(data_prep(data          = data, 
                          response      = "sale_price",
                          split_or_fold = 5,
                          holdout_ratio = 0.1,
                          unique_row    = TRUE,
                          rm_low_var    = TRUE,
                          freq_thresh   = 95/5,
                          impute        = TRUE,
                          rm_outliers   = 10,
                          impute_method = "mice",
                          impute_ignore = c("dist_to_park", "dist_to_sealed_road", 
                                            "dist_to_school", "sale_date"),
                          pred_ignore   = c("sale_price"),
                          vif_select    = FALSE,
                          thresh        = 6,
                          balance       = FALSE,
                          scale         = TRUE,
                          target_encode = TRUE,
                          encode_cols   = "sale_qtr",
                          blend         = TRUE,
                          encode_inflec = 50,
                          noise         = 0.015))
test_that("no error in running function", {
  expect_false(inherits(data_try, "try-error"))
})
