context("check switch_model and delete_grid functions")
library(testthat)
library(nano)
library(h2o)
library(data.table)

# setup -----
data("property_prices")
var <- setdiff(colnames(property_prices), "sale_price")

h2o.init()
train <- as.h2o(property_prices)

hyper_params1 = list(ntrees = 1:2)
hyper_params2 = list(ntrees = 3:4)
hyper_params3 = list(ntrees = 5:6)

create_rf <- function(hyper_params){
  grid <- h2o.grid(x               = var,
                   y               = "sale_price",
                   training_frame  = train,
                   algorithm       = "randomForest",
                   hyper_params    = hyper_params,
                   nfolds          = 3,
                   seed            = 628)
}

grid_3 <- create_rf(hyper_params1)
grid_4 <- create_rf(hyper_params2)
#grid_3 <- create_rf(hyper_params3)

model_3 <- h2o.getModel(grid_1@model_ids[[1]])
model_4 <- h2o.getModel(grid_2@model_ids[[1]])
#model_3 <- h2o.getModel(grid_3@model_ids[[1]])



