library(nano)
library(h2o)

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

create_rf(hyper_params1, "grid_1")
create_rf(hyper_params2, "grid_2")
create_rf(hyper_params3, "grid_3")

h2o.saveGrid("./model", "grid_1")
h2o.saveGrid("./model", "grid_2")
h2o.saveGrid("./model", "grid_3")

h2o.shutdown(prompt = FALSE)


