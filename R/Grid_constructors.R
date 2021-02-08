#' @title Creating Grid S4 Object 
#' @description constructors for `Grid` S4 object
#' @param obj A `H2OGrid` or `H2OAutoML` object.
#' @return a `Grid` S4 object.
#' @details Creates `Grid` S4 object from a `H2OGrid` or `H2OAutoML` object. This object
#' is used as the `grid` element in a `nano` object. The `Grid` object holds useful 
#' information about the input object which will be used by other functions. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(h2o)
#'  library(nano)
#'  
#'  h2o.init()
#'  
#'  # import dataset
#'  data(property_prices)
#'  train <- as.h2o(property_prices)
#'  
#'  # set the response and predictors
#'  response <- "sale_price"
#'  var <- setdiff(colnames(property_prices), response)
#'  
#'  # build grids
#'  grid_1 <- h2o.grid(x               = var,
#'                     y               = response,
#'                     training_frame  = train,
#'                     algorithm       = "randomForest",
#'                     hyper_params    = list(ntrees = 1:2),
#'                     nfolds          = 3,
#'                     seed            = 628)
#'                     
#'  Grid_1 <- create_Grid(grid_1)
#'
#'  }
#' }


#' @rdname Grid_constructors
setClass("Grid", 
         slots = list(Grid_id = "character", 
                      model_ids = "list",
                      summary_table = "list",
                      model_type = "character",
                      meta = "list")
         )


#' @rdname Grid_constructors
create_Grid.H2OGrid <- function(obj) {
  
  ## calculate metrics slot for Grid object
  # get a single h2o model from grid to infer model type
  tmp <- h2o::h2o.getModel(obj@model_ids[[1]])
  if (class(tmp) == "H2ORegressionModel") {
    model_type = "regression"
  } else if (class(tmp) == "H2OMultinomialModel") {
    model_type = "multinomial classification"
  } else {
    model_type = "binomial classification"
  }

  # set of metrics to show in summary table  
  if (grepl("classification", model_type)) {
    met <- c("mean_class_err", "logloss", "rmse", "mse")
  } else {
    met <- c("mean_residual_dev", "rmse", "mse", "mae", "rmsle")
  }
  
  # identify which metric to show in metric slot 
  if (!is.null(tmp@model$cross_validation_metrics@metrics)) {
    metric_type <- "cv"
  } else if (!is.null(tmp@model$validation_metrics@metrics)) {
    metric_type <- "test"  
  } else {
    metric_type <- "train"
  }
  
  metrics_all <- data.table::data.table()
  for (i in 1:length(obj@model_ids)) {
    metrics <- nano:::model_metrics(h2o.getModel(obj@model_ids[[i]]))
    metrics <- data.table::as.data.table(metrics[[paste0(metric_type, "_metrics")]][met])
    metrics_all <- rbind(metrics_all, metrics)
  }
  metrics_all[, model_id := unlist(obj@model_ids)]
  data.table::setcolorder(metrics_all, c("model_id", met))  
  
  ## TO DO: create new S4 class for summary_table and create new print method
  ##        to print the message when viewing the table
  Grid <- new("Grid",
              Grid_id       = obj@grid_id, 
              model_ids     = obj@model_ids, 
              # summary_table = list(paste0("Showing ", metric_type, " metrics"),
              #                      metrics = metrics_all), 
              summary_table = list(metric_smry = metrics_all,
                                   grid_smry   = obj@summary_table),
              meta          = list(origin_class = "H2OGrid",
                                   tune_hyper_params = obj@hyper_names,
                                   failed_params     = obj@failed_params,
                                   failure_details   = obj@failure_details,
                                   failed_raw_params = obj@failed_raw_params,
                                   description       = ""
              ))
}


#' @rdname Grid_constructors
create_Grid.H2OAutoML <- function(obj) {
  
  ## calculate metrics slot for Grid object
  # determine type of model
  if (class(obj@leader) == "H2ORegressionModel") {
    model_type = "regression"
  } else if (class(obj@leader) == "H2OMultinomialModel") {
    model_type = "multinomial classification"
  } else {
    model_type = "binomial classification"
  }
  
  # metrics to calculate
  if (grepl("classification", model_type)) {
    met <- c("mean_class_err", "logloss", "rmse", "mse")
  } else {
    met <- c("mean_residual_dev", "rmse", "mse", "mae", "rmsle")
  }
  
  # which metrics to show in summary slot
  if (!is.null(obj@leader@model$cross_validation_metrics@metrics)) {
    metric_type <- "cv"
  } else if (!is.null(obj@leader@model$validation_metrics@metrics)) {
    metric_type <- "test"  
  } else {
    metric_type <- "train"
  }
  
  # calculate metrics
  metrics_all <- data.table::data.table()
  for (i in 1:nrow(obj@leaderboard[["model_id"]])) {
    metrics <- nano:::model_metrics(h2o.getModel(data.table::as.data.table(obj@leaderboard)[["model_id"]][i]))
    metrics <- data.table::as.data.table(metrics[[paste0(metric_type, "_metrics")]][met])
    metrics_all <- rbind(metrics_all, metrics)
  }
  metrics_all[, model_id := as.data.table(obj@leaderboard)[["model_id"]]]
  data.table::setcolorder(metrics_all, c("model_id", met))  
  
  # assign slots
  Grid <- new("Grid",
              Grid_id       = obj@project_name,
              model_ids     = as.list(obj@leaderboard[["model_id"]]),
              # summary_table = list(paste0("Showing ", metric_type, " metrics"),
              #                      metrics = metrics_all), 
              summary_table = list(metric_smry = metrics_all),
              meta = list(origin_class    = "H2OAutoML",
                          modelling_steps = obj@modeling_steps,
                          description     = ""
                          ))
}


#' @rdname Grid_constructors
create_Grid.Grid <- function(obj) {
  obj
}


#' @rdname Grid_constructors
create_Grid <- function(obj) {
  UseMethod("create_Grid")
}


setMethod("create_Grid",
          "H2OGrid",
          create_Grid.H2OGrid)

setMethod("create_Grid",
          "H2OAutoML",
          create_Grid.H2OAutoML)

setMethod("create_Grid",
          "Grid",
          create_Grid.Grid)
