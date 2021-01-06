#' @title Calculates PDP for multiple models 
#' @description Calculates partial dependency plots (PDPs) from multiple h2o models.
#' @param models a list of h2o models.
#' @param data a list of datasets.
#' @param vars a character vector of variables to create PDPs off. 
#' @return a list of data.tables containing the calculated PDPs for each model. Each data.table
#' has the outputs for each variable in `vars` combined into the one data.table.  
#' @details Creates a list of data.tables. Each data.table corresponds to the calculated PDPs 
#' values from a single model. In each data.table, contains the PDPs values for each variable
#' combined together into a single data.table.
#' 
#' For creating pdps, it is recommended to instead use the \code{nano_pdp} function 
#' which is a wrapper for a series of functions which creates pdps. It is able to create
#' pdps directly from a nano object, for both single and multi models, and has the option
#' to return plots of the pdps.
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
#'  # build model
#'  grid <- h2o.grid(x               = var,
#'                   y               = response,
#'                   training_frame  = train,
#'                   algorithm       = "randomForest",
#'                   hyper_params    = list(ntrees = 1:2),
#'                   nfolds          = 3,
#'                   seed            = 628)
#'  model <- h2o.getModel(grid@model_ids[[1]])
#'  
#'  # calculate pdp
#'  nano_multi_pdp(model, property_prices, c("lot_size", "income"))
#'  
#'  }
#' }
#' @rdname nano_multi_pdp
#' @export 



nano_multi_pdp <- function (models, data, vars) {
  
  if (!is.list(models)) {
    stop("`models` must be a list.", 
         call. = FALSE)
  }

  if (!all(grepl("H2O", sapply(models, function(x) as.vector(class(x))))) | 
      !all(grepl("Model", sapply(models, function(x) as.vector(class(x)))))) {
    stop("`models` must be a list of h2o models.", 
         call. = FALSE)
  }
  
  if (!is.list(data)) {
    stop("`data` must be a list.",
         call. = FALSE)
  }
  
  # convert data to a list of h2oframes
  if (length(data) == 1 & length(models) != 1) {
    data <- rep(list(data[[1]]), length(models))
  }
  
  for (model in models) {
    if (!all(vars %in% model@parameters$x)) {
      stop("`vars` must be predictors in all the models in `models`.",
           call. = FALSE)
    }
  }
  
  # calculate pdps for each model, for each variable
  result <- list()
  for (i in 1:length(models)) {
    model <- models[[i]]
    data_mod <- data[[i]]
    result[[i]] <- nano_single_pdp(model, data_mod, vars)
  }
  return(result)
}
