#' @title Calculates ICE for multiple models 
#' @description Calculates initial conditional expectations (ICEs) from multiple h2o models.
#' @param models a list of h2o models.
#' @param data a dataset. Dataset used to create `model`.
#' @param vars vector of characters. Vector containing variables in `data` to create ICEs 
#' @param max_levels a numeric. Maximum number of unique levels to calculate ICE for each 
#' variable.  
#' @param quantiles a numeric vector of quantiles (numbers from 0 to 1) for each ICE to be
#' calculated for.
#' @param targets a character vector. Only applicable for classification models. Subset of
#' levels of response variables which ICE should be calculated for. 
#' @return a list of data.tables containing the calculated ICEs for each model. Each data.table
#' has the outputs for each variable in `vars` combined into the one data.table.  
#' @details Creates a list of data.tables. Each data.table corresponds to the calculated ICEs 
#' values from a single model. In each data.table, contains the ICEs values for each variable
#' combined together into a single data.table.
#' 
#' For creating ICEs, it is recommended to instead use the \code{nano_ice} function 
#' which is a wrapper for a series of functions which creates ICEs. It is able to create
#' ICEs directly from a nano object, for both single and multi models, and has the option
#' to return plots of the ICEs.
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
#'  grid_2 <- h2o.grid(x               = var,
#'                     y               = response,
#'                     training_frame  = train,
#'                     algorithm       = "randomForest",
#'                     hyper_params    = list(ntrees = 3:4),
#'                     nfolds          = 3,
#'                     seed            = 628)
#'  
#'  model_1 <- h2o.getModel(grid_1@model_ids[[1]])
#'  model_2 <- h2o.getModel(grid_2@model_ids[[1]])
#'  
#'  # calculate ICE
#'  nano_multi_ice(models    = list(model_1, model_2), 
#'                 data      = list(property_prices), 
#'                 vars      = c("lot_size", "income"),
#'                 quantiles = seq(0, 1, 0.1))
#'  
#'  }
#' }
#' @rdname nano_multi_ice
#' @export 



nano_multi_ice <- function (models, data, vars, max_levels = 30, quantiles = seq(0, 1, 0.1),
                            target = NULL) {
  
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
  
  # calculate ices for each model, for each variable
  result <- list()
  for (i in 1:length(models)) {
    model       <- models[[i]]
    data_mod    <- data[[i]]
    result[[i]] <- nano::nano_single_ice(model      = model     , 
                                         data       = data_mod  , 
                                         vars       = vars      ,
                                         max_levels = max_levels,
                                         quantiles  = quantiles ,
                                         target     = target)
  }
  return(result)
}
