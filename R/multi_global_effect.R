#' @title Global Interpretation for Multiple Models
#' @description Creates ALEs, PDPs and ICEs for variable(s) for multiple h2o models.
#' @param models a list of h2o model.
#' @param data a list of dataseta. If the underlying dataset is the same for each model, can
#' only input a list with a single element.
#' @param vars a list of character strings. Elements are variables to calculate global effects 
#' of. For "ale" and "pdp" method, able to calculate two-way variable interaction global effect.
#' To specify two-way interaction, enter pair of variables as a vector in the list. For example:
#' list(v1, c(v2, v3), v4) will calculate the global effects for v1 and v4, and then the 
#' two-way effect of v2 and v3. 
#' @param max_levels a numeric. Maximum number of unique levels to calculate pdp for each 
#' variable.  
#' @param method a character. Takes the value "ale", "pdp" or "ice".
#' @param quantiles a numeric vector of quantiles (numbers from 0 to 1) for each ICE to be
#' calculated for. Only valid when \code{method = "ice"}. 
#' @return a list of data.tables containing values for each variable combined together and each
#' element corresponds to a separate model.
#' @details The "ale" and "pdp" method is implement using the `iml` package. The main  
#' advantage of the `iml` package is that it is extremely robust and has one of the 
#' fastest algorithmns for computing global effects. Further, it is one of the few 
#' packages while is able to calculate two-way variable interactions for ALEs and PDPs. 
#' For more details, see `iml::FeatureEffect`.
#' 
#' The "ice" method is implemented by using h2o's built in `h2o.ice` function. The reason
#' why this is preferred over iml's implementation is that `h2o.ice` allows flexibility
#' for which rows to calculate the ICE for. In the `iml` package, the ICE is calculated
#' for all rows which can become computationally intensive for large dataset. For more
#' details, see `h2o::h2o.ice`.
#' 
#' When modelling using the `nano` package, it is recommended to instead use the 
#' \code{nano_global_effect} function. This is a wrapper for a series of functions which 
#' calculates global effects. It is able to calculate the global effects directly from a 
#' nano object, for both single and multiple models, and has the option to return various plots.
#' 
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
#'  grid1 <- h2o.grid(x               = var,
#'                    y               = response,
#'                    training_frame  = train,
#'                    algorithm       = "randomForest",
#'                    hyper_params    = list(ntrees = 1:2),
#'                    nfolds          = 3,
#'                    seed            = 628)
#'  grid2 <- h2o.grid(x               = var,
#'                    y               = response,
#'                    training_frame  = train,
#'                    algorithm       = "gbm",
#'                    hyper_params    = list(ntrees = 1:2),
#'                    nfolds          = 3,
#'                    seed            = 628)
#'                    
#'  model1 <- h2o.getModel(grid1@model_ids[[1]])
#'  model2 <- h2o.getModel(grid2@model_ids[[1]])
#'  
#'  # calculate ale
#'  single_global_effect(list(model1, model2), 
#'                       list(property_prices), 
#'                       c("lot_size"))
#'  
#'  }
#' }
#' @rdname multi_global_effect
#' @export


multi_global_effect <- function (models, data, vars, max_levels = 30, method = "ale",
                                 quantiles = seq(0, 1, 0.1)) {
  
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
  
  for (model in models) {
    if (!all(unlist(vars) %in% model@parameters$x)) {
      stop("`vars` must be predictors in all the models in `models`.",
           call. = FALSE)
    }
  }
  
  if (!is.list(vars)) {
    stop("`vars` must be a list.",
         call. = FALSE)
  }
  
  if (max(sapply(vars, length)) > 2) {
    stop("Each element in `vars` must have length of 1 or 2.",
         call. = FALSE)
  }
  
  if (!is.integer(as.integer(max_levels)) | !max_levels > 0) {
    stop("`max_levels` must be an integer greater than 0.")
  }
  
  if (!method %in% c("ale", "pdp", "ice")) {
    stop("`method` must either be `ale`, `pdp` or `ice`.")
  }
  
  if (!is.numeric(quantiles)) {
    stop("`quantiles` must be numeric.",
         call. = FALSE)
  }
  
  if (quantiles[1] != 0 | quantiles[length(quantiles)] != 1) {
    stop("`quantiles` must begin with 0 and end with 1.",
         call. = FALSE)
  }
  
  # replicate data if required
  if (length(data) == 1 & length(models) != 1) {
    data <- rep(list(data[[1]]), length(models))
  }
  
  
  # calculate pdps for each model, for each variable
  result <- list()
  for (i in 1:length(models)) {
    model       <- models[[i]]
    data_mod    <- data[[i]]
    result[[i]] <- nano::single_global_effect(model      = model, 
                                              data       = data_mod, 
                                              vars       = vars,
                                              max_levels = 30,
                                              method     = method,
                                              quantiles  = quantiles)
  }
  return(result)
}
