#' @title Variable Interaction 
#' @description Calculated pair-wise variable interaction in a predictive model.
#' @importFrom iml Predictor
#' @importFrom iml Interaction
#' @param nano object containing models to calculate interactions from. 
#' @param model_no the positions of each model in the list of models in the nano object for which
#' the interactions should be calculated. If not entered, the last model is taken by default.
#' @param vars a character vector of variables to calculate pair-wise interactions for. 
#' @param plot a logical specifying whether plots of the interactions should be created.
#' @return nano object with interactions of specified models calculated. Also returns a 
#' plot if \code{plot = TRUE}.
#' @details The interactions are calculated by the Friedman's H-statistic (square root of the H
#' -squared test statistic) and takes on values between 0 (no interaction) to 1 (100% of standard
#' deviation of f(x) du to interaction). This package uses the \code{Interaction} function from
#' the `iml` package to calculate the H-statistic. Please see their manual for more details.
#' 
#' For the variables specified in the `vars` argument, this funtion calculates the interaction
#' for each pair-wise interaction of that variable with every other variable used as a predictor 
#' for the specified model.
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
#'  obj <- create_nano(grid = list(grid_1),
#'                     data = list(property_prices),  
#'                     ) # since model is not entered, will take best model from grids
#'  
#'  # calculate all pair-wise interactions for the variable "lot_size"
#'  nano <- nano_interaction(nano, 1, "lot_size", FALSE) 
#'  
#'  }
#' }
#' @rdname nano_interaction
#' @export

nano_interaction <- function(nano, model_no = nano$n_model, vars, plot = FALSE) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!is.integer(as.integer(model_no))) {
    stop("`model_no` must be numeric.", 
         call. = FALSE)
  }
  
  if (min(model_no) <= 0) {
    stop("`model_no` must be greater than 0", 
         call. = FALSE)
  }
  
  if (max(model_no) > nano$n_model) {
    stop("`model_no` cannot be greater than number of models in `nano`.", 
         call. = FALSE)
  }

  if (missing(vars)) {
    stop("`vars` must be entered, there are no defaults.",
         call.= FALSE)
  }
  
  if (!is.character(vars)) {
    stop("`vars` must be a vector of character.",
         call. = FALSE)
  }
  
  for (i in model_no) {
    if (!all(vars %in% nano$model[[i]]@parameters$x)) {
      stop("`vars` must be predictors in each of the specified models.",
           call. = FALSE)
    }
  }
  
  for (i in model_no) {
    model <- nano$model[[i]]
    # create a data.table with just the features
    data <- nano$data[[i]][, model@parameters$x, with = FALSE]
    # vector with the actual responses
    res <- as.vector(nano$data[[i]][, model@parameters$y, with = FALSE])
    # create custom predict function which is compatible with IML package
    pred <- function(model, data)  {
      results <- as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(data)))
      return(results[[3L]])
    }
    # create predictor object
    pred_mod <- iml::Predictor$new(model       = model, 
                                   data        = data, 
                                   y           = res, 
                                   predict.fun = pred
                                   )
    # for each variable, calculate interactions
    interact <- list()
    for (j in 1:length(vars)) {
      var <- vars[j]
      # check if interaction has already been calculated for this model and variable
      if (!all(is.na(nano$interaction[[i]]))) {
        if (any(grepl(paste0(":", var), nano$interaction[[i]]$feature))) break
      }
      interact[[var]] <- iml::Interaction$new(pred_mod, feature = var)$results 
      # remove "." from column names
      names(interact[[var]]) <- gsub("\\.", "", names(interact[[var]]))
      interact[[var]][, var := var]
    }
    # if any interactions are calculated, append values into single data.table
    if (length(interact) > 0) {
      interact_all <- do.call(rbind, lapply(interact, data.table::as.data.table))
      # add calculated interactions to nano object 
      if (all(is.na(nano$interaction[[i]]))) {
        nano$interaction[[i]]      <- interact_all
        names(nano$interaction)[i] <- paste0("interaction_", i)
      } else {
        nano$interaction[[i]] <- rbind(nano$interaction[[i]], interact_all)
      }
    }
  }
  return(nano)
}