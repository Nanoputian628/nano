#' @title Create PDP 
#' @description Creates partial dependency plots (PDPs) from h2o models stored i nano
#' objects.
#' @param model_no the position of the model in the list of models in the nano object. If not 
#' entered, the last model is taken by default.
#' @param plot a logical specifying whether the variable importance should be plotted.
#' @param n_var only relevant if \code{plot = TRUE}. Specifies the maximum number of variables 
#' which should be shown in the plot.
#' @param subtitle subtitle for the plot.
#' @param save a logical specifying whether the plot should be saved into working directory.
#' @param subdir sub directory in which the plot should be saved.
#' @param file_name file name of the saved plot.   
#' @return nano object with variable importance of specified model calculated. Also returns a plot
#' if \code{plot = TRUE}.
#' @details Function first checks if the variable importance of the specified model has already 
#' been calculated (by checking in the list \code{nano$varimp}). If it has not been calculated, 
#' then the variable importance will be calculated and the relevant slot in \code{nano$varimp}
#' will be filled out. 
#' 
#' If \code{plot = TRUE}, a plot of the variable importance will also be returned. The plot can
#' be saved in a subfolder of the working directory by using the `save` and `subdir` arguments.
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
#' @rdname nano_pdp
#' @export 

nano_pdp <- function (nano, model_no = NA, vars, plot = TRUE, n_var = 10, subtitle = NA, save = FALSE, 
                      subdir = NA, file_name) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!is.na(model_no)) {
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
  }
  
  if (!missing(vars)) {
    stop("`vars` must be entered, there are no defaults.",
         call.= FALSE)
  }
  
  if (!is.character(vars)) {
    stop("`vars` must be a vector of character.",
         call. = FALSE)
  }
  
  # if model_no not entered, then use last model as default
  if (is.na(model_no)) model_no = nano$n_model
  
  if (!all(vars %in% names(nano$data[model_no]))) {
    stop("`vars` must variables in the dataset.",
         call. = FALSE)
  }
  
  if (!is.integer(as.integer(n_var))) {
    stop("`n_var` must be numeric.", 
         call. = FALSE)
  }
  
  if (n_var <= 0) {
    stop("`n_var` must be greater than 0", 
         call. = FALSE)
  }
  
  # if varimp has not already been calculated for the model:
  if (is.na(nano$varimp[model_no])) {
    if (length(model_no) == 1) {
      # create pdp for single model
      nano$pdp[[model_no]] <- nano::nano_single_pdp(model = nano$model[[model_no]],
                                                    data = nano$data[[model_no]],
                                                    vars = vars)
    } else {
      # create pdp for multiple models
    }
  }
}  