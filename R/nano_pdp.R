#' @title Create PDP 
#' @description Creates partial dependency plots (PDPs) from h2o models stored i nano
#' objects.
#' @param model_no the positions of each model in the list of models in the nano object for which
#' the PDP should be calculated. If not entered, the last model is taken by default.
#' @param vars a character vector of variables to create PDPs off. 
#' @param row_index a numeric vector of dataset rows numbers to be used to calculate PDPs. To
#' use entire dataset, set to -1.
#' @param plot a logical specifying whether the variable importance should be plotted.
#' @param subtitle subtitle for the plot.
#' @param save a logical specifying whether the plot should be saved into working directory.
#' @param subdir sub directory in which the plot should be saved.
#' @param file_name file name of the saved plot.   
#' @return nano object with variable importance of specified models calculated. Also returns a 
#' plot if \code{plot = TRUE}.
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
#'  
#'  obj <- create_nano(grid = list(grid_1, grid_2),
#'                     data = list(property_prices), # since underlying dataset is the same 
#'                     ) # since model is not entered, will take best model from grids
#'  
#'  # calculate PDP
#'  obj <- nano_pdp(nano = obj, model_no = 1:2, vars <- c("lot_size", "income"))
#'  
#'  }
#' }
#' @rdname nano_pdp
#' @export 

nano_pdp <- function (nano, model_no = NA, vars, row_index = -1, plot = TRUE, subtitle = NA, 
                      save = FALSE, subdir = NA, file_name) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!all(is.na(model_no))) {
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
  
  if (missing(vars)) {
    stop("`vars` must be entered, there are no defaults.",
         call.= FALSE)
  }
  
  if (!is.character(vars)) {
    stop("`vars` must be a vector of character.",
         call. = FALSE)
  }
  
  # if model_no not entered, then use last model as default
  if (all(is.na(model_no))) model_no = nano$n_model
  
  for (i in model_no) {
    if (!all(vars %in% nano$model[[i]]@parameters$x)) {
      stop("`vars` must be predictors in each of the specified models.",
           call. = FALSE)
    }
  }
  
  if (plot & length(model_no) > 1) {
    for (i in 1:(length(model_no) - 1)) {
      if (nano$model[[model_no[i]]]@parameters$y != nano$model[[model_no[i + 1]]]@parameters$y) {
        warning("the response variable of each of the specified models are not the same hence plots will not be produced.", 
             call. = FALSE)
      }
    }
  }
  
  
  # check which models do not already have PDP calculated
  models    <- list()
  vars_inc  <- list()
  data      <- list()
  model_inc <- c()
  j <- 1
  for (i in model_no) {
    if (all(is.na(nano$pdp[[i]]))) {
      new_vars <- vars
    } else {
      new_vars <- vars[!vars %in% nano$pdp[[i]]$var]
    }
    if (length(new_vars) > 0) {
      models[[j]]   <- nano$model[[i]]
      vars_inc[[j]] <- new_vars
      data[[j]]     <- nano$data[[i]]
      model_inc     <- c(model_inc, i)
      j <- j + 1
    }
  }
  
  if (length(models) > 0) {
    ## need to calculate PDPs for required models and variables
    # variables which need to be calculated for all models
    vars_multi <- Reduce(intersect, vars_inc)
    # remaining variables for each model
    vars_single <- lapply(vars_inc, function(x) setdiff(x, vars_multi))
    
    # calculate pdps for variables common across all models
    if (length(vars_multi) > 0) {
      pdp_multi <- nano::nano_multi_pdp(models, data, vars_multi, row_index)
      # add calculated pdps to nano object
      for (i in 1:length(models)) {
        model_no <- model_inc[i]
        if (all(is.na(nano$pdp[[model_no]]))) {
          nano$pdp[[model_no]]      <- pdp_multi[[i]]
          names(nano$pdp)[model_no] <- paste0("pdp_", model_no)
        } else {
          nano$pdp[[model_no]] <- rbind(nano$pdp[[model_no]], pdp_multi[[i]])
        }
      }
    }
    # for each model, calculate pdps for remaining variables
    if (sum(sapply(vars_single, length)) > 0) {
      index            <- lapply(vars_single, length) > 0
      models_single    <- models[index]
      vars_single      <- vars_single[index]
      data_single      <- data[index]
      model_inc_single <- model_inc[index]
      for (i in 1:length(models_single)) {
        pdp_single <- nano::nano_single_pdp(model     = models_single[[i]], 
                                            data      = data_single[[i]], 
                                            vars      = vars_single[[i]],
                                            row_index = row_index)
        model_no <- model_inc_single[i]
        # add newly calculated pdps to nano object
        if (all(is.na(nano$pdp[[model_no]]))) {
          nano$pdp[[model_no]]      <- pdp_single
          names(nano$pdp)[model_no] <- paste0("pdp_", model_no)
        } else {
          nano$pdp[[model_no]] <- rbind(nano$pdp[[model_no]], pdp_single)
        }
      }
    }
  }
  return(nano)
  # create plots
}  