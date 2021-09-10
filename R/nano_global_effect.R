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
#' @rdname nano_global_effect
#' @export

nano_global_effect <- function (nano, model_no = nano$n_model, vars, max_levels = 30, 
                                method = "ale", quantiles = seq(0, 1, 0.1), plot = FALSE, 
                                subtitle = NA, save = FALSE, subdir = NA, file_name) {
  
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
    } else if (nrow(nano$pdp[[i]][type == method])) {
      new_vars <- vars
    } else {
      vars_adj <- vars
      for (i in 1:length(vars)) {
        var <- vars[[i]]
        if (length(var) == 2) {
          var <- var[order(var)]
          vars_adj[[i]] <- paste0(var[1], ":", var[2])
        } 
      }
      vars_adj <- unlist(vars_adj)
      new_vars <- vars[which(!vars_adj %in% nano$pdp[[i]][type == method]$var)]
    }
    if (length(new_vars) > 0) {
      models[[j]]   <- nano$model[[i]]
      vars_inc[[j]] <- new_vars
      data[[j]]     <- nano$data[[i]]
      model_inc     <- c(model_inc, i)
      j <- j + 1
    }
    
    # write message for pdps which have already been created
    vars_prev_inc <- setdiff(vars, new_vars)
    if (length(vars_prev_inc) > 0) {
      message("For model_", model_no[i], ", ", toupper(method), "s have already been created for the following variables:")
      for (var in vars_prev_inc) {
        message(" ", var)
      } 
    }
  }
  
  
  if (length(models) > 0) {
    ## need to calculate global effects for required models and variables
    # variables which need to be calculated for all models
    vars_multi <- Reduce(intersect, vars_inc)
    # remaining variables for each model
    vars_single <- lapply(vars_inc, function(x) setdiff(x, vars_multi))
    
    # calculate globale effects for variables common across all models
    if (length(vars_multi) > 0) {
      pdp_multi <- nano::multi_global_effect(models, data, vars_multi, method = method,
                                             quantiles = quantiles)
      # add calculated global effects to nano object
      for (i in 1:length(models)) {
        model_no <- model_inc[i]
        if (method == "ale") met <- "pdp" else met <- method
        if (all(is.na(nano[[met]][[model_no]]))) {
          nano[[met]][[model_no]]      <- pdp_multi[[i]]
          names(nano[[met]])[model_no] <- paste0(met, "_", model_no)
        } else {
          nano[[met]][[model_no]] <- rbind(nano[[met]][[model_no]], pdp_multi[[i]])
        }
      }
    }
    
    # for each model, calculate global effects for remaining variables
    if (sum(sapply(vars_single, length)) > 0) {
      index            <- lapply(vars_single, length) > 0
      models_single    <- models[index]
      vars_single      <- vars_single[index]
      data_single      <- data[index]
      model_inc_single <- model_inc[index]
      for (i in 1:length(models_single)) {
        pdp_single <- nano::single_global_effect(model     = models_single[[i]], 
                                                 data      = data_single[[i]], 
                                                 vars      = vars_single[[i]],
                                                 row_index = row_index)
        model_no <- model_inc_single[i]
        # add newly calculated global effects to nano object
        if (method == "ale") met <- "pdp" else met <- method
        if (all(is.na(nano[[met]][[model_no]]))) {
          nano[[met]][[model_no]]      <- pdp_single
          names(nano[[met]])[model_no] <- paste0(met, "_", model_no)
        } else {
          nano[[met]][[model_no]] <- rbind(nano[[met]][[model_no]], pdp_single)
        }
      }
    }
  }
  return(nano)
  # create plots
}  