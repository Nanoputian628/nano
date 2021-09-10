#' @title Global Interpretation for a Single Model
#' @description Creates ALEs, PDPs and ICEs for variable(s) for a single h2o model.
#' @param model a h2o model.
#' @param data a dataset. Dataset used to create `model`.
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
#' @return a data.tables containing values for each variable combined together.
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
#'  single_global_effect(model, property_prices, c("lot_size"))
#'  
#'  }
#' }
#' @rdname single_global_effect
#' @export


single_global_effect <- function (model, data, vars, max_levels = 30, method = "ale",
                                  quantiles = seq(0, 1, 0.1)) {
  
  if (!grepl("H2O", class(model)) | !grepl("Model", class(model))) {
    stop("`model` must be a h2o model.", 
         call. = FALSE)
  }
  
  if (!("data.frame" %in% class(data))) {
    stop("`data` must be a dataset.",
         call. = FALSE)
  }
  
  if (!is.list(vars)) {
    stop("`vars` must be a list.",
         call. = FALSE)
  }
  
  if (max(sapply(vars, length)) > 2) {
    stop("Each element in `vars` must have length of 1 or 2.",
         call. = FALSE)
  }
  
  if (!all(unlist(vars) %in% model@parameters$x)) {
    stop("`vars` must be column names in `data`.", 
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
  
  # convert to h20 data.frame
  data <- nano:::quiet(h2o::as.h2o(data))
  models_info <- h2o:::.process_models_or_automl(model, 
                                                 data, 
                                                 require_single_model = TRUE)
  # check if number of levels in each of the vars exceeds the max_level. If so, ommit
  # the least common levels.
  nbins <- 20
  for (var in unlist(vars)) {
    if (h2o::h2o.nlevels(data[[var]]) > max_levels) {
      factor_frequencies <- h2o:::.get_feature_count(data[[var]])
      factors_to_merge   <- tail(names(factor_frequencies), n = -max_levels)
      data[[var]]        <- ifelse(data[[var]] %in% factors_to_merge, 
                                   NA_character_, 
                                   data[[var]])
      message(length(factor_frequencies) - max_levels, 
              " least common factor levels were omitted from \"", 
              var, "\" feature.")
    }
    # calculate number of bins
    if (is.factor(data[[var]])) nbins <- max(20, h2o::h2o.nlevels(data[[var]]) + 1)
  }
  
  # copy new data
  data  <- data.table::as.data.table(data)
  response <- model@parameters$y
  
  # calculate global effects
  if (method %in% c("pdp", "ale")) {
    # create custom predict function which is compatible with IML package
    pred <- function(model, data)  {
      results <- as.data.frame(nano:::quiet(
        h2o::h2o.predict(model, data)))
      return(results[[3L]])
    }
    # create predictor object
    predictor <- iml::Predictor$new(model       = model, 
                                    data        = data[, names(data) != response, with = FALSE],
                                    y           = data[[response]], 
                                    predict.fun = pred)
    # adjust nbins since number of actual bins = grid.size + 1
    if (method == "ale") nbins <- nbins - 1
    
    # for each variable, calculate pdp and store in list
    pdp_all <- list(NA)
    for (var in vars) {
      if (length(var) == 2 & method == "pdp") {
        message("Warning: Calculating partial dependency for two-way variable interaction may take a long time. To reduce run time, consider using the ale method.")
      }
      # create grid
      if (length(var) == 1) grid_size <- nbins else grid_size <- c(nbins, nbins)
      # calculate feature effect
      out <- nano:::quiet(iml::FeatureEffect$new(predictor, var, method = method,
                                                 grid.size = grid_size))
      pdps <- data.table::as.data.table(out$results)
      # rename column names
      if (".class" %in% names(pdps)) {
        names(pdps)[names(pdps == ".class")] <- "target"
      } else {
        pdps[, target := response] 
      }
      if (length(var) == 1) { # for single variable global effect
        setnames(pdps, c(".type", ".value", var), c("type", "value", "var_band"))
        # store var in data.table
        pdps[["var"]] <- var        
      } else { # for two variable global effect
        if (method == "ale") {
          setnames(pdps, c(".type", ".ale"), c("type", "value"))
          pdps[, c(".right", ".left", ".bottom", ".top") := NULL]
        } else {
          setnames(pdps, c(".type", ".value"), c("type", "value"))
        }
        # sort var alphabetically 
        var <- var[order(var)]
        # combine var bands together
        band <- paste0(pdps[[var[1]]], ":", pdps[[var[2]]])
        pdps[, var_band := band]
        pdps[, c(var[1], var[2]) := NULL]
        # concatenate to form var
        pdps[["var"]] <- paste(var, collapse = ":")
      }
      pdp_all[[paste(var, collapse = ":")]] <- pdps  
    }
    pdp_all[[1]] <- NULL
    out <- do.call(rbind, lapply(pdp_all, data.table::as.data.table))
  } else {
    data <- nano:::quiet(h2o::as.h2o(data))
    # set targets to the be levels of response
    targets <- h2o::h2o.levels(data[[models_info$y]])
    
    # if regression model, calculate index of quantiles
    # if classification model, calculate index for each unique level
    # note: it is assumed that all specified models are of the same type
    res <- property_prices[[model@parameters$y]]
    if (!models_info$is_classification) {
      quant <- quantile(res, quantiles)
      quant <- as.vector(sapply(quant, function(x) which.min(abs(res - x))))
    } else {
      i <- 1
      quant <- rep(NA, min(10, length(levels(res))))
      for (level in levels(res)) {
        if (i > 10) break
        quant[i] <- which.max(level == res)
        i <- i + 1
      }
    }
    
    # for each variable, calculate ice and store in list
    ice_all <- list(NA)
    for (var in vars) {
      h2o:::with_no_h2o_progress({
        # for each quantile, calculate ice and append to one data.table
        i <- 0
        ices <- data.table()
        for (idx in quant) {
          # calculate ice for particular quantile
          ice <- h2o::h2o.partialPlot(object    = models_info$get_model(models_info$model_ids[[1]]), 
                                      data      = data, 
                                      cols      = var, 
                                      plot      = FALSE, 
                                      targets   = targets, 
                                      nbins     = nbins, 
                                      row_index = idx)
          
          # convert to data.table and combine all datasets together
          if (!is.null(targets)) {
            for (j in seq_along(ice)) {
              data.table::setDT(ice[[j]]) 
              ice[[j]][, "target" := targets[[j]]]
            }
            ice <- do.call(rbind, lapply(ice, data.table::as.data.table))
          } else {
            ice <- data.table::data.table(ice)
            ice[, "target" := response]
          }
          
          # create quantile column
          if (!models_info$is_classification) {
            perc <- round(i * 100 / (length(quant) - 1))
            ice[, name := sprintf("%dth Percentile", perc)]
          } else {
            ice[, name := sprintf("Level: %s", levels(res)[i + 1])]
          }
          i <- i + 1
          ices <- rbind(ices, ice)
        }
        
        ices[, type := "ice"]
        ices[["var"]] <- var
        # use for identification in nano_ice function to easily identify if the 
        # ICE has already been calculated
        ices[["key"]] <- paste0(var, " - ", ices[["name"]])
        # change column name for variable name to "var_band". This is to ensure all
        # datasets have the same column names which allows them to be appended 
        setnames(ices, c(var, "mean_response"), c("var_band", "value"))
        # remove columns not required
        ices[, c("stddev_response", "std_error_mean_response") := NULL]
        ice_all[[var]] <- ices
      })
    }
    
    ice_all[[1]] <- NULL
    # combined ICE for each variable into a single data.table
    out <- do.call(rbind, lapply(ice_all, data.table::as.data.table))
  }
  
  return(out)
}
