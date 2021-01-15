#' @title Create ICE for a Single Model
#' @description Creates ICEs for variable(s) for a single h2o model.
#' @param model a h2o model.
#' @param data a dataset. Dataset used to create `model`.
#' @param vars vector of characters. Vector containing variables in `data` to create ICEs 
#' @param max_levels a numeric. Maximum number of unique levels to calculate ICE for each 
#' variable.  
#' @param quantiles a numeric vector of quantiles (numbers from 0 to 1) for each ICE to be
#' calculated for.
#' @param targets a character vector. Only applicable for classification models. Subset of
#' levels of response variables which ICE should be calculated for. 
#' @return a data.tables containing pdps for each variable combined together.
#' @details Creates a ICE for each variable specified in the `vars` argument given a h2o
#' model. 
#' 
#' For creating ICE, it is recommended to instead use the \code{nano_ice} function 
#' which is a wrapper for a series of functions which creates pdps. It is able to create
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
#'  # calculate ICE
#'  nano_single_ice(model     = model, 
#'                  data      = property_prices, 
#'                  vars      = c("lot_size"), 
#'                  quantiles = seq(0, 1, 0.1))
#'  
#'  }
#' }
#' @rdname nano_single_ice
#' @export


nano_single_ice <- function (model, data, vars, max_levels = 30, quantiles = seq(0, 1, 0.1),
                             targets = NULL) {
  
  if (!is.numeric(quantiles)) {
    stop("`quantiles` must be numeric.",
         call. = FALSE)
  }
  
  if (quantiles[1] != 0 | quantiles[length(quantiles)] != 1) {
    stop("`quantiles` must begin with 0 and end with 1.",
         call. = FALSE)
  }
  
  # convert to h20 data.frame
  data <- h2o::as.h2o(data)
  models_info <- h2o:::.process_models_or_automl(model, 
                                                 data, 
                                                 require_single_model = TRUE)
  # check if number of levels in each of the vars exceeds the max_level. If so, ommit
  # the least common levels.
  nbins <- 20
  for (var in vars) {
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
  
  # if targets is missing and classification, set targets to the be levels of response
  if (is.null(targets) & models_info$is_multinomial_classification) {
    targets <- h2o::h2o.levels(data[[models_info$y]])
  }
  
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
          ice <- data.table::data.table(ice[[1]])
          ice[, "target" := "Partial Depencence"]
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
      
      ices[["text"]] <- paste0("Feature Value: ", 
                               ices[[var]], "\n", "Mean Response: ", 
                               ices[["mean_response"]], "\n", "Target: ", 
                               ices[["target"]])
      ices[["var"]] <- var
      # use for identification in nano_ice function to easily identify if the 
      # ICE has already been calculated
      ices[["key"]] <- paste0(var, " - ", ices[["name"]])
      # change column name for variable name to "var_band". This is to ensure all
      # datasets have the same column names which allows them to be appended 
      names(ices)[1] <- "var_band"
      ice_all[[var]] <- ices
    })
  }
  
  ice_all[[1]] <- NULL
  # combined ICE for each variable into a single data.table
  out <- do.call(rbind, lapply(ice_all, data.table::as.data.table))
  return(out)
}

