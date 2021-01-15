#' @title Create PDP for a Single Model
#' @description Creates PDPs for variable(s) for a single h2o model.
#' @param model a h2o model.
#' @param data a dataset. Dataset used to create `model`.
#' @param vars vector of characters. Vector containing variables in `data` to create pdps. 
#' @param max_levels a numeric. Maximum number of unique levels to calculate pdp for each 
#' variable.  
#' @param row_index a numeric vector of dataset rows numbers to be used to calculate PDPs. To
#' use entire dataset, set to -1.
#' @return a data.tables containing pdps for each variable combined together.
#' @details Creates a pdp for each variable specified in the `vars` argument given a h2o
#' model. 
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
#'  nano_single_pdp(model, property_prices, c("lot_size"))
#'  
#'  }
#' }
#' @rdname nano_single_pdp
#' @export



nano_single_pdp <- function (model, data, vars, max_levels = 30, row_index = -1) {

  if (!grepl("H2O", class(model)) | !grepl("Model", class(model))) {
    stop("`model` must be a h2o model.", 
         call. = FALSE)
  }
  
  if (!("data.frame" %in% class(data))) {
    stop("`data` must be a dataset.",
         call. = FALSE)
  }

  if (!all(vars %in% names(data))) {
    stop("`vars` must be column names in `data`.", 
         call. = FALSE)
  }
  
  if (!is.integer(as.integer(max_levels)) | !max_levels > 0) {
    stop("`max_levels` must be an integer greater than 0.")
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
  
  # margin <- ggplot2::margin(5.5, 5.5, 5.5, 5.5)
  # if (h2o::h2o.isfactor(newdata[[column]])) 
  #   margin <- ggplot2::margin(5.5, 5.5, 5.5, max(5.5, max(nchar(h2o::h2o.levels(newdata[[column]])))))
  
  # different levels in multinomial classification
  targets <- NULL
  if (models_info$is_multinomial_classification) {
    targets <- h2o::h2o.levels(data[[models_info$y]])
  }
  
  # for each variable, calculate pdp and store in list
  pdp_all <- list(NA)
  for (var in vars) {
    h2o:::with_no_h2o_progress({
      pdps <- h2o::h2o.partialPlot(object = models_info$get_model(models_info$model_ids[[1]]), 
                                   data = data, 
                                   cols = var, 
                                   plot = FALSE, 
                                   targets = targets, 
                                   nbins = nbins, 
                                   row_index = row_index)
      if (!is.null(targets)) {
        # convert to data.table and combine all datasets together
        for (idx in seq_along(pdps)) {
          data.table::setDT(pdps[[idx]]) 
          pdps[[idx]][, "target" := targets[[idx]]]
        }
        pdp <- do.call(rbind, lapply(pdps, data.table::as.data.table))
      }
      else {
        pdp <- data.table::data.table(pdps[[1]])
        pdp[, "target" := "Partial Depencence"]
      }
      pdp[["text"]] <- paste0("Feature Value: ", 
                              pdp[[var]], "\n", "Mean Response: ", 
                              pdp[["mean_response"]], "\n", "Target: ", 
                              pdp[["target"]])
      pdp[["var"]] <- var
      # change column name for variable name to "var_band". This is to ensure all
      # datasets have the same column names which allows them to be appended 
      names(pdp)[1] <- "var_band"
      pdp_all[[var]] <- pdp
    })
  }
  pdp_all[[1]] <- NULL
  out <- do.call(rbind, lapply(pdp_all, data.table::as.data.table))
  # out <- pdp_all[[1]] 
  # if (length(pdp_all) > 1) {
  #   for (i in 2:length(pdp_all)) {
  #     out <- rbind(out, pdp_all[[i]])
  #   }
  # }
  return(out)
}

