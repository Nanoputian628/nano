#' @title Building H2O Grids  
#' @description Creates wide range of machine learning models and perform grid search using 
#' `H2O`'s \code{h2o.grid} function implemented with `nano` objects. 
#' @param nano nano object to store model in. If not specified, a new nano object will be 
#' created the results will be stored in the new nano object. 
#' @param response a character. Target variable for model.
#' @param algo a character. Algorithm of model to be built.
#' @param data a data.frame containing data to train model. May also contain testing and 
#' holdout data, in which case, the `train_test` must be specified.
#' @param test a data.frame containing testing dataset. If this is provided, the `train_test`,
#' `fold_column` and `nfolds` arguments cannot be used.
#' @param train_test a character. Variable in `data` which contains split for training, 
#' testing and holdout datasets (optional). Can only have the values: "training", "test", 
#' "holdout".
#' @param grid_id a character. Unique id for created grid.
#' @param ignore_vars vector of characters. Variables in the dataset which should not be used
#' for modelling. Note, if any of `train_test`, `weight_column` or `fold_column` arguments
#' are specified, those variables will be automatically included in `ignore_vars`.
#' @param weight_column a character. Column name in `data` containing weights if used.
#' @param fold_column a character. Column name in `data` containing fold assignments if used. 
#' If this is provided, the `test` and `nfolds` arguments cannot be used. The `train_test`
#' argument can be used, however it cannot contain the values "test".
#' @param nfolds a numeric. Number of folds used in cross-validation. If this is provided, the
#' `test` and `nfolds` arguments cannot be used. The `train_test` argument can be used, 
#' however it cannot contain the values "test".  
#' @param thresh a numeric. Cutoff of number of unique values in response variable to 
#' determine whether performing classification or regression. Default value is 10.
#' @param hyper_params a list. Contains model hyper-parameters for hyper-parameter tuning. The
#' possible hyper-parameters that can be used depends on the algorithm. Look at the `H2O` 
#' functions for the specific algorithm to see full details on the available hyper-parameters:
#' h2o.gbm, h2o.glm, h2o.kmeans, h2o.deepLearning.  
#' @param strategy a character. Specify whether to perform a Cartesian or random grid search. 
#' Only required when more than 1 combination of hyper-parameters are entered.
#' @param max_models a numeric. Maximum number of models to be built.
#' @param max_runtime_secs a numeric. Maximum amount of time (sec) spent building models.
#' @param stopping_metric a character. Metric used to determine whether to terminate fitting
#' process.
#' @param stopping_tolerance a numeric. Minimum threshold for the `stopping_metric` to 
#' improve by to continue the fitting process. 
#' @param stopping_rounds a numeric. Number of rounds in which if the `stopping_metric` has 
#' not at least improved by the `stopping_tolerance`, then terminate fitting process. 
#' @param plots a logical. Whether to produce plots.
#' @param alarm a logical. Whether to beep when function has finished running.
#' @param quiet a logical. Whether to print messages to the console.
#' @param seed a numeric.
#' @return nano object with new entry filled with grid produced. 
#' @details This function used `H2O`'s \code{h2o.grid} function to easily and quickly build
#' difference machine learning models and perform grid search for hyper-parameter tuning. To
#' perform hyper-parameter tuning, input the desired hyper-parameters in the `hyper_params`
#' argument and set the range of values to build the models on. For more details, please see 
#' the documentation for \code{h2o.grid}.
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
#'  # prepare data for modelling
#'  data_all <- nano::data_prep(data          = property_prices, 
#'                              response      = "sale_price",
#'                              split_or_fold = 0.7,
#'                              holdout_ratio = 0.1)
#'  data <- data_all$data
#'  
#'  # create models and perform hyper-parameter tuning on ntrees
#'  nano <- nano_grid(data               = data, 
#'                    response           = "sale_price", 
#'                    algo               = "drf", # random forest
#'                    train_test         = "data_id",
#'                    ignore_vars        = "data_id",
#'                    hyper_params       = list(ntrees = 1:5),
#'                    strategy           = "RandomDiscrete", # random grid search
#'                    stopping_metric    = "mse",
#'                    stopping_tolerance = 1e-3,
#'                    stopping_rounds    = 5)
#'  
#'  }
#' }
#' @rdname nano_grid
#' @export 




nano_grid <- function (nano = nano::create_nano(), response, algo, data, test, 
                       train_test = NA, grid_id = paste0("grid_", nano$n_model + 1),
                       ignore_vars = c(), weight_column = NULL, fold_column = NULL, 
                       nfolds = NA, thresh = 10, hyper_params = NULL, 
                       strategy = "RandomDiscrete", max_models = 10, 
                       max_runtime_secs = 60 * 10, stopping_metric = NULL,
                       stopping_tolerance = NULL, stopping_rounds = NULL, plots = TRUE, 
                       alarm = TRUE, quiet = FALSE, save = FALSE, subdir = NA,
                       project = "ML Project", seed = 628, ...) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!missing(test) & !is.na(train_test)) {
    stop("Cannot specify `train_test` column and also provide a `test` dataset. Please choose only one of these options.",
         call. = FALSE)
  }
  
  if (!missing(fold_column) & !is.na(nfolds)) {
    stop("Cannot specify `fold_column` and also set the `nfolds` parameter. Please choose only one of these options.", 
         call. = FALSE)
  }
  
  if ((!missing(test) | "test" %in% data[[train_test]]) & (!missing(fold_column) | !is.na(nfolds))) {
    stop("Cannot specify both testing dataset and cross-validation methods. Please choose only one of these methods.",
         call. = FALSE)
  }
  
  if (!is.na(train_test) & !all(c("train", "test") %in% data[[train_test]])) {
    stop("`train_test` column must contain the values `train` and `test`.",
         call. = FALSE)
  }
  
  
  nano:::tic(id = paste0("nano_", grid_id))
  on.exit(nano:::toc(id = paste0("nano_", grid_id), msg = "Process duration:", 
                     quiet = quiet))
  if (!quiet) message(paste(Sys.time(), "| Started process..."))
  
  
  ## create training and testing datasets
  # initialise testing dataset
  if (!missing(test)) test <- NA
  
  # split data in training and testing
  train <- data.table::as.data.table(data)
  if (!is.na(train_test)) {
    train <- data[get(train_test) == "train"]
    test  <- data[get(train_test) == "test"]
  }
  
  # determine variables to ignore in training models
  ignore_vars <- c(ignore_vars, 
                   if (!missing(train_test))    train_test,
                   if (!missing(weight_column)) weight_column,
                   if (!missing(fold_column))   fold_column)
  
  # create list of search conditions
  search_criteria = list(strategy           = strategy,
                         max_models         = max_models,
                         max_runtime_secs   = max_runtime_secs,
                         stopping_metric    = stopping_metric,
                         stopping_tolerance = stopping_tolerance,
                         stopping_rounds    = stopping_rounds)
  # remove null values
  search_criteria <- search_criteria[!sapply(search_criteria, is.null)]
  
  # determine type of model
  res_levels <- unique(train[[which(colnames(train) == response)]])
  model_type <- ifelse(length(res_levels) <= thresh, "Classification", "Regression")
  if (!quiet) message("MODEL TYPE: ", model_type)
  

  if (!quiet) 
    message(sprintf(">>> Iterating until %s models or %s seconds...", 
                    max_models, max_time))
  
  
  # fit models
  params <- list(...)
  grid <- do.call(h2o::h2o.grid, c(list(x = setdiff(names(train), c(response, ignore_vars)),
                                        y                = response,
                                        algorithm        = algo,
                                        grid_id          = grid_id,
                                        search_criteria  = search_criteria,
                                        training_frame   = nano:::quiet(as.h2o(train)),
                                        seed             = seed),
                                   list(validation_frame = nano:::quiet(h2o::as.h2o(test)))[data.table::is.data.table(test)],
                                   list(weights_column   = weight_column)[!is.null(weight_column)],
                                   list(fold_column      = fold_column)[!is.null(fold_column)],
                                   list(nfolds           = nfolds)[!is.na(nfolds)],
                                   list(hyper_params     = hyper_params)[!is.null(hyper_params)]))
  
  

  # sort the grid models by metric and select best model
  metric <- if (is.null(stopping_metric)) "mse" else stopping_metric
  grid <- h2o.getGrid(grid_id, sort_by = metric, decreasing = FALSE)
  
  # print leaderboard
  if (nrow(grid@summary_table) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 or increase max_time")
  } else {
      if (!quiet) {
        message(paste("- EUREKA: Succesfully generated", 
                      nrow(grid@summary_table), "models"))
        if (!quiet) print(head(grid@summary_table, 3))
      }
  }
  
  # add models to nano object
  nano$n_model               <- nano$n_model + 1
  nano$grid[[nano$n_model]]  <- nano:::create_Grid(grid)
  nano$model[[nano$n_model]] <- h2o::h2o.getModel(grid@model_ids[1])
  nano$data[[nano$n_model]]  <- data.table::as.data.table(data)
  nano$meta[[nano$n_model]]  <- nano:::model_meta(nano$model[[nano$n_model]], 
                                                  h2o::as.h2o(train))
  if (!is.null(hyper_params)) {
    nano$grid[[paste0("grid_", nano$n_model)]]@meta$tune_hyper_params <- hyper_params 
  } 
  
  # rename elements in nano object
  names(nano$grid)[nano$n_model]  <- paste0("grid_" , nano$n_model)
  names(nano$model)[nano$n_model] <- paste0("model_", nano$n_model)
  names(nano$data)[nano$n_model]  <- paste0("data_" , nano$n_model)
  names(nano$meta)[nano$n_model]  <- paste0("meta_" , nano$n_model)
  
  return(nano)
}  
  