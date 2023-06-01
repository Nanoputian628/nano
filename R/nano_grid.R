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
#' testing and holdout datasets (optional). Can only have the values: "train", "test", 
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
#' @param monotone_constraints a list. Mapping between variable names in `data` to values
#' +1 or -1. Use +1 to enforce an increasing constraint while use -1 for a decreasing 
#' constraint. Constraints are only valid for numerical columns.
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
#' @param grid_description a character. Optional description of grid. Can be later accessed by
#' \code{nano$grid[[grid_no]]@meta$description}. 
#' @param ... further parameters to pass to \code{h2o.grid} depending on `algo`.
#' @return nano object with new entry filled with grid produced. 
#' @details This function used `H2O`'s \code{h2o.grid} function to easily and quickly build
#' difference machine learning models and perform grid search for hyper-parameter tuning. To
#' perform hyper-parameter tuning, input the desired hyper-parameters in the `hyper_params`
#' argument and set the range of values to build the models on. Importantly, an active H2O 
#' connection is required (i.e. run \code{h2o.init()})) before using this function. 
#' 
#' For more details, please see the documentation for \code{h2o.grid}.
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
                       nfolds = NA, thresh = 10, monotone_constraints = c(), 
                       hyper_params = NULL, strategy = "RandomDiscrete", max_models = 10, 
                       max_runtime_secs = 60 * 10, stopping_metric = NULL,
                       stopping_tolerance = 0, stopping_rounds = 0, plots = TRUE, 
                       alarm = TRUE, quiet = FALSE, save = FALSE, subdir = NA,
                       project = "ML Project", seed = 628, grid_description = "", ...) {
  
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
  
  if (!is.na(train_test) & !all(unique(data[[train_test]]) %in% c("train", "test", "holdout"))) {
    stop("`train_test` column must contain the values `train`, `test` or `holdout`.",
         call. = FALSE)
  }
  
  if (!algo %in% c("gbm", "xgboost") & !is.null(monotone_constraints)) {
    stop("Can only specify `monotone_constraints` for gbm and xgboost algorithmns.",
         call. = FALSE)
  }
  
  if (!all(names(monotone_constraints) %in% names(data))){
    stop("Names of `monotone_constraints` must be variables in `data`.",
         call. = FALSE)
  }
  
  if (!is.character(grid_description)) {
    stop("`grid_description` must be character type.",
         call. = FALSE)
  }
  
  
  nano:::tic(id = paste0("nano_", grid_id))
  on.exit(nano:::toc(id = paste0("nano_", grid_id), msg = "Process duration:", 
                     quiet = quiet))
  if (!quiet) message(paste(Sys.time(), "| Started process..."))
  
  
  # increase nano space if required
  nano <- nano:::nano_increase(nano)
  
  # determine type of model
  res_levels <- unique(data[[which(colnames(data) == response)]])
  model_type <- ifelse(length(res_levels) <= thresh, "Classification", "Regression")
  # for classification model, convert response variable to factor type
  if (model_type == "Classification") data[[response]] <- as.factor(data[[response]])
  if (!quiet) message("MODEL TYPE: ", model_type)
  
  ## create training and testing datasets
  # initialise testing dataset
  if (missing(test)) test <- NA
  
  # split data in training and testing
  train <- data.table::copy(data.table::as.data.table(data))
  if (!is.na(train_test)) {
    train <- data[get(train_test) == "train"]
    test  <- data[get(train_test) == "test"]
    # if "test" not specified in train_test then set test to NA
    if (nrow(test) == 0) test <- NA
  }
  
  # if nfolds specified, assign folds to rows
  if (!is.na(nfolds)) {
    fold <- caret::createFolds(as.vector(train[[response]]), k = nfolds, 
                               list = FALSE)
    train[, fold := as.factor(fold)]
    fold_column <- "fold"
    nfolds <- NA
  }
  
  # determine variables to ignore in training models
  ignore_vars <- c(ignore_vars, 
                   if (!missing(train_test))    train_test,
                   if (!missing(weight_column)) weight_column,
                   if (!missing(fold_column))   fold_column)
  
  # default stopping metric
  if (is.null(stopping_metric)) {
    if (model_type == "Regression") {
      stopping_metric = "deviance"
    } else {
      stopping_metric = "logloss"
    }
  }
  
  # search criteria
  if (strategy == "Cartesian") {
    max_models       = NULL
    max_runtime_secs = NULL
  }
  search_criteria <- list(strategy         = strategy,
                          max_models       = max_models,
                          max_runtime_secs = max_runtime_secs)
  search_criteria <- search_criteria[!sapply(search_criteria, is.null)]
  
  
  if (!quiet) 
    message(sprintf(">>> Iterating until %s models or %s seconds...", 
                    max_models, max_runtime_secs))
  

  
  # fit models
  params <- list(...)
  grid <- do.call(h2o::h2o.grid, c(list(x = setdiff(names(train), c(response, ignore_vars)),
                                        y                    = response,
                                        algorithm            = algo,
                                        grid_id              = grid_id, 
                                        training_frame       = nano:::quiet(as.h2o(train)),
                                        search_criteria      = search_criteria,
                                        stopping_tolerance   = stopping_tolerance,
                                        stopping_rounds      = stopping_rounds,
                                        stopping_metric      = stopping_metric,
                                        seed                 = seed),
                                   list(validation_frame     = nano:::quiet(h2o::as.h2o(test)))[data.table::is.data.table(test)],
                                   list(weights_column       = weight_column)[!is.null(weight_column)],
                                   list(hyper_params         = hyper_params)[!is.null(hyper_params)],
                                   list(fold_column          = fold_column)[!is.null(fold_column)],
                                   list(monotone_constraints = monotone_constraints)[!is.null(monotone_constraints)],
                                   params[length(params) > 0]
                                   ))
  
  

  # sort the grid models by metric and select best model
  grid <- h2o.getGrid(grid_id, decreasing = FALSE)
  
  # print leaderboard
  if (nrow(grid@summary_table) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 or increase max_time")
  } else {
      if (!quiet) {
        message(paste("- EUREKA: Succesfully generated", 
                      nrow(grid@summary_table), "models"))
        if (!quiet) print(head(grid@summary_table, 10))
      }
  }
  
  
  # recreate dataset to input into nano object
  if (missing(train_test) & !"data_id" %in% names(data)) {
    # is train_test column is not specified, create data_id column 
    # and combine datasets together 
    train[, data_id := "train"]
    data <- data.table::copy(train)
    if (data.table::is.data.table(test)) {
      test[, data_id := "test"]
      data <- rbind(data, test)
    }  
  } else {
    if (!"data_id" %in% names(data)) {
      # rename train_test to data_id for consistency
      names(data)[names(data) == train_test] <- "data_id" 
      if (!quiet) {
        message(paste0("Renamed ", train_test, " to data_id."))
      }
    }
  }
  
  # rename fold_column to fold for consistency
  if (!missing(fold_column)) {
    if (fold_column != "fold") {
      names(data)[names(data) == fold_column] <- "fold" 
      if (!quiet) {
        message("Renaming ", fold_column, " to fold.")
      }
    }
  }
  
  
  # add models to nano object
  nano$n_model                <- nano$n_model + 1
  nano$grid[[nano$n_model]]   <- nano:::create_Grid(grid)
  nano$model[[nano$n_model]]  <- h2o::h2o.getModel(grid@model_ids[[1]])
  nano$metric[[nano$n_model]] <- nano:::model_metrics(nano$model[[nano$n_model]], data)
  nano$data[[nano$n_model]]   <- data.table::copy(data)
  nano$meta[[nano$n_model]]   <- nano:::model_meta(nano$model[[nano$n_model]], 
                                                  h2o::as.h2o(train))
  nano$grid[[nano$n_model]]@meta$description <- grid_description
  
  # if (!is.null(hyper_params)) {
  #   nano$grid[[paste0("grid_", nano$n_model)]]@meta$tune_hyper_params <- hyper_params
  # }
  
  # rename elements in nano object
  names(nano$grid)[nano$n_model]   <- paste0("grid_"  , nano$n_model)
  names(nano$model)[nano$n_model]  <- paste0("model_" , nano$n_model)
  names(nano$metric)[nano$n_model] <- paste0("metric_", nano$n_model)
  names(nano$data)[nano$n_model]   <- paste0("data_"  , nano$n_model)
  names(nano$meta)[nano$n_model]   <- paste0("meta_"  , nano$n_model)
  
  return(nano)
}  
  