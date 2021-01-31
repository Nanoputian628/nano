#' @title Building Automated H2O Models  
#' @description Creates robust, fast models using `H2O`'s \code{h2o.automl} function 
#' implemented with `nano` objects. 
#' @param nano nano object to store model in. If not specified, a new nano object will be 
#' created the results will be stored in the new nano object. 
#' @param response a character. Target variable for model.
#' @param data a data.frame containing data to train model. May also contain testing and 
#' holdout data, in which case, the `train_test` must be specified.
#' @param test a data.frame containing testing dataset. If this is provided, the `train_test`,
#' `fold_column` and `nfolds` arguments cannot be used.
#' @param train_test a character. Variable in `data` which contains split for training, 
#' testing and holdout datasets (optional). Can only have the values: "training", "test", 
#' "holdout".
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
#' @param max_models a numeric. Maximum number of models to be built.
#' @param max_time a numeric. Maximum amount of time spent building models.
#' @param thresh a numeric. Cutoff of number of unique values in response variable to 
#' determine whether performing classification or regression. Default value is 10.
#' @param exclude_algos a vector of characters. Algorithms which should be excluding from
#' training process.
#' @param include_algos a vector of characters. Algorithms to be included in training process.
#' Set to \code{NULL} to ignore. If `exclude_algos` and `include_algos` are both provided, 
#' only `include_algos` will be used. 
#' @param plots a logical. Whether to produce plots.
#' @param alarm a logical. Whether to beep when function has finished running.
#' @param quiet a logical. Whether to print messages to the console.
#' @param seed a numeric.
#' @return nano object with new entry filled with models produced. 
#' @details This function used `H2O`'s \code{h2o.automl} function to easily and quickly build
#' several different machine learning models. For more details, please see the documentation 
#' for \code{h2o.automl}.
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
#'  # create models
#'  nano <- nano_automl(data         = data, 
#'                      response     = "sale_price", 
#'                      train_test   = "data_id",
#'                      ignore_vars = "data_id")
#'  
#'  }
#' }
#' @rdname nano_automl
#' @export 

nano_automl <- function (nano = nano::create_nano(), response, data, test, train_test = NA, ignore_vars = c(), weight_column = NULL, fold_column = NULL, nfolds = NA,  
          max_models = 3, max_time = 10 * 60, thresh = 10,
          exclude_algos = c("StackedEnsemble", "DeepLearning"), 
          include_algos = NULL, plots = TRUE, alarm = TRUE, quiet = FALSE, 
          save = FALSE, subdir = NA, project = "ML Project", seed = 628, ...) {
  
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
  
  
  nano:::tic(id = "nano_automl")
  on.exit(nano:::toc(id = "nano_automl", msg = "Process duration:", 
                     quiet = quiet))
  if (!quiet) message(paste(Sys.time(), "| Started process..."))

  
  # increase nano space if required
  nano <- nano:::nano_increase(nano)
  
  ## create training and testing datasets
  # initialise testing dataset
  if (missing(test)) test <- NA
  
  # split data in training and testing
  train <- data.table::as.data.table(data)
  if (!is.na(train_test)) {
    train <- data[get(train_test) == "train"]
    test  <- data[get(train_test) == "test"]
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
  
  # determine type of model
  res_levels <- unique(train[[which(colnames(train) == response)]])
  model_type <- ifelse(length(res_levels) <= thresh, "Classification", "Regression")
  if (!quiet) message("MODEL TYPE: ", model_type)

  # print algorithms used
  if (length(exclude_algos) > 0 & length(include_algos) == 0 & !quiet) 
    message(paste("ALGORITHMS excluded:", nano:::print_vector(exclude_algos)))
  if (length(include_algos) > 0 & !quiet) {
    message(paste("ALGORITHMS included:", nano:::print_vector(include_algos)))
    exclude_algos <- NULL
  }          
  
  if (!quiet) 
    message(sprintf(">>> Iterating until %s models or %s seconds...", 
                    max_models, max_time))
  
  # fit models
  params <- list(...)
  aml <- do.call(h2o:::h2o.automl, c(list(x = setdiff(names(train), c(response, ignore_vars)), 
                                          y                = response, 
                                          training_frame   = nano:::quiet(as.h2o(train)),
                                          max_runtime_secs = max_time, 
                                          max_models       = max_models, 
                                          exclude_algos    = exclude_algos, 
                                          include_algos    = include_algos, 
                                          seed             = seed, 
                                          params),
                                     list(validation_frame = nano:::quiet(h2o::as.h2o(test)))[data.table::is.data.table(test)],
                                     list(weights_column   = weight_column)[!is.null(weight_column)],
                                     list(fold_column      = fold_column)[!is.null(fold_column)]))


  # print leaderboard
  if (nrow(aml@leaderboard) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 or increase max_time")
  } else {
    if (!is.nan(aml@leaderboard[1, 2])) 
      if (!quiet) {
        message(paste("- EUREKA: Succesfully generated", 
                      nrow(aml@leaderboard), "models"))
        if (!quiet) 
          print(head(aml@leaderboard, 3))
      }
  }
  
  
  # recreate dataset to input into nano object
  if (missing(train_test)) {
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
      message("Renaming ", train_test, "to data_id.")
    }
  }
  
  # rename fold_column to fold for consistency
  if (!missing(fold_column)) {
    if (fold_column != "fold") {
      names(data)[names(data) == fold_column] <- "fold" 
      message("Renaming ", fold_column, "to fold.")
    }
  }
  
  
  # add models to nano object
  nano$n_model                <- nano$n_model + 1
  nano$grid[[nano$n_model]]   <- nano:::create_Grid(aml)
  nano$model[[nano$n_model]]  <- h2o::h2o.getModel(as.data.frame(aml@leaderboard)$model_id[1])
  nano$metric[[nano$n_model]] <- nano:::model_metrics(nano$model[[nano$n_model]], data)
  nano$data[[nano$n_model]]   <- data
  nano$meta[[nano$n_model]]   <- nano:::model_meta(nano$model[[nano$n_model]], 
                                                  h2o::as.h2o(train))
  
  # rename elements in nano object
  names(nano$grid)[nano$n_model]   <- paste0("grid_"  , nano$n_model)
  names(nano$model)[nano$n_model]  <- paste0("model_" , nano$n_model)
  names(nano$metric)[nano$n_model] <- paste0("metric_", nano$n_model)
  names(nano$data)[nano$n_model]   <- paste0("data_"  , nano$n_model)
  names(nano$meta)[nano$n_model]   <- paste0("meta_"  , nano$n_model)

  # results <- h2o_results(aml, test = train, train = train, y = response, which = 1, model_type = model_type,
  #                        ignore = ignore, seed = seed, quiet = quiet)
  # if (save) {
  #   export_results(results, subdir = subdir, thresh = thresh)
  #   if (!quiet) 
  #     message("- EXPORT: Results and model files exported succesfully!")
  # }
  # if (!quiet) 
  #   print(results)
  # if (alarm) {
  #   try_require("beepr", stop = FALSE)
  #   try(beep())
  # }
  # attr(results, "type") <- "h2o_automl"
  return(nano)
}
