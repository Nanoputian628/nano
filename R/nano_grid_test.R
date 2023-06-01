nano_grid_test <- function (nano = nano::create_nano(), response, algo, data, test, 
                            train_test = NA, grid_id = paste0("grid_test", nano$n_model + 1),
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
  return(grid)
}




