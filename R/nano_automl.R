function (nano, train, test, response, ignore = c(), train_test = NA, 
          split = 0.7, weight = NULL, target = "auto", balance = FALSE, 
          impute = FALSE, no_outliers = TRUE, unique_train = TRUE, 
          center = FALSE, scale = FALSE, thresh = 10, seed = 0, nfolds = 5, 
          max_models = 3, max_time = 10 * 60, start_clean = FALSE, 
          exclude_algos = c("StackedEnsemble", "DeepLearning"), 
          include_algos = NULL, plots = TRUE, alarm = TRUE, quiet = FALSE, 
          print = TRUE, save = FALSE, subdir = NA, project = "ML Project", 
          ...) {
  
  
  tic(id = "nano_automl")
  on.exit(toc(id = "nano_automl", msg = "Process duration:", 
              quiet = quiet))
  if (!quiet) 
    message(paste(Sys.time(), "| Started process..."))
  # if (nrow(test) == 0) 
  #   test <- train
  
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
  aml <- nano:::quiet(h2o::h2o.automl(x = colnames(df)[!colnames(df) %in% 
                                             c("tag", ignore)], y = "tag", training_frame = quiet(as.h2o(train)), 
                          weights_column = weight, max_runtime_secs = max_time, 
                          max_models = max_models, exclude_algos = exclude_algos, 
                          include_algos = include_algos, nfolds = nfolds, seed = seed, 
                          ...), quiet = quiet)
  if (nrow(aml@leaderboard) == 0) {
    stop("NO MODELS TRAINED. Please set max_models to at least 1 and increase max_time")
  }
  else {
    if (!is.nan(aml@leaderboard[1, 2])) 
      if (!quiet) {
        message(paste("- EUREKA: Succesfully generated", 
                      nrow(aml@leaderboard), "models"))
        if (print) 
          print(head(aml@leaderboard, 3))
      }
  }
  flow <- "http://localhost:54321/flow/index.html"
  if (!quiet & Sys.getenv("HOSTNAME") == "") 
    message("- UI: Check results using H2O Flow's interface: ", 
            flow)
  results <- h2o_results(aml, test, train, y, which = 1, model_type = model_type, 
                         target = target, split = split, plots = plots, project = project, 
                         ignore = ignore, seed = seed, quiet = quiet)
  if (save) {
    export_results(results, subdir = subdir, thresh = thresh)
    if (!quiet) 
      message("- EXPORT: Results and model files exported succesfully!")
  }
  if (!quiet & print) 
    print(results)
  if (alarm & !quiet) {
    try_require("beepr", stop = FALSE)
    try(beep())
  }
  attr(results, "type") <- "h2o_automl"
  return(results)
}