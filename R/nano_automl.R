
function (nano, train, test, response, ignore = c(), train_test = NA, 
          weight_column = NULL, fold_column = NULL, target = "auto", seed = 628, 
          max_models = 3, max_time = 10 * 60, thresh = 10,
          exclude_algos = c("StackedEnsemble", "DeepLearning"), 
          include_algos = NULL, plots = TRUE, alarm = TRUE, quiet = FALSE, 
          save = FALSE, subdir = NA, project = "ML Project", ...) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  tic(id = "nano_automl")
  on.exit(toc(id = "nano_automl", msg = "Process duration:", 
              quiet = quiet))
  if (!quiet) message(paste(Sys.time(), "| Started process..."))
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
  aml <- nano:::quiet(h2o::h2o.automl(x = setdiff(names(train), c(response, ignore)), 
                                      y                = response, 
                                      training_frame   = nano:::quiet(as.h2o(train)),
                                      weights_column   = weight_column, 
                                      fold_column      = fold_column,
                                      max_runtime_secs = max_time, 
                                      max_models       = max_models, 
                                      exclude_algos    = exclude_algos, 
                                      include_algos    = include_algos, 
                                      nfolds           = nfolds, 
                                      seed             = seed))
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

  results <- h2o_results(aml, test = train, train = train, y = response, which = 1, model_type = model_type,
                         ignore = ignore, seed = seed, quiet = quiet)
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
  return(results)
}