quiet <- function(fun, quiet = TRUE) {
  if (!quiet) 
    return(fun)
  sink(tempfile())
  on.exit(sink())
  invisible(force(fun))
}


set.tictoc <- function (which, id) {
  now <- proc.time()["elapsed"]
  aux <- getOption(which)
  name <- sprintf("%s_%s", which, id)
  if (is.null(aux)) 
    aux <- list()
  aux[[name]] <- now
  return(aux)
}


tic <- function(id = 1, quiet = FALSE) {
  tic <- set.tictoc("tic", id)
  options(tic = tic)
  if (!quiet) 
    message(sprintf("Tic `id = %s` start time: %s", 
                    id, Sys.time()))
  invisible(tic)
}


toc <- function (id = 1, msg = "Elapsed time:", units = TRUE, 
                 signif = 3, quiet = FALSE) {
  if (!sprintf("tic_%s", id) %in% names(getOption("tic"))) 
    stop(sprintf("You need to tic(id = '%s') before you toc(id = '%s')", 
                 id, id))
  toc <- set.tictoc("toc", id)
  options(toc = toc)
  tic <- getOption("tic")[[paste0("tic_", id)]]
  toc <- getOption("toc")[[paste0("toc_", id)]]
  time <- as.numeric(toc - tic)
  if (units) {
    x <- time
    u <- ifelse(x < 60, "s", ifelse(x < 3600, "m", 
                                    ifelse(x < 86400, "h", "d")))
    d <- ifelse(x < 60, 1, ifelse(x < 3600, 60, ifelse(x < 
                                                         86400, 3600, 86400)))
    timer <- paste0(signif(time/d, signif), u)
  }
  else timer <- paste0(signif(time, signif), "s")
  msg <- sprintf("%s %s", msg, timer)
  if (!quiet) 
    message(msg)
  res <- list(tic = tic, toc = toc, time = time, msg = msg)
  invisible(res)
}

wafflecut <- function (x, intervals, buckets = intervals, na.bucket = NA, 
                       unmatched.bucket = NA, out.as.factor = TRUE) {
  l <- length(intervals)
  if (l != length(buckets)) {
    stop("FancyCut requires a 1-1 map from intervals to buckets")
  }
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  out <- rep(NA, length(x))
  intervals_df <- nano:::parse_intervals(intervals)
  for (index in 1:l) {
    b <- buckets[index]
    lower <- intervals_df$left[index]
    upper <- intervals_df$right[index]
    left <- intervals_df$left_strict[index]
    right <- intervals_df$right_strict[index]
    mask <- rep(FALSE, length(x))
    if (left & right) {
      mask <- x >= lower & x <= upper
    }
    if (left & !right) {
      mask <- x >= lower & x < upper
    }
    if (!left & right) {
      mask <- x > lower & x <= upper
    }
    if (!left & !right) {
      mask <- x > lower & x < upper
    }
    out[mask] <- b
  }
  if (sum(is.na(x)) == 0L) {
    na.bucket <- NULL
  }
  else {
    out[is.na(x)] <- na.bucket
  }
  if (sum(is.na(out)) == 0L) {
    unmatched.bucket <- NULL
  }
  else {
    out[is.na(out)] <- unmatched.bucket
  }
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  if (out.as.factor) {
    return(factor(out, levels = levels, exclude = NULL))
  }
  else {
    return(out)
  }
}


# define parse_intervals function. From fancycut package but seems to be missing from the
# package now, so manualy created function here. 
parse_intervals <- function(intervals) {
  rx <- "^\\s*(\\(|\\[)\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*,\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*(\\)|\\])\\s*$"
  lindex <- regexec(rx, intervals)
  lmatch <- regmatches(intervals, lindex)
  nrows <- length(lmatch)
  ncols <- sapply(lmatch, length)
  mmatch <- matrix(NA_character_, nrow = nrows, ncol = 5)
  
  for (x in 1:nrows) {
    row <- lmatch[[x]]
    n <- length(row)
    if (n > 0) {
      mmatch[x, 1:n] <- lmatch[[x]][1:n]
    }
  }
  
  intervals_df <- data.frame(
    interval = intervals,
    left = as.numeric(mmatch[, 3]),
    right = as.numeric(mmatch[, 4]),
    left_strict = (mmatch[, 2] == '['),
    right_strict = (mmatch[, 5] == ']'),
    match_count = ncols,
    stringsAsFactors = FALSE
  )
  
  # Fix if point values
  rx <- "^[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?$"
  points <- grepl(rx, intervals)
  intervals_df$interval[points] <- intervals[points]
  intervals_df$left[points] <- as.numeric(intervals[points])
  intervals_df$right[points] <- as.numeric(intervals[points])
  intervals_df$right_strict[points] <- TRUE
  intervals_df$left_strict[points] <- TRUE
  intervals_df$match_count[points] <- 5
  
  
  for (x in 1:nrows) {
    
    if (intervals_df$match_count[x] != 5) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
      next
    }
    
    if (intervals_df$right[x] < intervals_df$left[x]) {
      warning(paste0('The interval "',intervals_df$interval[x],'" has right < left.'))
    }
    
    if (intervals_df$right[x] == intervals_df$left[x] &
        (!intervals_df$left_strict[x] | !intervals_df$right_strict[x])) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
    }
    
  }
  
  return(intervals_df)
}


# function copied from fancycut package 
wafflecut <- function (x, intervals, buckets = intervals, na.bucket = NA, 
                       unmatched.bucket = NA, out.as.factor = TRUE) {
  l <- length(intervals)
  if (l != length(buckets)) {
    stop("FancyCut requires a 1-1 map from intervals to buckets")
  }
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  out <- rep(NA, length(x))
  intervals_df <- nano:::parse_intervals(intervals)
  for (index in 1:l) {
    b <- buckets[index]
    lower <- intervals_df$left[index]
    upper <- intervals_df$right[index]
    left <- intervals_df$left_strict[index]
    right <- intervals_df$right_strict[index]
    mask <- rep(FALSE, length(x))
    if (left & right) {
      mask <- x >= lower & x <= upper
    }
    if (left & !right) {
      mask <- x >= lower & x < upper
    }
    if (!left & right) {
      mask <- x > lower & x <= upper
    }
    if (!left & !right) {
      mask <- x > lower & x < upper
    }
    out[mask] <- b
  }
  if (sum(is.na(x)) == 0L) {
    na.bucket <- NULL
  }
  else {
    out[is.na(x)] <- na.bucket
  }
  if (sum(is.na(out)) == 0L) {
    unmatched.bucket <- NULL
  }
  else {
    out[is.na(out)] <- unmatched.bucket
  }
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  if (out.as.factor) {
    return(factor(out, levels = levels, exclude = NULL))
  }
  else {
    return(out)
  }
}


# prints each element of a vector separate
print_vector <- function(vec, sep = ", ") {
  out <- vec[1]
  for (i in 2:length(vec)) {
    out <- paste(out, vec[i], sep = sep)
  }
  return(out)
}


# extracts meta data from model
model_meta <- function(model, data) {
  model_info <- h2o:::.process_models_or_automl(model, data)
  meta <- list()
  meta[["x"]] <- model_info$x
  meta[["y"]] <- model_info$y
  meta[["is_classification"]] <- model_info$is_classification
  meta[["is_multinomial_classification"]] <- model_info$is_multinomial_classification
  meta[["distribution"]] <- model@parameters$distribution
  meta[["hyper_params"]] <- model@allparameters[c("ntrees", "max_depth", "min_rows", 
                                                  "learn_rate", "distribution", 
                                                  "tweedie_power", "sample_rate", 
                                                  "col_sample_rate",
                                                  "col_sample_rate_per_tree",
                                                  "min_split_improvement")]
  meta[["search_conditions"]] <- model@allparameters[c("stopping_rounds", "stopping_metric", "stopping_tolerance")]
  meta[["max_runtime_secs"]] <- model@allparameters$max_runtime_secs
  meta[["seed"]] <- model@allparameters$seed
  meta[["description"]] <- ""
  return(meta)
}



# calculates metrics from h2o model. This will be input into nano object
model_metrics <- function(model, data = NULL) {
  
  # determine model type
  if (class(model) == "H2ORegressionModel") {
    model_type = "regression"
  } else if (class(model) == "H2OMultinomialModel") {
    model_type = "multinomial classification"
  } else {
    model_type = "binomial classification"
  }
  
  # assign list to hold metrics
  metrics <- list(train_metrics   = NULL,
                  test_metrics    = NULL,
                  cv_metrics      = NULL,
                  holdout_metrics = NULL)
  
  
  # functions to calculate metrics
  metric_fun <- function(fun, metric) {
    
    # calculate train metrics
    metrics$train_metrics[[metric]] <- fun(model, train = TRUE)
    
    # calculate test metrics
    if (!is.null(model@model$validation_metrics@metrics)) {
      metrics$train_metrics[[metric]] <- fun(model, valid = TRUE)
    }
    
    # calculate cv metrics
    if (!is.null(model@model$cross_validation_metrics@metrics)) {
      metrics$cv_metrics[[metric]] <- fun(model, xval = TRUE)
    }
    return(metrics)
  }
  
  # calculate metrics for all model types
  metrics <- metric_fun(h2o::h2o.r2  , "r2")
  metrics <- metric_fun(h2o::h2o.mse , "mse")
  metrics <- metric_fun(h2o::h2o.rmse, "rmse")
  
  # calculate extra metrics if regression
  if (model_type == "regression") {
    metrics <- metric_fun(h2o::h2o.rmsle                 , "rmsle")
    metrics <- metric_fun(h2o::h2o.mae                   , "mae")
    metrics <- metric_fun(h2o::h2o.mean_residual_deviance, "mean_residual_dev")
  }
  
  # calculate extra metrics if classification
  if (grepl("classification", model_type)) {
    metrics <- metric_fun(h2o::h2o.mean_per_class_error, "mean_class_err")
    metrics <- metric_fun(h2o::h2o.logloss             , "logloss")
  }  
  
  # calculate extra metrics if binomial classification
  if (model_type == "binomial classification") {
    metrics <- metric_fun(h2o::h2o.giniCoef          , "gini_coef")
    metrics <- metric_fun(h2o::h2o.mcc               , "mcc")
    metrics <- metric_fun(h2o::h2o.F1                , "f1")
    metrics <- metric_fun(h2o::h2o.F0point5          , "f0point5")
    metrics <- metric_fun(h2o::h2o.F2                , "f2")
    metrics <- metric_fun(h2o::h2o.accuracy          , "accuracy")
    metrics <- metric_fun(h2o::h2o.auc               , "auc")
    metrics <- metric_fun(h2o::h2o.aucpr             , "aucpr")
    metrics <- metric_fun(h2o::h2o.kolmogorov_smirnov, "ks")
  }
  
   
  # calculate metrics for holdout dataset 
  if ("data_id" %in% names(data)) {
    if ("holdout" %in% data[["data_id"]]) {
      
      # calculate performance object for each data type
      holdout_perf <- h2o::h2o.performance(model, h2o::as.h2o(data[data_id == "holdout"]))

      # function to calculate holdout metric 
      metric_fun <- function(fun, metric) {
        metrics[["holdout_metrics"]][[metric]] <- fun(holdout_perf) 
        return(metrics)
      }
      
      # calculate metrics
      metrics <- metric_fun(h2o::h2o.r2   , "r2")
      metrics <- metric_fun(h2o::h2o.mse  , "mse")
      metrics <- metric_fun(h2o::h2o.rmse , "rmse")
      
      # calculate extra metrics if regression
      if (model_type == "regression") {
        metrics <- metric_fun(h2o::h2o.rmsle                 , "rmsle")
        metrics <- metric_fun(h2o::h2o.mae                   , "mae")
        metrics <- metric_fun(h2o::h2o.mean_residual_deviance, "mean_residual_dev")
      }
      
      # calculate extra metrics if classification
      if (grepl("classification", model_type)) {
        metrics <- metric_fun(h2o::h2o.mean_per_class_error, "mean_class_err")
        metrics <- metric_fun(h2o::h2o.logloss             , "logloss")
      }
      
      # calculate extra metrics if binomial classification
      if (model_type == "binomial classification") {
        metrics <- metric_fun(h2o::h2o.giniCoef          , "gini_coef")
        metrics <- metric_fun(h2o::h2o.mcc               , "mcc")
        metrics <- metric_fun(h2o::h2o.F1                , "f1")
        metrics <- metric_fun(h2o::h2o.F0point5          , "f0point5")
        metrics <- metric_fun(h2o::h2o.F2                , "f2")
        metrics <- metric_fun(h2o::h2o.accuracy          , "accuracy")
        metrics <- metric_fun(h2o::h2o.auc               , "auc")
        metrics <- metric_fun(h2o::h2o.aucpr             , "aucpr")
        metrics <- metric_fun(h2o::h2o.kolmogorov_smirnov, "ks")
      }    
    }
  }
  
  return(metrics)
}



# function to increase space of nano object if it is full
nano_increase <- function(nano) {
  if (sum(sapply(nano$grid, is.logical)) == 0) {
    nano$grid        <- append(nano$grid       , rep(list(NA)      , 10))
    nano$model       <- append(nano$model      , rep(list(NA)      , 10))
    nano$metric      <- append(nano$metric     , rep(list(NA)      , 10))
    nano$data        <- append(nano$data       , rep(data.table(NA), 10))
    nano$varimp      <- append(nano$varimp     , rep(list(NA)      , 10))
    nano$pdp         <- append(nano$pdp        , rep(list(NA)      , 10))
    nano$ice         <- append(nano$ice        , rep(list(NA)      , 10))
    nano$interaction <- append(nano$interaction, rep(list(NA)      , 10))
    nano$meta        <- append(nano$meta       , rep(list(NA)      , 10))
  }
  
  return(nano)
}

