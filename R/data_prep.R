#' @title Prepare Dataset for Modelling
#' @description Prepares dataset for modelling by cleaning, banding, imputing and by other 
#' options.
#' @importFrom caret nearZeroVar
#' @param data dataset to be analysed.
#' @param response response variable to be used in modelling. 
#' @param intervals a list defining the bands for each of the variables.
#' @param buckets a list defining the names of the bands for each of the variables.
#' @param na_bucket a character or a list defining the bucket name for entries with \code{NA}.
#' @param unmatched_bucket a character or a list defining the bucket name for unmatched entries.
#' @param trunc_left a logical specifying whether the band to \code{-Inf} should be created.  
#' @param trunc_right a logical specifying whether the band to \code{Inf} should be created.
#' @param include_left a logical specifying if should include the left or right endpoint for 
#' each interval.
#' @param split_or_fold a numeric. If between 0 and 1, dataset is split into training and 
#' testing dataset. The number specifies the percentage of rows to be kept for training. If 1,
#' dataset is not split and all rows kept for training. If an integer greater than 1,
#' specifies number of folds to divide the dataset into.
#' @param holdout_ratio a numeric between 0 and 1. Specifies what percentage of rows in the 
#' original dataset should be used for the holdout dataset. 
#' @param unique_row a logical. Whether duplicate rows should be deleted or retained.
#' @param rm_low_var a logical. Whether variables with low variance should be deleted or 
#' retained.
#' @param freq_thresh the cutoff for the ratio of the most common value to the second most 
#' common value in which a variable is removed when \code{rm_low_var = TRUE}. 
#' @param impute a logical. Whether missing values and outliers should be imputed. 
#' @param impute_method method of imputation. Possible methods are "mice" or "mean/mode".
#' @param pred_ignore columns in dataset to be not used in data imputation process. Only required if `method` = "mice".
#' @param impute_ignore columns in dataset to be not imputed.
#' @param rm_outliers a numeric where values which are `rm_outliers` standard deviations 
#' away from the mean will be imputed. Can either be a single number or a vector of numbers for 
#' each column in `data` (including variables in `impute_ignore`. By default, set to \code{Inf}, 
#' hence no outliers are imputed. 
#' @param vif_select a logical. Whether stepwise VIF selection should be performed.
#' @param vif_ignore columns in dataset to be not removed. Only relevant if `remove` is
#' \code{TRUE}. 
#' @param vif_thresh threshold of VIF for variables to be removed.
#' @param balance a logical. Whether the dataset should be balanced. 
#' @param balance_class categorical variable in dataset to be balanced by. This is an optional
#' argument. 
#' @param balance_method specifies whether undersampling or oversample should be performed. 
#' Takes the value "under" or "over".
#' @param balance_prop desired distribution of response per each class.
#' @param scale a logical specifying whether the numeric variables should be scaled with 0 mean 
#' and 1 standard deviation. 
#' @param seed seed for `set.seed`.
#' @param quiet a logical specifying whether messages should be output to the console. 
#' @param used for determining whether building a regression or classification model. If number
#' of unique levels in `response` is less than `thresh`, then classification, otherwise 
#' regression model.  
#' @param target_encode a logical specifying whether to perform target encoding on factor 
#' variables.
#' @param encode_cols a character vector. Factor type variables to be target encoded.
#' @param blend a logical specifying whether the target average should be weighted based on
#' the count of the group.
#' @param encode_inflec a numeric. This determines half of the minimal sample size for which
#' the the estimate based on the sample in the particular level is completely trusted. This 
#' value is only valid when \code{blend = TRUE}. 
#' @param smoothing a numeric. The smoothing value is used for blending. Only valid when 
#' \code{blend = TRUE}.
#' @param noise a numeric. Specify the amount of random noise that should be added to the target
#' average in order to prevent overfitting. Set to 0 to disable noise.
#' @return List containing prepared dataset with various other metrics and summaries depending
#' on the arguments entered.
#' @details The purpose of this function is to provide a general and off-the-shelf process to
#' quickly prepare raw datasets for modelling. A large amount of flexibility is provided by 
#' the function and has the options to: band variables, impute missing values and outliers,
#' perform step-wise VIF selection and balance the dataset by class. For further details on
#' these process and their arguments, see the following functions respectively contained in the 
#' `nano` package: band_data, impute, vif_step and balance_data.
#' 
#' This function also provides the option to: split the dataset into k folds, training, testing,
#' holdout dataset via the `split_or_fold` and `holdout_ratio` arguments. To split dataset into
#' training and testing dataset, set `split_or_fold` to be a number between 0 and 1. To divide the
#' dataset into k folds, set `split_or_fold` to be an integer greater than 1. If the dataset has
#' been split into training and testing, or divided into k folds, additionally, a holdout dataset
#' can be created. This can be done by using the `holdout_ratio` argument. Importantly, the 
#' holdout dataset can only be created if the dataset has been split into training and testing, or
#' into k folds (i.e. `split_or_fold` != 1). 
#' 
#' Other features available in this function are: remove variables with low variance via the 
#' `rm_low_var` argument, target encoding via the `target_encode` argument and scale numeric 
#' variables via the `scale` argument. 
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  data_prep(data          = property_prices, 
#'            split_or_fold = 0.7, 
#'            holdout_ratio = 0.1, 
#'            impute        = TRUE, 
#'            vif_select    = TRUE,
#'            quiet         = TRUE)
#'  }
#' }
#' @rdname data_prep
#' @export 

data_prep <- function(data, response, intervals = NULL, buckets = NULL, na_bucket, 
                      unmatched_bucket, trunc_left = FALSE, trunc_right = FALSE, 
                      include_left = TRUE, split_or_fold = 1.0, holdout_ratio = 0, 
                      unique_row = TRUE, rm_low_var = FALSE, freq_thresh = 95/5, 
                      impute = FALSE, impute_method = "mice", pred_ignore = c(), 
                      impute_ignore = c(), rm_outliers = Inf, vif_select = FALSE, 
                      vif_ignore = c(), vif_thresh = 5, balance = FALSE, balance_class, 
                      balance_method = "under", balance_prop = 0.5, scale = FALSE, seed = 628,
                      quiet = FALSE, thresh = 10, retain_names = TRUE, target_encode = FALSE,
                      encode_cols, blend = FALSE, encode_inflec = 50, smoothing = 20, 
                      noise) {
  
  # set stopwatch
  nano:::tic(id = "data_prep")
  on.exit(nano:::toc(id = "data_prep", msg = "Data prep in", 
              quiet = TRUE))

  if (!"data.frame" %in% class(data)) {
    stop("`data` must have class data.frame.", 
         call. = FALSE)
  }

  if (!response %in% names(data)) {
    stop(paste("'response' is not a column in `data`."), 
         call. = FALSE)
  }
  
  if (!is.numeric(split_or_fold)) {
    stop("`split_or_fold` must be numeric.", 
         call. = FALSE) 
  }
  
  if (split_or_fold < 0 | (split_or_fold > 1 & split_or_fold %% 1 != 0)) {
    stop("`split_or_fold` must be between 0 and 1 or an integer greater than 1.",
         call. = FALSE)
  }
  
  if (!is.numeric(holdout_ratio)) {
    stop("`holdout_ratio` must be numeric.", 
         call. = FALSE) 
  }
  
  if (holdout_ratio < 0 | holdout_ratio > 1) {
    stop("`holdout_ratio` must be between 0 and 1 or an integer greater than 1.",
         call. = FALSE)
  }
  
  if (split_or_fold == 1 & holdout_ratio > 0) {
    stop("Cannot create holdout dataset without creating test dataset. Try using the `split_or_fold` argument instead.",
         call. = FALSE)
  }
  
  if (split_or_fold < 1 & split_or_fold + holdout_ratio >= 1) {
    stop("`split_or_fold` + `holdout_ratio` must be strictly less than 1.",
         call. = FALSE)
  }
  
  if (!is.logical(rm_low_var)) {
    stop("`rm_low_var` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.numeric(freq_thresh)) {
    stop("`freq_thresh` must be numeric.", 
         call. = FALSE)
  }

  if (freq_thresh < 1) {
    stop("`freq_thresh` must be greater than 1.", 
         call. = FALSE)
  }
  
  if (!is.logical(impute)) {
    stop("`impute` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.logical(vif_select)) {
    stop("`vif_select` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.logical(balance)) {
    stop("`balance` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.logical(unique_row)) {
    stop("`unique_row` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.logical(scale)) {
    stop("`scale` must either be TRUE or FALSE.", 
         call. = FALSE)
  }
  
  if (!is.logical(target_encode)) {
    stop("`target_encode` must either be TRUE or FALSE.",
         call. = FALSE)
  }
  
  if (!is.logical(blend)) {
    stop("`blend` must either be TRUE or FALSE.",
         call. = FALSE)
  }
  

  data <- nano::tidy_data(data, thresh, retain_names, response)
  
  # summary on how clean the dataset is
  if (!quiet) {
    message("Following numbers are based on the entire dataset:")
  }
  clean_smry <- nano::cleanliness_smry(data = data, outlier_sd = rm_outliers, quiet = quiet)
  if (unique_row) {
    data <- clean_smry$unique_rows
    if (!quiet) {
      if (!is.character(clean_smry$duplicates)) {
        message(paste0(nrow(clean_smry$duplicates), " duplicates were removed from dataset."))
      }
    }
  }
  
  # message for variables which are blank
  if (!quiet) {
    if (sum(clean_smry$blanks) > 0) {
      blank_var <- names(clean_smry$blanks)[clean_smry$blanks > 0]
      cat("The following variables have blank values:\n")
      for (i in 1:length(blank_var)) {
        cat(paste0(" ", blank_var[i], ": ", clean_smry$blanks[names(clean_smry$blanks) == blank_var[i]], "\n"))
      }
    } 
    
    # message for variables which are NA
    if (sum(clean_smry$nas) > 0) {
      na_var <- names(clean_smry$nas)[clean_smry$nas > 0]
      cat("The following variables have NAs:\n")
      for (i in 1:length(na_var)) {
        cat(" ", paste0(na_var[i], ": ", clean_smry$nas[names(clean_smry$nas) == na_var[i]], "\n"))
      }
    } 
    
    # message for variables which have special characters
    if (sum(clean_smry$special_chars) > 0) {
      special_var <- names(clean_smry$special_chars)[clean_smry$special_chars > 0]
      cat("The following variables have special characters:\n")
      for (i in 1:length(special_var)) {
        cat(paste0(" ", special_var[i], ": ", clean_smry$special_chars[names(clean_smry$special_chars) == special_var[i]], "\n"))
      }
    }
  }
  
  # missing pattern for entire dataset
  missing_pat <- nano::missing_pattern(data, plot = FALSE)
  
  # determine type of model to be built
  res_levels <- unique(data[[which(colnames(data) == response)]])
  model_type <- ifelse(length(res_levels) <= thresh, "Classification", "Regression")
  if (!quiet) message("MODEL TYPE: ", model_type)
  if (model_type == "Regression") {
    data[, (response) := as.numeric(get(response))]
  } else {
    data[, (response) := as.factor(get(response))]
  }

  # band variables
  if (!is.null(intervals)) {
    nano::band_data(data, intervals, buckets = buckets, na_bucket, unmatched_bucket, 
                    trunc_left = trunc_left, trunc_right = trunc_right, 
                    include_left = include_left)
  }
  
  # split data or assign folds depending on value of split_or_fold
  if (split_or_fold <= 1) {
    # split dataset in training and testing
    split <- caret::createDataPartition(as.vector(data[[response]]), p = split_or_fold, list = FALSE)
    data_train <- data[split] 
    data_test <- data[-split]
    if (!quiet) {
      message(paste0("Dataset split into training and testing dataset in ", split_or_fold, " ratio:
Training dataset: ", nrow(data_train), " rows
Testing dataset: ", nrow(data_test), " rows."))
    }
  } else {
    split <- caret::createDataPartition(as.vector(data[[response]]), p = 1 - holdout_ratio,
                                        list = FALSE)
    data_train <- data[split] 
    # create holdout dataset 
    # (however called data_test to avoid different names for different cases)
    data_test <- data[-split]
    # assign folds to training dataset
    fold <- caret::createFolds(as.vector(data_train[[response]]), k = split_or_fold, 
                               list = FALSE)
    data_train[, fold := as.factor(fold)]
  }
  
  ### CREATE PLOT TO VERIFY DATA IS BALANCED BETWEEN TRAINING/TESTING/HOLDOUT
  # p1 <- ggplot(data = xtrain0 %>% filter(pure_premium < 20000 & pure_premium > 0)) + 
  #   geom_histogram(mapping = aes(x = pure_premium), fill = "royalblue") +
  #   lares::theme_lares2(bg_colour = "white") # histogram of training data 
  # p2 <- ggplot(data = xtest0 %>% filter(pure_premium < 20000 & pure_premium > 0)) + 
  #   geom_histogram(mapping = aes(x = pure_premium), fill = "royalblue") +
  #   lares::theme_lares2(bg_colour = "white") # histogram of test data
  # # display plots
  # grid.arrange(p1, p2, nrow = 1) # plots are very similar as expected
  
  
  
  # re-level response if factor type
  if (is.factor(data_train[, response, with = FALSE])) {
    freq <- as.data.frame(table(data_train[, response, with = FALSE]))
    base_org <- as.character(freq[1, 1])
    base_new <- as.character(freq[which.max(freq[, 2]), 1])
    if (base_org != base_new) {
      data_train[, (response) := relevel(data_train[[response]], ref = base_new)]
      if (!quiet) {
        message("Base level of response changed from ", '"', base_org, '"', " to ", '"', base_new, '"')
      }
    }
    # re-level in same way for test data
    if (nrow(data_test) > 0) {
      data_test[, (response) := relevel(data_test[[response]], ref = base_new)]
    }
  }
  
  # remove variables with low variance
  if (rm_low_var) {
    freq <- caret::nearZeroVar(data_train, freqCut = freq_thresh, saveMetrics = TRUE)
    rm_var <- rownames(freq)[freq$zeroVar | freq$nzv]
    data_train[, (rm_var) := NULL]
    data_test[, (rm_var) := NULL]
    if (!quiet) {
      cat("The following variables were removed due to low variance:\n")
      for (var in rm_var) {
        freq_ratio <- round(freq$freqRatio[rownames(freq) == var], 2)
        cat(paste0(" ", var, ": ", freq_ratio, "\n"))
      }
    }
    # adjust vif_ignore for removed variables
    vif_ignore <- setdiff(vif_ignore, rm_var)
   }
  
  # vif step-wise selection
  if (vif_select) {
    train_vif <- nano::vif_step(data_train, 
                                ignore = vif_ignore, 
                                thresh = vif_thresh, trace = FALSE, remove = TRUE)
    
    # print variables that were removed
    if (!quiet) {
      cat("Following variables were removed due to high VIF:\n")
      rm_var_vif <- setdiff(names(data_train), names(train_vif$data))
      for (var in rm_var_vif) {
        cat(paste0(" ", var, "\n"))
      }
    }
    
    # remove variables from datasets
    data_train <- train_vif$data
    data_test  <- data_test[, intersect(names(data_train), names(data_test)), with = FALSE]
  }
  
  # impute training dataset
  if (impute) {
    # add banded variables to list of ignored variables to prevent collinearity with the
    # original variables  
    band_vars <- names(data_train)[grepl("_bnd", names(data_train), fixed = TRUE)]
    pred_ignore <- c(pred_ignore, band_vars, if (split_or_fold > 1) "fold")
    impute_ignore <- c(impute_ignore, band_vars, if (split_or_fold > 1) "fold")
    imputation_train <- nano:::quiet(nano::impute(data           = data_train   , 
                                                  method         = impute_method, 
                                                  pred_ignore    = pred_ignore  , 
                                                  impute_ignore  = impute_ignore,
                                                  impute_outlier = rm_outliers  , 
                                                  seed           = seed)
    )
    data_train <- data.table::copy(imputation_train$imputed_data)
    if (nrow(data_test) > 0) {
      if (impute_method == "mice") {
        # extract imputation formula
        pred_matrix <- imputation_train$imputation_smry$predictorMatrix
        pred_matrix <- pred_matrix[, colnames(pred_matrix) %in% names(data_test)]
        pred_matrix <- pred_matrix[rownames(pred_matrix) %in% names(data_test), ]
        meth <- imputation_train$imputation_smry$method
        # impute test dataset
        imputation_test <- nano:::quiet(nano::impute(data           = data_test  , 
                                                     pred_matrix    = pred_matrix, 
                                                     impute_outlier = rm_outliers, 
                                                     seed           = seed)
        )
        data_test <- data.table::copy(imputation_test$imputed_data)
      } else {
        # extract impute mean/modes
        impute_values <- imputation_train$imputation_smry$imputed_value
        missing_var <- names(data_test)[colSums(is.na(data_test)) > 0][
          !(names(data_test)[colSums(is.na(data_test))>0] %in% impute_ignore)]
        # impute test dataset
        for (var in missing_var) {
          val <- impute_values[names(impute_values) == var]
          data_test[, (var) := replace(get(var), is.na(get(var)), val)]
        }
      }    
    } 
  }
  
  # target encoding 
  if (target_encode) {
    
    # start h2o connection if no existing connection 
    is_h2o_running <- nano:::check_h2o_connection()
    if (!is_h2o_running) nano::nano_init()

    # set default value for noise if missing 
    if (missing(noise)) {
      if (model_type == "Regression") {
        noise <- (max(data_train[[response]]) - min(data_train[[response]])) * 0.01
      } else {
        noise <- 0
      }
    }
    
    if (!missing(encode_cols)) {
      # adjust encode_cols 
      encode_cols <- intersect(encode_cols, names(data_train))
      # check if all are factor types
      if (!all(encode_cols %in% names(data_train)[sapply(data_train, is.factor)])) {
        stop("`encode_cols` must be factor variables in dataset.",
             call. = FALSE)
      }  
    } else {
      # if missing, set to all factor variables
      encode_cols <- names(data_train)[sapply(data_train, is.factor)]
    }
    
    # encode factor variables
    target_encoder <- do.call(h2o::h2o.targetencoder, 
                              c(list(training_frame = h2o::as.h2o(data_train),
                                     x              = encode_cols,
                                     y              = response,
                                     blending       = blend,
                                     noise          = noise, 
                                     seed           = seed),
                                list(fold_column           = "fold",
                                     data_leakage_handling = "kFold")["fold" %in% names(data_train)],
                                list(inflection_point = encode_inflec,
                                     smoothing        = smoothing)[blend]))
    
    # new target encoded dataset
    data_train <- data.table::as.data.table(h2o::h2o.transform(target_encoder, 
                                                               h2o::as.h2o(data_train), 
                                                               as_training = TRUE))
    data_test <- data.table::as.data.table(h2o::h2o.transform(target_encoder, 
                                                              h2o::as.h2o(data_test), 
                                                              noise = 0))
    # remove original variables before target encoding
    data_train[, (encode_cols) := NULL]
    data_test[, (encode_cols) := NULL]
    
    # shutdown h2o connection if did not exist beforehand
    if (!is_h2o_running) nano::nano_shutdown(prompt = FALSE)
  } 
  
  
  # balance training data
  if (balance) {
    if (model_type == "Classification" & balance) {
      data_train <- nano::balance_data(data     = data_train    , 
                                       class    = balance_class , 
                                       response = response      , 
                                       method   = balance_method, 
                                       prop     = balance_prop  , 
                                       thresh   = 20            , 
                                       quiet    = quiet         ,
                                       seed     = seed)
    }
  }
  
  # scale dataset
  if (scale) {
    var_numeric <- names(data_train)[sapply(data_train, is.numeric)]
    var_numeric_scale <- paste0(var_numeric, "_scale")
    scale_var <- function(x) {
      (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
    }
    # scale training dataset
    data_train[, (var_numeric_scale) := lapply(.SD, scale_var), .SDcols = var_numeric]
    
    # scale testing dataset
    if (nrow(data_test) > 0) {
      for (var in var_numeric) {
        var_scale <- paste0(var, "_scale")
        mean <- mean(data_train[[var]], na.rm = TRUE)
        sd <- sd(data_train[[var]], na.rm = TRUE)
        data_test[, (var_scale) := (get(var) - mean)/sd]
      }
    }
  }

  # combine all datasets again and label training, testing and holdout datasets
  data_train[, data_id := "train"]
  if (split_or_fold < 1) {
    data_test[, data_id := "test"]
    # split test dataset into holdout dataset
    if (holdout_ratio > 0) {
      split <- caret::createDataPartition(data_test[[response]], 
                                          p = holdout_ratio/(1-split_or_fold), 
                                          list = FALSE)
      data_test[split, data_id := "holdout"]
    } 
    data <- rbind(data_train, data_test)
  } else if (split_or_fold == 1) {
    data <- copy(data_train)
  } else {
    data_test[, data_id := "holdout"
              ][, fold := 0
                ][, fold := as.factor(fold)]
    data <- rbind(data_train, data_test)
  }
  
  out <- list(data                  = data,
              model_type            = model_type,
              clean_smry            = clean_smry,
              missing_pattern       = missing_pat,
              imputation_train_smry = if (impute) imputation_train$imputation_smry,
              train_vif             = if (vif_select) train_vif$vif
              )
  return(out)
}
