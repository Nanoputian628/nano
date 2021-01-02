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
#' @param include_left a logical specifying if should include the left or right endpoint for each
#' interval.
#' @param split_ratio ratio to split dataset into training and test dataset. Set to 1 to not
#' split the dataset. 
#' @param unique_row a logical specifying whether duplicate rows should be deleted or retained.
#' @param rm_low_var a logical specifying whether variables with low variance should be 
#' deleted or retained.
#' @param freq_thresh the cutoff for the ratio of the most common value to the second most 
#' common value in which a variable is removed when \code{rm_low_var = TRUE}. 
#' @param impute a logical specifying whether missing values and outliers should be imputed. 
#' @param impute_method method of imputation. Possible methods are "mice" or "mean/mode".
#' @param pred_ignore columns in dataset to be not used in data imputation process. Only required if `method` = "mice".
#' @param impute_ignore columns in dataset to be not imputed.
#' @param rm_outliers a numeric where values which are `rm_outliers` standard deviations 
#' away from the mean will be imputed. Can either be a single number or a vector of numbers for 
#' each column in `data` (including variables in `impute_ignore`. By default, set to \code{Inf}, 
#' hence no outliers are imputed. 
#' @param vif_select a logical specifying whether stepwise VIF selection should be performed.
#' @param vif_ignore columns in dataset to be not removed. Only relevant if `remove` is
#' \code{TRUE}. 
#' @param vif_thresh threshold of VIF for variables to be removed.
#' @param balance a logical specifying whether the dataset should be balanced. 
#' @param balance_class categorical variable in dataset to be balanced by. This is an optional
#' argument. 
#' @param balance_method specifies whether undersampling or oversample should be performed. Takes
#' the value "under" or "over".
#' @param balance_prop desired distribution of response per each class.
#' @param scale a logical specifying whether the numeric variables should be scaled with 0 mean 
#' and 1 standard deviation. 
#' @param seed seed for `set.seed`.
#' @param quiet a logical specifying whether messages should be output to the console. 
#' @param used for determining whether building a regression or classification model. If number
#' of unique levels in `response` is less than `thresh`, then classification, otherwise 
#' regression model.  
#' @return List containing prepared dataset with various other metrics and summaries depending
#' on the arguments entered.
#' @details The purpose of this function is to provide a general and off-the-shelf process to
#' quickly prepare raw datasets for modelling. A large amount of flexibility is provided by 
#' the function and has the options to: band variables, impute missing values and outliers,
#' perform step-wise VIF selection and balance the dataset by class. For further details on
#' these process and their arguments, see the following functions respectively contained in the 
#' `nano` package: band_data, impute, vif_step and balance_data.
#' 
#' This function also provides the option to: split the dataset into training and testing 
#' dataset via the `split_ratio` argument, remove variables with low variance via the 
#' `rm_low_var` argument and to scale numeric variables via the `scale` argument. 
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  data_prep(data = property_prices, split_ratio = 0.8, impute = TRUE, vif_select = TRUE,
#'            quiet = TRUE)
#'  }
#' }
#' @rdname data_prep
#' @export 

data_prep <- function(data, response, intervals = NULL, buckets = NULL, na_bucket, 
                      unmatched_bucket, trunc_left = FALSE, trunc_right = FALSE, 
                      include_left = TRUE, split_ratio = 0.7, unique_row = TRUE, 
                      rm_low_var = FALSE, freq_thresh = 95/5, impute = FALSE,
                      impute_method = "mice", pred_ignore = c(), impute_ignore = c(), 
                      rm_outliers = Inf, vif_select = FALSE, vif_ignore = c(), vif_thresh = 5,
                      balance = FALSE, balance_class, balance_method = "under", 
                      balance_prop = 0.5, scale = FALSE, seed = 628, quiet = FALSE, thresh = 10) {
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
  
  if (!is.numeric(split_ratio)) {
    stop("`split_ratio` must be numeric.", 
         call. = FALSE) 
  }
  
  if (split_ratio < 0 | split_ratio > 1) {
    stop("`split_ratio` must be between 0 and 1.",
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
  
  # convert characters to factors
  setDT(data)
  if (sum(sapply(data, is.character)) > 0) {
    col_char <- names(data)[sapply(data, is.character)]
    data[, (col_char) := lapply(.SD, as.factor), .SDcols = col_char]
    if (!quiet) {
      cat("Following variables converted from character to factor type:\n")
      for (i in 1:length(col_char)) {
        cat(paste0(col_char[i], "\n"))
      } 
    }
  }
  
  # filter rows with blanks or NAs in response
  tmp <- sum(is.na(data[, response, with = FALSE])) + sum(data[, response, with = FALSE] == "", na.rm = TRUE)
  if (tmp > 0) {
    data <- data[!is.na(get(response)) & !get(response) == ""]
    if (!quiet) {
      message(paste0(tmp, " rows were removed due to missing values in ", response, "."))
    }
  }

  # summary on how clean the dataset is
  if (!quiet) {
    message("Following numbers are based on the entire dataset.")
  }
  clean_smry <- cleanliness_smry(data = data, outlier_sd = rm_outliers, quiet = quiet)
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
        cat(paste0(blank_var[i], ": ", clean_smry$blanks[i], "\n"))
      }
    } 
    
    # message for variables which are NA
    if (sum(clean_smry$nas) > 0) {
      na_var <- names(clean_smry$nas)[clean_smry$nas > 0]
      cat("The following variables have NAs:\n")
      for (i in 1:length(na_var)) {
        cat(paste0(na_var[i], ": ", clean_smry$nas[i], "\n"))
      }
    } 
    
    # message for variables which have special characters
    if (sum(clean_smry$special_chars) > 0) {
      special_var <- names(clean_smry$special_chars)[clean_smry$special_chars > 0]
      cat("The following variables have NAs:\n")
      for (i in 1:length(special_var)) {
        cat(paste0(special_var[i], ": ", clean_smry$special_chars[i], "\n"))
      }
    }
  }
  
  # missing pattern for entire dataset
  missing_pat <- missing_pattern(data, plot = FALSE)
  
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
    band_data(data, intervals, buckets = buckets, na_bucket, unmatched_bucket, 
              trunc_left = trunc_left, trunc_right = trunc_right, 
              include_left = include_left)
  }
  
  # split data to training and testing if specified
  if (split_ratio < 1) {
    split <- caret::createDataPartition(as.vector(data[[response]]), p = split_ratio, list = FALSE)
    data_train <- data[split] 
    data_test <- data[-split]
    if (!quiet) {
      message(paste0("Dataset split into training and testing dataset in ", split_ratio, " ratio:
Training dataset: ", nrow(data_train), " rows
Testing dataset: ", nrow(data_test), " rows."))
    }
  }
  
  # re-level response if factor type
  if (is.factor(data_train[, response, with = FALSE])) {
    freq <- as.data.frame(table(data_train[, y, with = FALSE]))
    base_org <- as.character(freq[1, 1])
    base_new <- as.character(freq[which.max(freq[, 2]), 1])
    if (base_org != base_new) {
      data_train[, (response) := relevel(data_train[[which(names(data_train) == y)]], ref = base_new)]
      if (!quiet) {
        message("Base level of response changed from ", '"', base_org, '"', " to ", '"', base_new, '"')
      }
    }
    # re-level in same way for test data
    data_test[, (response) := relevel(data_test[[which(names(data_train) == y)]], ref = base_new)]
  }
  
  # remove variables with low variance
  if (rm_low_var) {
    freq <- caret::nearZeroVar(data_train, freqCut = freq_thresh)
    rm_var <- rownames(freq)[freq$zeroVar | freq$nzv]
    data_train[, (rm_var) := NULL]
    data_test[, (rm_var) := NULL]
    if (!quiet) {
      cat("The following variables were removed due to low variance:\n")
      for (var in rm_var) {
        freq_ratio <- round(freq$freqRatio[rownames(freq) == var], 2)
        cat(paste0(var, ": ", freq_ratio, "\n"))
      }
    }
   }
  
  # vif step-wise selection
  if (vif_select) {
    train_vif <- vif_step(data_train, ignore = vif_ignore, thresh = vif_thresh, trace = FALSE,
                          remove = TRUE)
    data_train_vif <- train_vif$data
    # keep same variables in test dataset
    data_test_vif <- data_test[, names(data_train_vif), with = FALSE]
    # print variables that were removed
    if (!quiet) {
      cat("Following variables were removed due to high VIF:\n")
      rm_var_vif <- setdiff(names(data_train), names(data_train_vif))
      for (var in rm_var_vif) {
        cat(paste0(var, "\n"))
      }
    }
  }
  
  # impute training dataset
  if (impute) {
    # add banded variables to list of ignored variables to prevent collinearity with the
    # original variables  
    band_vars <- names(data_train_vif)[grepl("_bnd", names(data_train_vif), fixed = TRUE)]
    pred_ignore <- c(pred_ignore, band_vars)
    impute_ignore <- c(impute_ignore, band_vars)
    imputation_train <- impute(data_train_vif, method = impute_method, pred_ignore = pred_ignore,
                               impute_ignore = impute_ignore, impute_outlier = rm_outliers, 
                               seed = seed)
    data_train <- copy(imputation_train$imputed_data)
    if (impute_method == "mice") {
      # extract imputation formula
      pred_matrix <- imputation_train$imputation_smry$predictorMatrix
      meth <- imputation_train$imputation_smry$method
      # impute test dataset
      imputation_test <- impute(data_test_vif, pred_matrix = pred_matrix, 
                                impute_outlier = rm_outliers, seed = seed)
      data_test <- copy(imputation_test$imputed_data)
    } else {
      # extract impute mean/modes
      impute_values <- imputation_train$imputation_smry$imputed_value
      missing_var <- names(data_test_vif)[colSums(is.na(data_test_vif)) > 0][
        !(names(data_test_vif)[colSums(is.na(data_test_vif))>0] %in% impute_ignore)]
      # impute test dataset
      for (var in missing_var) {
        val <- impute_values[names(impute_values) == var]
        data_test <- copy(data_test_vif)
        data_test[, (var) := replace(get(var), is.na(get(var)), val)]
      }
    }
  }
  
  # balance training data
  if (balance) {
    if (model_type == "Classification" & balance) {
      data_train_vif <- balance_data(data     = data_train_vif, 
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
    for (var in var_numeric) {
      var_scale <- paste0(var, "_scale")
      mean <- mean(data_train[[var]], na.rm = TRUE)
      sd <- sd(data_train[[var]], na.rm = TRUE)
      data_test[, (var_scale) := (get(var) - mean)/sd]
    }
  }

  out <- list(data = list(train_data = data_train, test_data = data_test),
              model_type = model_type,
              clean_smry = clean_smry,
              missing_pattern = missing_pat,
              imputation_train_smry = if (impute) imputation_train$imputation_smry,
              train_vif = if (vif_select) train_vif$vif
              )
  return(out)
}
