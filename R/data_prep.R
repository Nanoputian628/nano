# data imputation based if variable is factor or numeric
# remove duplicates

# data <- copy(property_prices)
# data$dum <- rep(c("A", "B", "C", "C"), 125)

data_prep <- function(data, y, ignore = c(), train_test = NA, 
                      split_ratio = 0.7, weight = NULL, target = "auto", balance = FALSE, 
                      impute = FALSE, rm_outliers = TRUE, unique_row = TRUE, 
                      scale = FALSE, correlation = FALSE, seed = 628, quiet = FALSE) {
  # set stopwatch
  tic(id = "model_preprocess")
  on.exit(toc(id = "model_preprocess", msg = "Pre-processed in", 
              quiet = TRUE))
  setDT(data)
  if (!y %in% colnames(data)) {
    stop(paste("You should have column 'y' in your dataset or select."))
  }
  
  #colnames(df)[colnames(df) == y] <- "tag"
  if (sum(sapply(data, is.character) > 0)) {
    col_char <- names(data)[sapply(data, is.character)]
    data[, (col_char) := lapply(.SD, as.factor), .SDcols = col_char]
    cat("Following variables converted from character to factor type: ")
    for (i in 1:length(col_char)) {
      cat(if(i == length(col_char)) col_char[i] else paste0(col_char[i], ", "))
    }
  }
  # re-level response if factor type
  if (y %in% col_char) {
    freq <- as.data.frame(table(data[, y, with = FALSE]))
    base_org <- as.character(freq[1, 1])
    base_new <- as.character(freq[which.max(freq[, 2]), 1])
    if (base_org != base_new) {
      data[, (y) := relevel(data[[which(names(data) == y)]], ref = base)]
      message("Base level of response changed from ", '"', base_org, '"', " to ", '"', base_new, '"')
    }
  }
  
  #df <- data.frame(df) %>% filter(!is.na(.data$tag)) %>% mutate_if(is.character, 
  #                                                                 as.factor)
  # extract response vector for manipulations
  res <- data[[which(colnames(data) == y)]]
  res_levels <- unique(res)
  model_type <- ifelse(length(res_levels) <= 10, "Classification", "Regression")
  if (!quiet) 
    message("MODEL TYPE: ", model_type)
  if (model_type == "Classification") 
    res_levels <- gsub(" ", "_", res_levels)
  # if (model_type == "Classification" & y %in% res_levels) {
  #   stop(paste("Your y parameter can't be named as any of the labels used.", 
  #              "Please, rename", y, "into a valid column name next such as", 
  #              paste0(y, "_labels for example.")))
  # }

  ## DONT THINK NEED THIS
  # if (model_type == "Classification" & sum(grepl("^[0-9]", res_levels)) > 0) {
  #   res <- as.factor(as.character(ifelse(grepl("^[0-9]", res), 
  #                                        paste0("n_", res), 
  #                                        as.character(res))))
  # }
  
  if (model_type == "Regression")
    res <- as.numeric(res)
  
  m <- missingness(data, summary = FALSE)
  if (!is.null(m)) {
    m <- mutate(m, label = paste0(.data$variable, " (", 
                                  .data$missingness, "%)"))
    if (!quiet) {
      top10 <- m %>% ungroup() %>% slice(1:10)
      which <- vector2text(top10$label, quotes = FALSE)
      if (nrow(m) > 10) 
        which <- paste(which, "and", nrow(m) - 
                         10, "other.")
      message(paste0("- MISSINGS: The following variables contain missing observations: ", 
                     which, if (!impute & !quiet) 
                       ". Consider using the impute parameter."))
    }
    if (impute) {
      if (!quiet) 
        message(paste(">>> Imputing", sum(m$missing), 
                      "missing values..."))
      df <- impute(df, seed = seed, quiet = TRUE)
    }
  }
  else if (!quiet) 
    message("- MISSINGS: No missing values in your data")
  if (length(ignore) > 0) {
    ignore <- ignore[ignore %in% colnames(df)]
    if (length(ignore) > 0 & !quiet) 
      message(paste("- SKIPPED: Ignored variables for training models:", 
                    vector2text(ignore)))
  }
  temp <- df[, !colnames(df) %in% c("tag", ignore)]
  nums <- df_str(temp, "names", quiet = TRUE)$nums
  if (length(nums) != ncol(temp) & !quiet) 
    message(paste("- CATEGORICALS: There are", ncol(temp) - 
                    length(nums), "non-numerical features.", "Consider using ohse() or equivalent prior to encode categorical variables."))
  if (scale | center & length(nums) > 0) {
    new <- data.frame(lapply(df[nums], function(x) scale(x, 
                                                         center = center, scale = scale)))
    colnames(new) <- nums
    df[nums] <- new
    msg <- ifelse(scale & center, "scaled and centered", 
                  ifelse(scale, "scaled", "centered"))
    if (!quiet) 
      message(paste0("- TRANSFORMATIONS: All numerical features (", 
                     length(nums), ") were ", msg))
  }
  if (is.numeric(df$tag)) {
    thresh <- ifelse(is.numeric(no_outliers), no_outliers, 
                     3)
    is_outlier <- outlier_zscore(df$tag, thresh = thresh)
    if (!quiet & !isTRUE(no_outliers)) 
      message(sprintf("- OUTLIERS: %s (%s) of %s values are considered outliers (Z-Score: >%ssd). %s", 
                      formatNum(100 * sum(is_outlier)/nrow(df), 1, 
                                pos = "%"), formatNum(sum(is_outlier), 
                                                      0), y, thresh, ifelse(no_outliers, paste("Removing them from the dataset for better results.", 
                                                                                               "To keep them, set the 'no_outliers' parameter."), 
                                                                            "Consider using the 'no_outliers' parameter to remove them.")))
    if (no_outliers) 
      df <- df[!is_outlier, ]
  }
  if (is.na(train_test)) {
    if (!quiet) 
      message(">>> Splitting datasets...")
    splits <- msplit(df, size = split, seed = seed, print = !quiet)
    train <- splits$train
    test <- splits$test
    train_index <- splits$train_index
  }
  else {
    if (train_test %in% colnames(df)) {
      colnames(df)[colnames(df) == train_test] <- "train_test"
      if (all(unique(as.character(df$train_test)) %in% 
              c("train", "test"))) {
        train <- filter(df, .data$train_test == "train")
        test <- filter(df, .data$train_test == "test")
        split <- nrow(train)/nrow(df)
        ignore <- c(ignore, train_test)
        train_index <- 1:nrow(train)
        if (!quiet) 
          print(table(df$train_test))
      }
      else stop("Your train_test column should have 'train' and 'test' values only!")
    }
    else stop(paste("There is no column named", train_test))
  }
  if (unique_train) {
    train_rows <- nrow(train)
    train <- distinct(train)
    if (nrow(train) != train_rows & !quiet) 
      message(paste("- REPEATED: There were", train_rows - 
                      nrow(train), "repeated rows which are being suppressed for the train dataset"))
  }
  if (nrow(train) > 10000 & !quiet) 
    message("- SAMPLE: Consider sampling or reduce the 'split' argument for faster results")
  if (model_type == "Classification" & balance) {
    total <- nrow(train)
    min <- freqs(train, .data$tag) %>% .$n %>% min(., na.rm = TRUE)
    train <- train %>% group_by(.data$tag) %>% sample_n(min)
    if (!quiet) 
      message(paste0("- BALANCE: Training set balanced: ", 
                     min, " observations for each (", length(cats), 
                     ") category; using ", round(100 * nrow(train)/total, 
                                                 2), "% of training data"))
  }
  if (model_type == "Classification") {
    if (!all(unique(df$tag) %in% unique(train$tag))) 
      stop(paste("You must train with all available tags:", 
                 vector2text(unique(df$tag))))
    if (!all(unique(df$tag) %in% unique(test$tag))) 
      warning("You are training with tags that are not in your test set.")
  }
  results <- list(data = df, train_index = train_index, model_type = model_type)
  attr(results, "type") <- "model_preprocess"
  return(invisible(results))
}