#' @title Create cleanliness summary of dataset 
#' @description Creates a summary of how clean a dataset is.
#' @param data dataset to be analysed.
#' @param ignore columns in dataset to be ignored. 
#' @param outlier_sd Number of standard deviations away from the mean for a observation to be
#' classified as an outlier. Default is 3.
#' @param quiet print messages to console.
#' @return a list of various summaries and plots on how the clean the dataset is.
#' @details Creates a list of various summaries and plots on how clean a dataset is. The function
#' returns number of; blank observations, NAs, duplicates, outliers as well as number of plots
#' for the mentioned figures. Outliers will only be assessed for numeric columns. If certain 
#' columns are to be excluded from this analysis, use the `ignore` parameter. To prevent messages 
#' from printing to the console, set `quiet` to \code{TRUE} (default is \code{FALSE}).  
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  cleanliness_smry(property_prices)
#'  }
#' }
#' @rdname cleanliness_smry
#' @export 

cleanliness_smry <- function(data, ignore = c(), outlier_sd = 3, quiet = FALSE) {
  # modify dataset
  keep_var <- setdiff(names(data), ignore)
  if (!is.data.table(data)) {
    data <- as.data.table(data)[, keep_var, with = FALSE]
  }
  
  blank_num <- sapply(data, function(x) if(is.factor(x)) sum(gsub(" ", "", x) == "") else 0)
  na_num <- sapply(data, function(x) sum(is.na(x)))
  if (!quiet) {
    message("Dataset has ", sum(blank_num), " blank values and ", sum(na_num), " na values.")
  }
  
  if (sum(blank_num) + sum(na_num) != 0) {
    missing_var <- union(names(blank_num)[(blank_num != 0)], names(na_num)[na_num != 0])
    blank_var <- as.vector(blank_num[match(missing_var, names(blank_num))])
    na_var <- as.vector(na_num[match(missing_var, names(na_num))])
    missing_smry <- data.table(variable   = missing_var, 
                               blank_num  = blank_var, 
                               blank_perc = blank_var / nrow(data) * 100,
                               na_num     = na_var,
                               na_perc    = na_var / nrow(data) * 100,
                               total_num  = blank_var + na_var, 
                               total_perc = (blank_var + na_var)/nrow(data) * 100)
  } else {
    missing_smry <- "Dataset has no missing values."
  }
  
  if (sum(blank_num) != 0) {
    blank_plot <- plot(blank_num) # replace with own user defined plotting functions
  } else {
    blank_plot <- "Dataset has no blanks"
  }
  if (sum(na_num) != 0) {
    na_plot <- plot(na_num) # replace with own user defined plotting functions
  } else {
    na_plot <- "Dataset has no nas"
  }
  
  # duplicates in dataset
  dup_index <- duplicated(data) | duplicated(data, fromLast = TRUE)
  dup_num <- sum(dup_index)
  if (dup_num == 0) duprows <- "Dataset has no duplicates!" else dup_rows <- data[dup_index]
  if (!quiet) {
    message("Dataset has ", dup_num, " duplicates.")
  }
  
  # outliers in dataset
  outlier_fun <- function(x) {
    which(abs((x - mean(x))/sd(x)) > outlier_sd)
  }
  outlier_index <- lapply(data[, sapply(data, is.numeric), with = FALSE], outlier_fun)
  outlier_num <- sapply(outlier_index, length)
  if (sum(outlier_num) != 0) {
    outlier_var <- names(outlier_index)[outlier_num != 0]
    outlier_row <- c()
    for (var in outlier_var) {
      outlier_row <- union(outlier_row, outlier_index[[var]])
    }
    outliers <- copy(data)[outlier_row][
      , outlier_var, with = FALSE][
        , index := outlier_row
      ]
    for (var in outlier_var) {
      outliers[, (var) := ifelse(outliers$index %in% outlier_index[[var]], var, "")]
    }
    outlier_cols <- rep("", nrow(outliers))
    for (var in outlier_var) {
      outlier_cols <- ifelse(as.vector(outliers[[var]]) == "", 
                             outlier_cols, 
                             paste(outlier_cols, as.vector(outliers[[var]]), sep = ", "))
    }
    outlier_cols <- gsub("^, +|, +$", "", outlier_cols)
    outliers_dat <- copy(data)[outlier_row][
      , outlier_cols := outlier_cols]
    outlier_plot <- plot(outlier_num) # replace with own user defined plotting functions  
  } else {
    outliers_dat <- "Dataset has no outliers!"
    outliers_plot <- "Dataset has no outliers!"
  }
  if (!quiet) {
    message("Dataset has ", sum(outlier_num), " outliers.")
  }
  
  # check for special characters
  pattern <- "/|:|\\?|<|>|\\|\\\\|\\*|\\@|\\#|\\$|\\%|\\^|\\&"
  special_index <- lapply(data, function(x) which(grepl(pattern, x)))
  special_num <- sapply(special_index, length)
  if (sum(special_num) != 0) {
    special_var <- names(special_num)[special_num != 0]
    special_row <- c()
    for (var in special_var) {
      special_row <- union(special_row, special_index[[var]])
    }
    special_dat <- copy(data)[special_row][, special_var, with = FALSE]
    special_plot <- plot(special_num) # replace with own user defined plotting functions
  } else {
    special_dat <- "Dataset has no special characters!"
    special_plot <- "Dataset has no special characters!"
  }
  if (!quiet) {
    message("Dataset has ", sum(special_num), " special characters.")
  }
  
  list(blanks = blank_num,
       nas = na_num,
       missing_smry = missing_smry,
       duplicates = dup_rows,
       outliers = outlier_num,
       outlier_rows = outliers_dat,
       special_chars = special_num,
       special_rows = special_dat,
       plots = list(blanks = blank_plot, 
                     nas = na_plot, 
                     outliers = outlier_plot, 
                     special_chars = special_plot
                     )
       )
  
  }
clean <- cleanliness_smry(data, outlier_sd = 5)
clean$missing_smry


