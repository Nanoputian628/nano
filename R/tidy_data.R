#' @title Tidy Dataset for General Use
#' @import data.table
#' @description Tidies dataset for general use by correcting variable types, variable names and
#' removing blanks/NAs in response variable.
#' @param data dataset to be tidied.
#' @param thresh a numeric. Threshold for the maximum unique levels of a numeric variables to
#' be converted as factor type.
#' @param retain_names a logical. Whether to retain original variable names.  
#' @param response a numeric. Response variable to be used in modelling. 
#' @return A data.table.
#' @details This function tidies dataset for general use by: correcting variable types,
#' correcting variable names and removing blanks/NAs in response variable. The function 
#' returns the tidied data as a data.table.
#' 
#' For further data cleaning in preparation for modelling, please see the `data_prep`
#' function which provides more extensive and comprehensive pre-processing. 
#' @rdname tidy_data
#' @export 


tidy_data <- function(data, thresh = 10, retain_names = FALSE, response, quiet = FALSE) {
  
  if (!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  
  # convert characters to factors
  if (sum(sapply(data, is.character)) > 0) {
    col_char <- names(data)[sapply(data, is.character)]
    data[, (col_char) := lapply(.SD, as.factor), .SDcols = col_char]
    if (!quiet) {
      cat("Following variables converted from character to factor type:\n")
      for (i in 1:length(col_char)) {
        cat(paste0(" ", col_char[i], "\n"))
      } 
    }
  }
  
  # convert variables with low number of unique values to factor type
  fun <- function(x) { length(unique(x)) < thresh & !is.factor(x) }
  if (sum(sapply(data, fun)) > 0) {
    col_char <- names(data)[sapply(data, fun)]
    data[, (col_char) := lapply(.SD, as.factor), .SDcols = col_char]
    if (!quiet) {
      cat(paste0("Following variables converted to factor type since has less than ", 
                 thresh,
                 " unique values:\n"))
      for (i in 1:length(col_char)) {
        cat(paste0(" ", col_char[i], "\n"))
      } 
    }
  }
  
  if (!retain_names) {
    # fix column names
    names(data) <- gsub(x = make.names(names(data)), pattern = "\\.", replacement = "_")
  }
  
  # filter rows with blanks or NAs in response
  tmp <- sum(is.na(data[, response, with = FALSE])) + sum(data[, response, with = FALSE] == "", na.rm = TRUE)
  if (tmp > 0) {
    data <- data[!is.na(get(response)) & !get(response) == ""]
    if (!quiet) {
      message(paste0(tmp, " rows were removed due to missing values in ", response, "."))
    }
  }
  
  return(data)
  
}
