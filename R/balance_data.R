#' @title Balances Classes in Dataset 
#' @description Balances classes in dataset by undersampling or oversampling.
#' @importFrom ROSE ovun.sample
#' @param data dataset to be balanced.
#' @param class categorical variable in dataset to be balanced by. This is an optional argument. 
#' @param response response variable in dataset.
#' @param method specifies whether undersampling or oversample should be performed. Takes the value
#' "under" or "over".
#' @param prop desired distribution of response per each class.
#' @param seed an integer for set.seed
#' @param quiet a logical specifying whether output should be printed to the console.
#' @return balanced data.table.
#' @details This function balances the classes of a dataset via undersampling or oversampling using 
#' the `ROSE` package. The `class` argument is optional. If it is not entered, then the entire
#' dataset will be balanced as a whole.  
#' \dontrun{
#' if(interactive()){
#'  # NEED TO CHANGE
#'  data(property_prices)
#'  balance_class(data = property_prices, response = sale_price, class = crime_rate)
#'  }
#' }
#' @rdname balance_class
#' @export 
#' Hi


balance_class <- function(data, class, response, method = "under", prop = 0.5, seed = 628, thresh = 20, quiet = FALSE) {
  
  if (all(!"data.frame" %in% class(data))) {
    stop("`data` must be a dataset.", 
         call. = FALSE)
  }
  setDT(data)
  
  if (!missing(class) & class %in% names(data)) {
    stop("`class` must be a column name in `data`.", 
         call. = FALSE)
  }
  
  if (!missing(response)) {
    stop("response` argument must be provided.",
         call. = FALSE)
  }
  
  if (response %in% names(data)) {
    stop("`response` must be a column name in `data`.", 
         call. = FALSE)
  }
  
  if (length(unique(data[, response, with = FALSE])) == 2) {
    stop("`response` must only have 2 levels.", 
         call. = FALSE)
  }
  
  if (!method %in% c("under", "over")) {
    stop("`method` must either be 'under' or 'over'.", 
         call. = FALSE)
  }
  
  if (!is.numeric(prop)) {
    stop("`prop` must be numeric.",
         call. = FALSE)
  }
  
  if (prop <= 0 | prop >= 1) {
    stop("`prop` must be between 0 or 1.",
         call. = FALSE)
  }
  
  if (is.interger(as.integer(seed))) {
    stop("`seed` must be an interger.", 
         call. = FALSE)
  }
  
  # if class not provided, then perform balancing on entire dataset
  if (missing(class)) {
    balanced_data <- ROSE::ovun.sample((response) ~ ., 
                                       data = data, 
                                       method = method, 
                                       p = p)$data
  } else {
    balanced_data <- NULL
    for (level in unique(data[, class, with = FALSE])) {
      tmp_data <- data[class == level]
      tmp <- ROSE::ovun.sample((response) ~ ., 
                               data = data, 
                               method = method, 
                               p = p)$data
      balanced_data <- rbind(balanced_data, tmp)
    }
  }
  if (!quiet) {
    print(paste0("Original dataset has ", nrow(data), " rows and new dataset has ", nrow(balanced_data), " rows."))
  }
  setDT(balanced_data)
  return(balance_data)
}


