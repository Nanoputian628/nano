#' @title Impute Missing Values in Dataset
#' @description Imputes blanks and NAs in dataset by various methods.
#' @importFrom mice mice
#' @param data dataset to be analysed.
#' @param method method of imputation. Possible methods are "mice" or "mean/mode".
#' @param mice_method the method of imputation used by the `mice` package. Only required if 
#' `method` = "mice".
#' @param pred_ignore columns in dataset to be not used in data imputation process. Only required if `method` = "mice".
#' @param impute_ignore columns in dataset to be not imputed.
#' @param pred_matrix optional pre-defined prediction matrix to derive the imputed values when 
#' using the "mice" method.
#' @param impute_outlier a numeric where values which are `impute_outlier` standard deviations 
#' away from the mean will be imputed. By default, set to \code{Inf}, hence no outliers are imputed. 
#' @param seed seed for `set.seed`.  
#' @return imputed data.table.
#' @details Imputes missing values (blanks or NAs) in dataset. There are two possible methods of imputation. If `method`
#' = "mean/mode", then all missing values in numeric variables will be imputed with its mean and all missing values in 
#' factor variables will be imputed with its mode. 
#' 
#' If `method` = "mice", then imputation will be performed using the `mice` package. The imputation method performed by 
#' `mice` can be selected using the `mice_method` parameter. A single method can be specified, which will be applied to 
#' all eligible variables in the dataset, or a vector of methods for each variable (including variables listed in 
#' `impute_ignore`) can be specified. If the `mice_method` parameter is not used, then the default methods selected by 
#' `mice` will be used. The `pred_ignore` parameter is only applicable when using the "mice" method. This specifies 
#' which variables should not be used in the data imputation process.   
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  impute(property_prices)
#'  }
#' }
#' @rdname impute
#' @export 
 

impute <- function(data, method = "mice", mice_method = NULL, pred_ignore = c(), impute_ignore = c(), pred_matrix = NULL, impute_outlier = Inf, seed = 628) {
  
  if (!("data.frame" %in% class(data))) {
    stop("`data` must be a dataset.", 
         call. = FALSE)
  }
  
  if (!(method %in% c("mice", "mean/mode"))) {
    stop("'method` must either be `mice` or `mean/mode`.", 
         call. = FALSE)
  }

  if (!(is.null(mice_method) | is.character(mice_method))) {
    stop("`mice_method` must be a character vector.", 
         call. = FALSE)
  }
  
  if (!is.null(mice_method) & (length(mice_method) != 1 | length(mice_method) != ncol(data))) {
    stop("`mice_method` must either be a vector of length 1 or length equal to number of columns in `data`. If specifying the full vector, ensure to include blanks for columns which are not to imputed.",
         call. = FALSE)
  }
  
  if (!(all(pred_ignore %in% names(data)))) {
    stop("'pred_ignore` must be column names in `data`.", 
         call. = FALSE)
  }
  
  if (!(all(impute_ignore %in% names(data)))) {
    stop("'impute_ignore` must be column names in `data`.", 
         call. = FALSE)
  }

  if (!is.numeric(impute_outlier)) {
    stop("`impute_outlier` must be numeric.",
         call. = FALSE)
  }
  
  if (length(impute_outlier) != 1 & length(impute_outlier) != ncol(data)) {
    stop("`impute_outlier must either have length equal to 1 or to the number of columns in `data`.",
         call. = FALSE)
  }
  
  if (!is.integer(as.integer(seed))) {
    stop("`seed` must be an integer.",
         call. = FALSE)
  }
  
  if (!is.null(pred_matrix) & !"matrix" %in% class(pred_matrix)) {
    stop("`pred_matrix` must be a matrix.", 
         call. = FALSE)
  }
  
  if (!all(colnames(pred_matrix) %in% names(data))) {
    stop("column names of `pred_matrix` must be in `data`.", 
         call. = FALSE)
  }
  
  if (!all(colnames(pred_matrix) == rownames(pred_matrix))) {
    stop("column names and row names of `pred_matrix` must be the same.",
         call. = FALSE)
  }
  
  setDT(data)
  data[data == ""] <- NA
  # replace outliers with NA
  outlier_detect <- function(x) {
    replace(x, 
            abs(x - mean(x, na.rm = TRUE)) > impute_outlier * sd(x, na.rm = TRUE),
            NA)
  }
  impute_var_num <- names(data)[sapply(data, is.numeric)
                                ][!(names(data)[sapply(data, is.numeric)] %in% impute_ignore)]
  data[, (impute_var_num) := lapply(.SD, outlier_detect), .SDcols = impute_var_num]

  if (method == "mean/mode") {
    missing_var <- names(data)[colSums(is.na(data))>0][
      !(names(data)[colSums(is.na(data))>0] %in% impute_ignore)]
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    impute_mm <- function(x) {
      if (is.factor(x)) {
        Mode(x[!is.na(x)])
      } else if (is.numeric(x)) {
        mean(x[!is.na(x)])
      } else {
        stop("Columns to be imputed using `mean/mode` method must either be factor or numeric type.",
             call. = FALSE)
      }
    }
    impute_replace <- function(x) {
      replace(x, is.na(x), impute_mm(x[!is.na(x)]))
    }
    data[, (missing_var) := lapply(.SD, impute_replace), .SDcols = missing_var]
    out = list(imputed_data = data, 
               imputation_smry = list(vars = missing_var, imputed_value = sapply(data[, missing_var, with = FALSE], impute_mm)))
  } else {
    if (!is.null(mice_method)) {
      if (length(mice_method) == 1) {
        meth <- rep(mice_method, ncol(data))
      } else {
        meth <- mice_method
      }
    } 
    # perform initial imputation to extract default imputation methods and 
    # predictor matrix
    set.seed(seed)
    init = nano:::quiet(mice::mice(data, maxit=0))
    if (is.null(mice_method)) meth = init$method
    if (is.null(pred_matrix)) pred_matrix = init$predictorMatrix
    meth[names(meth) %in% impute_ignore] <- ""
    pred_matrix[, pred_ignore] <- 0
    
    # perform actual imputation
    set.seed(seed)
    imputed <- nano:::quiet(mice::mice(data, method = meth, predictorMatrix = pred_matrix, m = 1))
    data <- nano:::quiet(mice::complete(imputed))
    setDT(data)
    if (sum(is.na(data[, setdiff(names(data), impute_ignore), with = FALSE])> 0)) {
      cat("The following variables still have missing values after imputation:\n")
      not_impute_var <- names(data)[sapply(data, function(x) sum(is.na(x)) > 0)]
      for (var in not_impute_var) {
        cat(paste0(var, ": ", sum(is.na(data[, var, with = FALSE])), "\n"))
      }
      cat("To remove all missing values, run `impute` again with the `mean/mode` method.\n")
    }
    out <- list(imputed_data = data, imputation_smry = imputed)
  }
  return(out)
}

