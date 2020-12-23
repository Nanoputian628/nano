#' @title Impute Missing Values in Dataset
#' @description Imputes blanks and NAs in dataset by various methods.
#' @importFrom mice mice
#' @param data dataset to be analysed.
#' @param method method of imputation. Possible methods are "mice" or "mean/mode".
#' @param mice_method the method of imputation used by the `mice` package. Only required if `method` = "mice".
#' @param pred_ignore columns in dataset to be not used in data imputation process. Only required if `method` = "mice".
#' @param impute_ignore columns in dataset to be not imputed.
#' @param seed seed for `set.seed`.  
#' @return imputed data.table.
#' @details Imputes missing values (blanks or NAs) in dataset. There are two possible methods of imputation. If `method`
#' = "mean/mode", then all missing values in numeric variables will be imputed with its mean and all missing values in 
#' factor variables will be imputed with its mode. 
#' 
#' If `method` = "mice", then imputation will be performed using the `mice` package. The imputation method performed by 
#' `mice` can be selected using the `mice_method` parameter. A single method can be specified, which will be applied to 
#' all eligible variables in the dataset, or a vector of methods for each variable (including variables listed in 
#' `impute_ignore`) must be specified. If the `mice_method` parameter is not used, then the default methods selected by 
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
 

impute <- function(data, method = "mice", mice_method = NULL, pred_ignore = c(), impute_ignore = c(), seed = 628) {
  
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

  if (!is.integer(as.integer(seed))) {
    stop("`seed` must be an integer.",
         call. = FALSE)
  }
  
  is_missing <- function(x) {
    is.na(x) | x == ""
  }
  
  setDT(data)
  missing_var <- names(data)[colSums(is_missing(data))>0]

  if (method == "mean/mode") {
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    impute_mm <- function(x) {
      if (is.factor(x)) {
        replace(x, is_missing(x), Mode(x[!is_missing(x)]))
      } else if (is.numeric(x)) {
        replace(x, is_missing(x), mean(x[!is_missing(x)]))
      } else {
        stop("Columns to be imputed using `mean/mode` method must either be factor or numeric type.",
             call. = FALSE)
      }
    }
    data[, (missing_var) := lapply(.SD, impute_mm), .SDcols = missing_var]
    out <- copy(data)
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
    init = quiet(mice::mice(data, maxit=0))
    meth = init$method
    predM = init$predictorMatrix
    if (!is.null(mice_method)) {
      if (length(mice_method) == 1) {
        meth <- rep(mice_method, ncol(data))
      } else {
        meth <- mice_method
      }
    }
    meth[names(meth) %in% impute_ignore] <- ""
    predM[, pred_ignore] <- 0
    
    # perform actual imputation
    set.seed(seed)
    imputed <- quiet(mice::mice(data, method = meth, predictorMatrix = predM, m = 1))
    data <- complete(imputed)
    setDT(data)
    if (sum(is_missing(data[, setdiff(names(data), impute_ignore), with = FALSE])> 0)) {
      message("There are still some missing values in imputed dataset. \nTo remove all missing values, run `impute` again with the `mean/mode` method.")
    }
    out <- list(imputed_data = data, imputation_sumry = imputed)
  }
  return(out)
}

