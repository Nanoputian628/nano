#' @title Bands Variables in Dataset 
#' @description Creates a custom bands for variables in dataset.
#' @param data dataset to be analysed.
#' @param intervals a list defining the bands for each of the variables.
#' @param buckets a list defining the names of the bands for each of the variables.
#' @param na_bucket a character or a list defining the bucket name for entries with \code{NA}.
#' @param unmatched_bucket a character or a list defining the bucket name for unmatched entries.
#' @param trunc_left a logical specifying whether the band to \code{-Inf} should be created.  
#' @param trunc_right a logical specifying whether the band to \code{Inf} should be created.
#' @param include_left a logical specifying if should include the left or right endpoint for each
#' interval.
#' @return a data.table with the original variables and new banded variables with the suffix "_bnd".
#' @details The `intervals` parameter must be entered as a list with names matching the column names
#' in `data`. The elements of the list can be specified in two ways. 
#' 
#' It can be specified as a vector of non-decreasing numbers (note the same number can be repeated, 
#' this will correspond to a band of a single point). The intervals will then be derived from these 
#' vectors of numbers in combination with the `trunc_left`, `trunc_right` and `include_left` 
#' parameters. For example, the vector \code{c(1, 3, 3, 6)} with the default parameters will produce 
#' the intervals \code{(-Inf, 1), [1, 3), [3, 3), [3, 6), [6, Inf)}. It can also be directly 
#' specified as a character vector of the desired intervals. Note if this option is taken, the 
#' `trunc_left`, `trunc_right` and `include_left` parameters become redundant. 
#' 
#' The `na_bucket` and `unmatched_bucket` parameters can be specified either a single character or
#' as a list of the desired bucket names for each variable. If specified as a single character, 
#' then this will be applied to all variables. If the `buckets` parameter is not specified, then
#' the bucket names will be set equal to the interval names. 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  band_data(data = property_prices,
#'            intervals = list(crime_rate = seq(0.1, 1, 0.1), # example as numeric vector
#'                             income = c("[0,500)", "[500, 1000)") # example as character vector
#'                             )
#'            )                 
#'  }
#' }
#' @rdname band_data
#' @export 


band_data <- function(data, intervals, buckets = NULL, na_bucket, unmatched_bucket, trunc_left = FALSE, trunc_right = FALSE, include_left = TRUE) {
  
  if (all(!"data.frame" %in% class(data))) {
    stop("`data` must be a dataset.", 
         call. = FALSE)
  }
  
  if (!is.list(intervals)) {
    stop("`intervals` must be a list.", 
         call. = FALSE)
  }
  
  if (!(all(names(intervals) %in% names(data))) | is.null(names(intervals))) {
    stop("names of 'intervals` must be column names in `data`.", 
         call. = FALSE)
  }
  
  if (!is.logical(trunc_left)) {
    stop("'trunc_left' must either be TRUE or FALSE,", 
         call. = FALSE)
  }
  
  if (!is.logical(trunc_right)) {
    stop("'trunc_right' must either be TRUE or FALSE,", 
         call. = FALSE)
  }
  
  if (!is.null(buckets) & !is.list(buckets)) {
    stop("`buckets` must be a list.", 
         call. = FALSE) 
  }
  
  if (!is.null(buckets) & (!(all(names(buckets) %in% names(data))) | is.null(names(buckets)))) {
    stop("names of 'buckets` must be column names in `data`.", 
         call. = FALSE)
  }
  
  if (!missing(na_bucket)) {
    if (!((is.character(na_bucket) & length(na_bucket) == 1) | (is.list(na_bucket) & length(na_bucket) == length(intervals)))) {
      stop("'na_bucket` must be either be a character of length 1 or a list with length equal to length of `intervals`.", 
           call. = FALSE)
    }
  }
  
  if (!missing(unmatched_bucket)) {
    if (!((is.character(unmatched_bucket) & length(unmatched_bucket) == 1) | (is.list(unmatched_bucket) & length(unmatched_bucket) == length(intervals)))) {
      stop("'unmatched_bucket` must be either be a character of length 1 or a list with length equal to length of `intervals`.", 
           call. = FALSE)
    }
  }
  
  if (!is.null(buckets) & !setequal(names(intervals), names(buckets))) {
    stop("`intervals` and `buckets` must have the same names.",
         call. = FALSE)
  }
  
  if (!missing(na_bucket)) {
    if (is.list(na_bucket) & !setequal(names(intervals), names(na_bucket))) {
      stop("`intervals` and `na_bucket` must have the same names.",
           call. = FALSE)
    }
  }
  
  if (!missing(unmatched_bucket)) {
    if (is.list(unmatched_bucket) & !setequal(names(intervals), names(unmatched_bucket))) {
      stop("intervals` and `unmatched_bucket` must have the same names.",
           call. = FALSE)
    }
  }  
  
  if (!is.logical(include_left)) {
    stop("'include_left' must either be TRUE or FALSE,", 
         call. = FALSE)
  }
  
  setDT(data)
  
  # band each variable
  for (var in names(intervals)) {
    intervals_var <- intervals[[var]]
    # if numeric, manually create intervals to pass into wafflecut
    if (is.numeric(intervals_var)) {
      # check if intervals_var is non-decreasing
      if (!all(diff(intervals_var) >= 0)) {
        stop("Elements of `intervals` must be non-decreasing.", 
             call. = FALSE)
      }
      bands <- rep(NA, length(intervals_var) - 1)
      # intervals with LHS closed and RHS half-open
      if (include_left) {
        for (i in 1:length(intervals_var) - 1) {
          bands[i] <- paste0("[", intervals_var[i], ",", intervals_var[i + 1], ")")  
        }
        # add intervals for -Inf and Inf if required
        bands <- c(if (!trunc_left) paste0("(-Inf,", intervals_var[1], ")"), 
                   bands, 
                   if (!trunc_right) paste0("[", intervals_var[length(intervals_var)], ",Inf)"))
      }
      # intervals with LHS half-closed and RHS closed
      if (!include_left) {
        for (i in 1:length(intervals_var) - 1) {
          bands[i] <- paste0("(", intervals_var[i], ",", intervals_var[i + 1], "]")  
        }
        # add intervals for -Inf and Inf if required
        bands <- c(if (!trunc_left) paste0("(-Inf,", intervals_var[1], "]"), 
                   bands, 
                   if (!trunc_right) paste0("(", intervals_var[length(intervals_var)], ",Inf)"))    
      }
    } else if (is.character(intervals_var)) {
      bands <- intervals_var
    } else {
      stop("Elements of `intervals` must either be a character or numeric vector.", 
           call. = FALSE)
    }
    if (!is.null(buckets) & length(bands)  != length(buckets[[var]])) {
      stop("number of `buckets` must be equal to the number of desired intervals.",
           call. = FALSE)
    }
    if (!missing(na_bucket)) {
      if (is.list(na_bucket)) na_bucket_var <- na_bucket[[var]] else na_bucket_var <- na_bucket
    }
    if (!missing(unmatched_bucket)) {
      if (is.list(unmatched_bucket)) unmatched_bucket_var <- unmatched_bucket[[var]] else unmatched_bucket_var <- unmatched_bucket
    } 
    # if buckets is not provided, use bands as the buckets
    if (is.null(buckets)) buckets_var <- bands else buckets_var <- buckets[[var]]
    # create bands using fancycut package
    data[, paste0(var, "_bnd") := wafflecut(data[[var]], 
                                            bands, 
                                            buckets_var, 
                                            na.bucket        = na_bucket_var, 
                                            unmatched.bucket = unmatched_bucket_var, 
                                            out.as.factor    = TRUE)]
  }
  return(data)
}

