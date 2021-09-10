#' @title Calculate Predictions
#' @description Calculates predictions from model stored in a nano object.
#' @param nano a nano object. 
#' @param model_no a numeric. Position of model in `nano` to calculate predictions off.
#' @param data_type a character. Subset of dataset to calculate predictions off. 
#' @param fold a numeric. Fold number to calculate predictions off.
#' @return a data.table with two columns, actual and predicted. 
#' @details `data_type` can take the values: "train", "test", "holdout". However, these values
#' must first exist in the "data_id" column of the relevant dataset. To calculate the predictions
#' of a specific fold (if cross-validation was performed), used the `fold` argument. If the 
#' `fold` argument is specified, then the `data_type` argument will be ignored. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(h2o)
#'  library(nano)
#'  
#'  h2o.init()
#'  
#'  # import dataset
#'  data(property_prices)
#'  
#'  # create model
#'  nano <- nano_automl(data     = data, 
#'                      response = "sale_price")
#'                      
#'  # calculate training predictions
#'  pred <- nano_predict(nano, 1, "train")
#'  
#'  }
#' }
#' @rdname nano_predict
#' @export 


nano_predict <- function(nano, model_no, data_type = "train", fold = NULL) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.",
         call. = FALSE)
  }
  
  if (model_no %% 1 != 0 | model_no < 0 | model_no > nano$n_model) {
    stop("`model_no` must be an integer between 0 and nano$n_model.",
         call. = FALSE)
  }
  
  if (!data_type %in% nano$data[[model_no]]$data_id) {
    stop("`data_type` must be a value in `data_id` column in dataset.",
         call. = FALSE)
  }
  
  if (!is.null(fold)) {
    if (!"fold" %in% names(nano$data[[model_no]])) {
      stop("Must have fold column to specify `fold` argument.",
           call. = FALSE)
    }
  }
  
  if (!is.null(fold)) {
    if (!fold %in% nano$data[[model_no]]$fold) {
      stop("`fold` must be a value in `fold` column in dataset.",
           call. = FALSE)
    }
  }
  
  if (!missing(data_type) & !is.null(fold)) {
    message("Have specified both `data_type` and `fold`. `data_type` argument will be ignored and `fold` argument will be used instead.")
  }
  
  if (!is.null(fold)) {
    actual <- nano$data[[model_no]][fold == fold][[nano$meta[[model_no]]$y]]
    pred <- nano:::quiet(h2o::h2o.predict(nano$model[[model_no]], h2o::as.h2o(nano$data[[model_no]][fold == fold])))
  } else {
    actual <- nano$data[[model_no]][data_id == data_type][[nano$meta[[model_no]]$y]]
    pred <- nano:::quiet(h2o::h2o.predict(nano$model[[model_no]], h2o::as.h2o(nano$data[[model_no]][data_id == data_type])))
  }
  
  pred <- data.table::data.table(actual    = actual,
                                 predicted = data.table::as.data.table(pred)$predict)
  return(pred)
}
