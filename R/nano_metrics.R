#' @title Calculate Metrics of Models  
#' @description Calculate various metrics from models in `nano` object. 
#' @param nano nano object to containing models to calculate metrics from. 
#' @param model_no a numeric. Vector of model numbers to calculate metrics from.
#' @param metrics a character. Vector of metrics to be calculated.
#' @param data_type a character. From which data source to calculate metrics from. Can take values: "train", "test", "cv" or "holdout".
#' @param plot a logical. Whether to output plots. 
#' @return a data.table of the metrics for the chosen models.
#' @details This function 
#' @examples 
#' \dontrun{
#' }
#' @rdname nano_metrics
#' @export 


nano_metrics <- function(nano, model_no = nano$n_model, data_type = "train", metrics, plot = FALSE) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!is.numeric(model_no)) {
    stop("`mode_no` must be numeric.",
         call. = FALSE)
  }
  
  if (!missing(metrics)) {
    if (!is.character(metrics)) {
      stop("`metrics` must be a character.",
           call. = FALSE)
    }
  }
  
  if (!data_type %in% c("train", "test", "cv", "holdout")) {
    stop("`data_type` must either be: 'train', 'test', 'cv' or 'holdout'.",
         call. = FALSE)
  }
  
  if (!is.logical(plot)) {
    stop("`plot` must be a logical.",
         call. = FALSE)
  }
  
  # if missing metrics, default to all possible metrics
  if (missing(metrics)) {
    metrics <- names(nano$metric[[model_no[1]]][[paste0(data_type, "_metrics")]])
    if (length(model_no) > 1) {
      for (i in 2:length(model_no)) {
        metrics <- intersect(metrics, names(nano$metric[[model_no[i]]][[paste0(data_type, "_metrics")]]))
      }
    }
  }
  
  met <- data.table::data.table()
  for (i in 1:length(model_no)) {
    metric <- data.table::as.data.table(nano$metric[[model_no[i]]][[paste0(data_type, "_metrics")]][metrics])
    metric[, model_id := nano$model[[model_no[i]]]@model_id]
    met <- rbind(met, metric)
  }
  data.table::setcolorder(met, c("model_id", metrics))

  return(met)
  
  
  ### PLOTS - NEED TO DO
  
}