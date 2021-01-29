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


nano_metrics <- function(nano, model_no = nano$n_model, metrics, data_type = "train", plot = FALSE) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!is.numeric(model_no)) {
    stop("`mode_no` must be numeric.",
         call. = FALSE)
  }
  
  if (!is.character(metrics)) {
    stop("`metrics` must be a character.",
         call. = FALSE)
  }
  
  if (!data_type %in% c("train", "test", "cv", "holdout")) {
    stop("`data_type` must either be: 'train', 'test', 'cv' or 'holdout'.",
         call. = FALSE)
  }
  
  if (!is.logical(plot)) {
    stop("`plot` must be a logical.",
         call. = FALSE)
  }
  
  met <- data.table::data.table()
  for (i in 1:length(model_no)) {
    metric <- t(data.table::as.data.table(nano$metric[[model_no[i]]][[paste0(data_type, "_metrics")]][metrics]))
    met <- cbind(met, metric)
    names(met)[i] <- nano$model[[i]]@model_id
  }
  met <- cbind(metrics, met)
  
  return(met)
  
  
  ### PLOTS - NEED TO DO
  
}