#' @title Switch Model from Grid in Nano Object 
#' @description Switches a model with another model in its associated grid.
#' @param nano `nano` objected created by \code{create_nano()}.
#' @param model_id model_id of the new model to be switched to
#' @param model_no position in the list of models of the model to be replaced.
#' @param varimp_eval calculate variable importance for the new model.
#' @param pdp_eval calculate partial dependecies for the new model. If \code{TRUE}, then will calculate the partial dependencies for each 
#' of the variables the partial dependecies were calculated for the original model. If the original model did not have any partial
#' dependecies, then partial dependecies will be calculated for each variable.
#' @param interaction_eval calcualates the interactions for the new model. Follows same logic as `pdp_eval`.  
#' @return a `nano` object with the original model replaced by the new model specified by the `model_id`.
#' @details Switches a model in a `nano` object with another model in its associated grid. By default, when a new grid is produced, the 
#' model with the best metric (chosen by the user) is set as the associated model for that grid. Hence, the user may call this function
#' to select a different model from the built grid instead. The `model_no` is the position in the list of models of the model to be 
#' replaced. The `model_id` is the model_id of the new model which is to replace the original model. The new model must exist in the 
#' associated grid of the original model.   
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  # switches the second model in the list nano$model with the model with model_id "grid_2_model_3"
#'  nano <- switch_model(nano, 2, "grid_2_model_3")
#'  }
#' }
#' @rdname switch_model
#' @export 


switch_model <- function(nano, model_id, model_no, varimp_eval = FALSE, pdp_eval = FALSE, interaction_eval = FALSE) {
  
  if (class(nano) != "nano") {
    stop(
      "`nano` must be a 'nano' object.",
      call. = FALSE
    )
  }
  
  if (!is.character(model_id)) {
    stop(
      "`model_id` must be character type.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(model_no)) {
    stop(
      "`model_no` must be numeric type.",
      call. = FALSE      
    )
  }
  
  if (model_no > nano$n_model) {
    stop(
      "`model_no` is greater than number of models in object.",
      call. = FALSE
    )
  }
  
  if (!model_id %in% nano$grid[[model_no]]@model_ids) {
    stop(
      "`model_id` is not in the selected grid.",
      call. = FALSE
    )
  } 
  
  # extract new model 
  model <- h2o.getModel(model_id)
  # extract data associated with the model 
  data <- nano$data[[model_no]]
  
  # replace current model with new model
  nano$model[[model_no]] <- model
  
  # recalculate varimp with new model
  if (varimp_eval) {
    nano$varimp[[model_no]] <- as.data.table(h2o.varimp(model))
    names(nano$varimp)[model_no] <- paste0("varimp_", model_no)
  }  
  
  ## NEED TO REPLACE H2O.PD_PLOT FUNCTION WITH USER DEFINED FUNCTION
  # recalculate pdp with new model
  # if (pdp_eval) {
  #   vars <- names(nano$pdp)
  #   nano$pdp[[model_no]] <- lapply(vars, function(x) h2o.pd_plot(model, newdata = data, column = x))
  #   names(nano$pdp[[model_no]]) <- vars
  #   names(nano$pdp)[model_no] <- paste0("pdp_", model_no)
  # }
  
  nano
}