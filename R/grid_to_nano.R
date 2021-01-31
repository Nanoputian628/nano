#' @title Convert Grid to Nano Object  
#' @description Converts a grid in a nano object to a separate nano object. 
#' @param nano nano object containing the grid to be converted to a new nano object.
#' @param grid_no a numeric. Position of grid to be converted to a nano object.
#' @return a `nano` object
#' @details Converts a specified grid in a nano object to a new nano object where a new slot 
#' is created for each model in the specified grid. The purpose of this function is to be used
#' when performing hyper-parameter tuning (using the `nano_grid` function). When hyper-
#' parameter tunning is performed, a grid of models is created, however, only a single model 
#' will be stored in the nano object. The diagnostic functions (e.g. `nano_pdp`, `nano_ice`, 
#' etc) are only able to analyse models in different slots of the nano object. Hence,
#' to compare the models in the grid with each other, this function should be run to create
#' a new nano object with a slot of each model. Then the usual diagnostic functions can be 
#' run to compare the models. Later, if you wish to select a different model from the grid 
#' (by default, the model with the best metric will be stored in the nano object), then the
#' `switch_model` function can be run. 
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
#'  # set the response and predictors
#'  response <- "sale_price"
#'  var <- setdiff(colnames(property_prices), response)
#'  
#'  # hyper-parameter tuning
#'  nano <- nano_grid(data         = property_prices, 
#'                    response     = response, 
#'                    algo         = "drf",
#'                    hyper_params = list(ntrees = 1:3))
#'                    
#'  # convert created grid to new nano object with 3 slots
#'  nano_1 <- grid_to_nano(nano, 1)
#'  
#'  }
#' }
#' @rdname grid_to_nano
#' @export


grid_to_nano <- function(nano, grid_no = nano$n_model) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (grid_no < 1 | grid_no > nano$n_model | grid_no %% 1 != 0) {
    stop(paste0("`grid_no` must be an integer between 1 and ", nano$n_model),
         call. = FALSE)
  }
  
  num  <- length(nano$grid[[grid_no]]@model_ids)
  grid <- rep(list(nano$grid[[grid_no]]), num)
  data <- list(nano$data[[grid_no]])
  
  # produce list of models in chosen grid
  model <- rep(list(NA), num)
  for (i in length(model)) {
    model[[i]] <- h2o::h2o.getModel(nano$grid[[grid_no]]@model_ids[[i]])
  }
  
  # copy calculated diagnostics from original nano object to new nano object
  varimp      <- rep(list(nano$grid[[grid_no]]), num)
  pdp         <- rep(list(nano$grid[[grid_no]]), num)
  ice         <- rep(list(nano$grid[[grid_no]]), num)
  interaction <- rep(list(nano$grid[[grid_no]]), num)
  
  index <- which(nano$grid[[grid_no]]@model_ids == nano$model[[grid_no]]@model_id)
  varimp[[index]]      <- nano$varimp[[grid_no]]
  pdp[[index]]         <- nano$pdp[[grid_no]]
  ice[[index]]         <- nano$ice[[grid_no]]
  interaction[[index]] <- nano$interaction[[grid_no]]
   
    
  # create nano object
  nano <- nano::create_nano(grid        = grid,
                            model       = model, 
                            data        = data,
                            varimp      = varimp,
                            pdp         = pdp,
                            ice         = ice,
                            interaction = interaction)
  return(nano)
}

