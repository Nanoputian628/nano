#' @title Convert Grid to Nano Object  
#' @description Converts a grid in a nano object to a separate nano object. 
#' @param nano nano object containing the grid to be converted to a new nano object.
#' @param grid_no a numeric. Vector containing positions of grids to be converted to a nano
#'  object.
#' @param n_top_model a numeric. Vector of the n top number of models to select from each grid.
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


grid_to_nano <- function(nano, grids_no = nano$n_model, 
                         n_top_model = length(nano$grid[[grid_no]]@model_ids)) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (min(grids_no) < 1 | max(grids_no) > nano$n_model | any(grids_no %% 1 != 0)) {
    stop(paste0("`grids_no` must be a vector of integers between 1 and ", nano$n_model),
         call. = FALSE)
  }
  
  grid_all        <- list()
  model_all       <- list()
  data_all        <- list()
  varimp_all      <- list()
  pdp_all         <- list()
  ice_all         <- list()
  interaction_all <- list()
  
  if (length(grids_no) > 1 & length(n_top_model) == 1) {
    n_top_model <- rep(n_top_model, length(grids_no))
  }
  
  for (grid_no in grids_no) {
    
    num  <- min(length(nano$grid[[grid_no]]@model_ids), n_top_model[which(grid_no  == grids_no)])
    grid <- rep(list(nano$grid[[grid_no]]), num)
    data <- rep(list(nano$data[[grid_no]]), num)
    
    # produce list of models in chosen grid
    model <- rep(list(NA), num)
    for (i in 1:length(model)) {
      model[[i]] <- h2o::h2o.getModel(nano$grid[[grid_no]]@model_ids[[i]])
    }
    
    # copy calculated diagnostics from original nano object to new nano object
    varimp      <- rep(list(NA), num)
    pdp         <- rep(list(NA), num)
    ice         <- rep(list(NA), num)
    interaction <- rep(list(NA), num)
    
    index <- which(nano$grid[[grid_no]]@model_ids == nano$model[[grid_no]]@model_id)
    varimp[[index]]      <- nano$varimp[[grid_no]]
    pdp[[index]]         <- nano$pdp[[grid_no]]
    ice[[index]]         <- nano$ice[[grid_no]]
    interaction[[index]] <- nano$interaction[[grid_no]]
    
    # combine to overall list
    grid_all        <- c(grid_all, grid)
    model_all       <- c(model_all, model)
    data_all        <- c(data_all, data)
    varimp_all      <- c(varimp_all, varimp)
    pdp_all         <- c(pdp_all, pdp)
    ice_all         <- c(ice_all, ice)
    interaction_all <- c(interaction_all, interaction)
    
  }
    
  # create nano object
  new <- nano::create_nano(grid        = grid_all,
                           model       = model_all, 
                           data        = data_all,
                           varimp      = varimp_all,
                           pdp         = pdp_all,
                           ice         = ice_all,
                           interaction = interaction_all)
  return(new)
}

