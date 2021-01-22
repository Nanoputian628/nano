#' @title Creating Grid S4 Object 
#' @description constructors for `Grid` S4 object
#' @param obj A `H2OGrid` or `H2OAutoML` object.
#' @return a `Grid` S4 object.
#' @details Creates `Grid` S4 object from a `H2OGrid` or `H2OAutoML` object. This object
#' is used as the `grid` element in a `nano` object. The `Grid` object holds useful 
#' information about the input object which will be used by other functions. 
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
#'  train <- as.h2o(property_prices)
#'  
#'  # set the response and predictors
#'  response <- "sale_price"
#'  var <- setdiff(colnames(property_prices), response)
#'  
#'  # build grids
#'  grid_1 <- h2o.grid(x               = var,
#'                     y               = response,
#'                     training_frame  = train,
#'                     algorithm       = "randomForest",
#'                     hyper_params    = list(ntrees = 1:2),
#'                     nfolds          = 3,
#'                     seed            = 628)
#'                     
#'  Grid_1 <- create_Grid(grid_1)
#'
#'  }
#' }


#' @rdname Grid_constructors
setClass("Grid", 
         slots = list(Grid_id = "character", 
                      model_ids = "list",
                      summary_table = "data.table",
                      model_type = "character",
                      meta = "list")
         )


#' @rdname Grid_constructors
create_Grid.H2OGrid <- function(obj) {
  Grid <- new("Grid",
              Grid_id = obj@grid_id, 
              model_ids = obj@model_ids, 
              summary_table = as.data.table(obj@summary_table), 
              meta = list(origin_class = "H2OGrid",
                          tune_hyper_params = obj@hyper_names,
                          failed_params = obj@failed_params,
                          failure_details = obj@failure_details,
                          failed_raw_params = obj@failed_raw_params
                          ))
}


#' @rdname Grid_constructors
create_Grid.H2OAutoML <- function(obj) {
  Grid <- new("Grid",
              Grid_id = obj@project_name,
              model_ids = as.list(obj@leaderboard[["model_id"]]),
              summary_table = as.data.table(obj@leaderboard),
              meta = list(origin_class = "H2OAutoML",
                          modelling_steps = obj@modeling_steps
                          ))
}


#' @rdname Grid_constructors
create_Grid.Grid <- function(obj) {
  obj
}


#' @rdname Grid_constructors
create_Grid <- function(obj) {
  UseMethod("create_Grid")
}


setMethod("create_Grid",
          "H2OGrid",
          create_Grid.H2OGrid)

setMethod("create_Grid",
          "H2OAutoML",
          create_Grid.H2OAutoML)

setMethod("create_Grid",
          "Grid",
          create_Grid.Grid)
