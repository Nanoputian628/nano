#' @title Load Nano Object  
#' @description Loads nano object which has been saved using the `nano_save` function. 
#' @param path a character. Path of the directory where the nano object is saved in.
#' @return a nano object. 
#' @details A nano object is able to be saved using the `nano_save` function. The 
#' `nano_load` function is then used to load the data back as a nano object. 
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
#'  # save nano object
#'  nano_save(nano, tempdir())
#'  
#'  # load back nano object
#'  nano_load(tempdir())
#'  
#'  }
#' }
#' @rdname nano_load
#' @export 

nano_load <- function(path) {
  
  # load elements of nano object
  Grid_list <- readRDS(paste0(path, "/Grid_list"))
  model_ids <- readRDS(paste0(path, "/model_ids"))
  nano_res  <- readRDS(paste0(path, "/nano_res"))
  
  # vector of grid_ids
  grid_ids <- c()
  for (i in 1:nano_res$n_model) {
    grid_ids <- c(grid_ids, Grid_list[[i]]@Grid_id)
  }
  
  # load H2O models
  for (i in 1:nano_res$n_model) { 
    grid_path <- paste0(path, "/grid_", i)
    
    # check if models are from H2Ogrid or H2OautoML
    if (any(list.files(grid_path) %in% grid_ids)) {
      h2o::h2o.loadGrid(paste0(grid_path, 
                               "/", 
                               list.files(grid_path)[list.files(grid_path) %in% grid_ids]))
    } else {
      for (file in list.files(grid_path)) {
        h2o::h2o.loadModel(paste0(grid_path, "/", file))
      }
    }
  }
  
  # create list of models
  models <- rep(list(NA), 10)
  for (i in 1:nano_res$n_model) {
    models[[i]] <- h2o::h2o.getModel(model_ids[[i]])
  }
  # rename elements of models
  names(models) <- c(paste0("model_", 1:nano_res$n_model), rep("", 10 - nano_res$n_model))
  
  # reconstruct nano object
  nano <- list(grid = Grid_list,
               model = models)
  
  nano <- c(nano, nano_res)
  class(nano) <- "nano"
  
  return(nano)
}
