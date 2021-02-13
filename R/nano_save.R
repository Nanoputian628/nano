#' @title Save Nano Object  
#' @description Save nano object into a new directory.
#' @param nano a nano object. 
#' @param path a character. Path of the directory to save the nano object. The 
#' directory must be empty prior to saving the nano object.
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
#' @rdname nano_save
#' @export 
 

nano_save <- function(nano, path) {
  
  if (length(list.files(path)) > 0) {
    stop("Target directory must be empty prior to saving nano object.",
         call. = FALSE)
  }
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.",
         call. = FALSE)
  }  
  
  # save Grid objects
  Grid <- nano$grid

  # list to save model_ids in nano$model
  model_ids <- rep(list(NA), nano$n_model)
  
  # remaining parts of nano object to save
  nano_res <- nano[c("metric", "data", "varimp", "pdp", "ice", "interaction", "meta", "n_model")]
    
  # save grid/models
  for (i in 1:nano$n_model) {
    path_grid <- paste0(path, "/grid_", i)
    if (nano$grid[[i]]@meta$origin_class == "H2OAutoML") {
      
      # save each model in autoML
      for (j in 1:length(nano$grid[[i]]@model_ids)) {
        h2o::h2o.saveModel(object = h2o::h2o.getModel(nano$grid[[i]]@model_ids[[j]]),
                           path   = path_grid,
                           force  = TRUE)
      }
    } else {
      h2o::h2o.saveGrid(path_grid, nano$grid[[i]]@Grid_id)
    }
    
    # extract model_ids
    model_ids[[i]] <- nano$model[[i]]@model_id
  }
  
  # save objects
  saveRDS(Grid, paste0(path, "/Grid_list"))
  saveRDS(model_ids, paste0(path, "/model_ids"))
  saveRDS(nano_res, paste0(path, "/nano_res"))
  
}
