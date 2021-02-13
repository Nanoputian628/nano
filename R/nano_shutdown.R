#' @title Shutdown to H2O Cluster
#' @description Shutdown connection to a H2O cluster.
#' @param prompt a logical specifying whether to prompt the user before shutting down connection 
#' to H2O cluster. 
#' @details This function is a wrapper for the `h2o.shutdown` function from the `H2O` package. 
#' Note, running this function will removal all H2O grids and models built during the session 
#' (hence will invalidate all `nano` objects). Save all `nano` objects first by using the 
#' `nano_save` function before running this function. For more details, see the manual for 
#' `h2o.shutdown`. 
#' @rdname nano_shutdown
#' @export 

nano_shutdown <- function(prompt = TRUE) {
  nano:::quiet(h2o.shutdown(prompt = prompt))
}