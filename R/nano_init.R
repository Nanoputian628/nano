#' @title Connect to H2O Cluster
#' @description Starts and connect to a H2O cluster.
#' @param min_mem_size a character string specifying the minimum size, in bytes, of the memory
#' allocation pool in H2O. 
#' @param max_mem_size a character string specifying the maximum size, in bytes, of the memory
#' allocation pool in H2O. 
#' @details This function is a wrapper for the `h2o.init` function from the `H2O` package. This 
#' function is used to start and connect to a H2O cluster. A connection to a H2O cluster must be
#' established before any modelling. For more details, see the manual for `h2o.init`. 
#' @rdname nano_init
#' @export 

nano_init <- function(min_mem_size = NULL, max_mem_size = NULL) {
  nano:::quiet(h2o.init(min_mem_size = min_mem_size,
                        max_mem_size = max_mem_size))
}