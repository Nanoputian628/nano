#' @title Delete Grid from Nano Object 
#' @description Deletes a specified grid from a `nano` objetct.
#' @param nano `nano` objected created by \code{create_nano()}.
#' @param model_no position of the grid to be deleted.
#' @return a `nano` object with the specified grid deleted.
#' @details Deletes a specified grid from a `nano` object. When the grid is deleted, all elements associated with the grid will be
#' deleted as well. I.e. all the `model_no`th elements in each of the lists will be deleted. If deleting a grid which is not the 
#' last one, then subsequent elements are moved up the list by one and renamed appropriately. E.g. if there are three grids and the
#' second grid is deleted, then the third grid will move to the second position and will be renamed from "grid_3" to "grid_2".  
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  nano <- delete_grid(nano, 1)
#'  }
#' }
#' @rdname delete_grid
#' @export 


delete_grid <- function(nano, model_no) {
  
  if (class(nano) != "nano") {
    stop(
      "`nano` must be a 'nano' object.",
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
  
  len_cur <- length(nano$grid)
  
  # rename and reorder elements
  rename_list <- function(name) {
    if (model_no < nano$n_model) {
      names(nano[[name]])[(model_no + 1):nano$n_model] <- names(nano[[name]])[model_no:(nano$n_model - 1)]
    }
    # move deleted element to the end of the list
    nano[[name]][model_no] <- NULL
    nano[[name]][[len_cur]] <- NA
    nano
  }
  for (name in setdiff(names(nano), "n_model")) {
    nano <- rename_list(name)
  }
  
  nano$n_model <- nano$n_model - 1
  nano
}