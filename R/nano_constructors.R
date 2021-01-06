#' @title Creating Nano Object 
#' @description constructors for `nano` object
#' @param x list of lists with elements containing the below parameters.
#' @param grid list of grids created by `h2o`.
#' @param model list of models created by `h2o`, belonging to their respective grids.
#' @param data list of datasets used to create each model.
#' @param varimp list of datasets containing variable importance for each model.
#' @param pdp list of datasets containing partial dependencies for each model.
#' @param interaction list of datasets containing interactions for each model.
#' @param n_model number of created models.
#' @return a `nano` object
#' @details Creates a `nano` objected which consists of a list of list. If no arguments are 
#' supplied, `nano` object is created with 10 elements initialised for each list. If supplying 
#' arguments, must supply arguments for `grid`, `model` and `data`. These must be in list format 
#' and must have the same length. If supplying the above arguments, it is optional to include 
#' varimp, pdp and interaction. If not supplied, they will be initialised as NA. When supplying 
#' arguments, extra elements will be initialised so total number of elements for each list is 10.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  obj <- create_nano()
#'  }
#' }


#' @rdname nano_constructors
new_nano <- function(x = list(grid        = rep(list(NA)      , 10),
                              model       = rep(list(NA)      , 10),
                              data        = rep(data.table(NA), 10),
                              varimp      = rep(list(NA)      , 10),
                              pdp         = rep(list(NA)      , 10),
                              interaction = rep(list(NA)      , 10),
                              n_model     = as.integer(0)
)
) {
  stopifnot(is.list(x))
  structure(x, class = "nano")
}


#' @rdname nano_constructors
validate_nano <- function(x) {
  nano <- new_nano(x)
  values <- unclass(nano)
  
  # function to calculate number of non NA elements in a list
  len <- function(list) length(list) - sum(sapply(list, typeof) == "logical")
  
  if (!(len(values$grid) == values$n_model)) {
    stop(
      "`n_model` must equal number of build models",
      call. = FALSE
    )
  }
  
  if (!(len(values$grid) == len(values$model) & len(values$grid) == len(values$data))) {
    stop(
      "number of `grids`, `models` and `datasets` must be equal",
      call. = FALSE
    )
  }
  
  if (values$n_model != 0 & !all(lapply(values$grid, class)[1:values$n_model] == "H2OGrid")) {
    stop(
      "All `grid` values must be H2OGrid class",
      call. = FALSE
    )
  }
  
  if (values$n_model != 0 & !all(grepl("H2O", sapply(values$model, function(x) as.vector(class(x)))[1:values$n_model])) & !all(grepl("Model", sapply(values$model, function(x) as.vector(class(x)))[1:values$n_model]))) {
    stop(
      "All `model` values must be a H2O model",
      call. = FALSE
    )
  }
  
  if (len(values$model) > 0) {
    for (i in 1:len(values$model)) {
      if (!values$model[[i]]@model_id %in% values$grid[[i]]@model_ids) {
        stop(
          "`model` is not in its corresponding grid.",
          call. = FALSE
        )
      }  
    }    
  }

  if (values$n_model != 0 & !all(lapply(values$data, function(x) class(x)[1])[1:values$n_model] == "data.table")) {
    stop(
      "All `data` values must be data.table class",
      call. = FALSE
    )
  }
  
  if (len(values$varimp) != 0 & !all(lapply(values$varimp, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `varimp` values must be data.table class",
      call. = FALSE
    )
  }
  
  if (len(values$pdp) != 0 & !all(lapply(values$pdp, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `pdp` values must be data.table class",
      call. = FALSE
    )
  }
  
  if (len(values$interaction) != 0 & !all(lapply(values$interaction, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `interaction` values must be data.table class",
      call. = FALSE
    )
  }
  
  nano
}


#' @rdname nano_constructors
#' @export
create_nano <- function(grid        = rep(list(NA)      , 10),
                        model       = rep(list(NA)      , 10),
                        data        = rep(data.table(NA), 10),
                        varimp      = rep(list(NA)      , 10),
                        pdp         = rep(list(NA)      , 10),
                        interaction = rep(list(NA)      , 10),
                        n_model     = as.integer(length(grid) - sum(sapply(grid, typeof) == "logical"))
) {
  
  
  # function to calculate number of non NA elements in a list
  len <- function(list) length(list) - sum(sapply(list, typeof) == "logical")

  # if model is not entered and grid is entered, take best model from grid by default
  if (missing(model) & len(grid) > 0) {
    for (i in 1:len(grid)) {
      model[[i]] <- h2o.getModel(grid[[i]]@model_ids[[1]])
    }
  }
  
  # pad each list with 10 elements
  if (length(grid) < 10 & length(model) < 10 & length(data) < 10) {
    pad <- function (var, type) {
      extra <- 10 - length(var)
      if (type == "list") {
        var <- c(var, rep(list(NA), extra))
      }
      else {
        var <- c(var, rep(data.table(NA), extra))
      }
    }
    grid        <- pad(grid       , "list")
    model       <- pad(model      , "list")
    data        <- pad(data       , "data.table")
    varimp      <- pad(varimp     , "list")
    pdp         <- pad(pdp        , "list")
    interaction <- pad(interaction, "list")
  }
  
  # rename elements of each list 
  change_names <- function (var, name) {
    extra <- 10 - len(var)
    list_names <- c(if (len(var) > 0) {paste0(name, 1:len(var))}, if(extra > 0) {rep("", extra)})
    var <- setNames(var, list_names)
  }
  grid        <- change_names(grid       , "grid_")
  model       <- change_names(model      , "model_")
  data        <- change_names(data       , "data_")
  varimp      <- change_names(varimp     , "varimp_")
  pdp         <- change_names(pdp        , "pdp_")
  interaction <- change_names(interaction, "interaction_")
  
  # convert n_model to integer if not already
  n_model <- as.integer(n_model)
  
  # create nano object
  validate_nano(list(grid        = grid, 
                     model       = model, 
                     data        = data, 
                     varimp      = varimp, 
                     pdp         = pdp, 
                     interaction = interaction, 
                     n_model     = n_model
                     )
                )
}


