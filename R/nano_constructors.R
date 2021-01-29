#' @title Creating Nano Object 
#' @description constructors for `nano` object
#' @param x list of lists with elements containing the below parameters.
#' @param grid list of grids created by `h2o`.
#' @param model list of models created by `h2o`, belonging to their respective grids.
#' @param data list of datasets used to create each model.
#' @param varimp list of datasets containing variable importance for each model.
#' @param pdp list of datasets containing partial dependencies for each model.
#' @param ice list of datasets containing initial conditional expectations for each model.
#' @param interaction list of datasets containing interactions for each model.
#' @param meta list of lists containing meta information for each model such as parameters,
#' hyper-parameters and model type.
#' @param n_model number of created models.
#' @return a `nano` object
#' @details Creates a `nano` objected which consists of a list of list. If no arguments are 
#' supplied, `nano` object is created with 10 elements initialised for each list. If supplying 
#' arguments, must supply arguments for `grid` and `data`. These must be in list format. If the 
#' underlying datasets for each grid are identical, then it is sufficient to only enter `data`
#' as a list of a single dataset. If supplying the above arguments, it is optional to include 
#' 'model', 'varimp', 'pdp', `ice`, 'interaction' and 'meta`. In fact, it is recommended to not
#' provide `meta` since has a strict structure which other functions are dependent on, and will
#' be calculated automatically if not provided. 
#' 
#' If 'model' is not supplied, then by default, model' will be taken as the best model from 'grid'. 
#' If 'varimp', 'pdp', `ice` or `interaction` are not supplied, they will be initialised as NA. 
#' When supplying arguments, extra elements will be initialised so total number of elements for 
#' each list is 10.
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
#'  grid_2 <- h2o.grid(x               = var,
#'                     y               = response,
#'                     training_frame  = train,
#'                     algorithm       = "randomForest",
#'                     hyper_params    = list(ntrees = 3:4),
#'                     nfolds          = 3,
#'                     seed            = 628)
#'
#'  
#'  obj <- create_nano(grid = list(grid_1, grid_2),
#'                     data = list(property_prices), # since underlying dataset is the same 
#'                     ) # since model is not entered, will take best model from grids
#'  }
#' }


#' @rdname nano_constructors
new_nano <- function(x = list(grid        = rep(list(NA)      , 10),
                              model       = rep(list(NA)      , 10),
                              metric      = rep(list(NA)      , 10),
                              data        = rep(data.table(NA), 10),
                              varimp      = rep(list(NA)      , 10),
                              pdp         = rep(list(NA)      , 10),
                              ice         = rep(list(NA)      , 10),
                              interaction = rep(list(NA)      , 10),
                              meta        = rep(list(NA)      , 10),
                              n_model     = as.integer(0)          
)
) {
  stopifnot(is.list(x))
  structure(x, class = "nano")
}


#' @rdname nano_constructors
validate_nano <- function(x) {
  nano <- nano:::new_nano(x)
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
  
  if (values$n_model != 0 & !all(lapply(values$grid, class)[1:values$n_model] == "Grid")) {
    stop(
      "All `grid` values must be Grid class",
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

  if (len(values$ice) != 0 & !all(lapply(values$ice, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `ice` values must be data.table class",
      call. = FALSE
    )
  }
    
  if (len(values$interaction) != 0 & !all(lapply(values$interaction, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `interaction` values must be data.table class",
      call. = FALSE
    )
  }

  if (len(values$meta) != 0 & !all(lapply(values$meta, function(x) class(x)[1])[1:values$n_model] == "list")) {
    stop(
      "All `meta` values must be a list",
      call. = FALSE
    )
  }
  nano
}


#' @rdname nano_constructors
#' @export
create_nano <- function(grid        = rep(list(NA)      , 10),
                        model       = rep(list(NA)      , 10),
                        metric      = rep(list(NA)      , 10),
                        data        = rep(data.table(NA), 10),
                        varimp      = rep(list(NA)      , 10),
                        pdp         = rep(list(NA)      , 10),
                        ice         = rep(list(NA)      , 10),
                        interaction = rep(list(NA)      , 10),
                        meta        = rep(list(NA)      , 10),
                        n_model     = as.integer(length(grid) - sum(sapply(grid, typeof) == "logical"))
) {

  # function to calculate number of non NA elements in a list
  len <- function(list) length(list) - sum(sapply(list, typeof) == "logical")
  
  if (n_model != 0 & !all(grepl("H2O", sapply(model, function(x) as.vector(class(x)))[1:n_model])) & !all(grepl("Model", sapply(model, function(x) as.vector(class(x)))[1:len(model)]))) {
    stop("All `model` values must be a H2O model", 
         call. = FALSE
    )
  }
  
  if (n_model != 0 & !all(lapply(data, function(x) class(x)[1])[1:len(data)] == "data.table")) {
    stop(
      "All `data` values must be data.table class",
      call. = FALSE
    )
  }
  
  # convert each element in grid to Grid object
  if (len(grid) >= 1) {
    for (i in 1:len(grid)) {
      grid[[i]] <- nano:::create_Grid(grid[[i]])
    }
  }
  
  # if model is not entered and grid is entered, take best model from grid by default
  if (len(model) < len(grid)) {
    for (i in (len(model)+1):len(grid)) {
      model[[i]] <- h2o.getModel(grid[[i]]@model_ids[[1]])
    }
  }
  
  # if only 1 dataset specified, assume underlying dataset for all grids are the same
  if (len(data) == 1 & len(grid) > 1) {
    data <- rep(data, len(grid))
  }
  # create data_id column for each dataset if doesn't already exist
  if (len(data) > 0) {
    for (i in 1:len(data)) {
      if (!"data_id" %in% names(data[[i]])) {
        data[[i]][, data_id := "train"]
      }
    }
  }
  
  # calculate metrics for each 
  if (len(model) >= 1) {
    for (i in 1:len(model)) {
      metric[[i]] <- nano:::model_metrics(model[[i]], data[[i]])
    }
  }

  # fill out meta for each model
  if (len(grid) >= 1) {
    for (i in 1:len(grid)) {
      meta[[i]] <- nano:::model_meta(model[[i]], as.h2o(data[[i]]))
    }
  }
  
  # pad each list with 10 elements
  if (length(grid) < 10 | length(model) < 10 | length(data) < 10) {
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
    metric      <- pad(metric     , "list")
    data        <- pad(data       , "data.table")
    varimp      <- pad(varimp     , "list")
    pdp         <- pad(pdp        , "list")
    ice         <- pad(ice        , "list")
    interaction <- pad(interaction, "list")
    meta        <- pad(meta       , "list")
  }
  
  # rename elements of each list 
  change_names <- function (var, name) {
    extra <- 10 - len(var)
    list_names <- c(if (len(var) > 0) {paste0(name, 1:len(var))}, if(extra > 0) {rep("", extra)})
    var <- setNames(var, list_names)
  }
  grid        <- change_names(grid       , "grid_")
  model       <- change_names(model      , "model_")
  metric      <- change_names(metric     , "metric_")
  data        <- change_names(data       , "data_")
  varimp      <- change_names(varimp     , "varimp_")
  pdp         <- change_names(pdp        , "pdp_")
  ice         <- change_names(ice        , "ice_")
  interaction <- change_names(interaction, "interaction_")
  meta        <- change_names(meta       , "meta_")
  
  # convert n_model to integer if not already
  n_model <- as.integer(n_model)
  
  # create nano object
  nano:::validate_nano(list(grid        = grid, 
                            model       = model, 
                            metric      = metric, 
                            data        = data, 
                            varimp      = varimp, 
                            pdp         = pdp, 
                            ice         = ice, 
                            interaction = interaction, 
                            meta        = meta,
                            n_model     = n_model
                            )
                       )
}
create_nano()


