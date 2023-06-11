#' @title Create PDP 
#' @description Creates partial dependency plots (PDPs) from h2o models stored i nano
#' objects.
#' @param model_no the positions of each model in the list of models in the nano object for which
#' the PDP should be calculated. If not entered, the last model is taken by default.
#' @param vars a character vector of variables to create PDPs off. 
#' @param row_index a numeric vector of dataset rows numbers to be used to calculate PDPs. To
#' use entire dataset, set to -1.
#' @param plot a logical specifying whether the variable importance should be plotted.
#' @param save a logical specifying whether the plot should be saved into working directory.
#' @param subdir sub directory of the working directory in which the plot should be saved.
#' @param file_type file type in which the plots should be saved. Can take values `html`, `jpeg`,
#' `png`, `pdf`.  
#' @return nano object with PDPs of specified models calculated. Also returns a 
#' plot if \code{plot = TRUE} and saves each of the plots if \code{save = TRUE}..
#' @details Function first checks if the PDPs of the specified models have already 
#' been calculated (by checking in the list \code{nano$pdp}). If it has not been calculated, 
#' then the required PDPs will be calculated and the relevant slot in \code{nano$pdp}
#' will be filled out. 
#' 
#' If \code{plot = TRUE}, a plot of the PDPs will also be returned. The plot can
#' be saved in a subfolder of the working directory by using the `save` and `subdir` arguments.
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
#'  
#'  # calculate PDP and save plots in working directory
#'  obj <- nano_pdp(nano = obj, model_no = 1:2, vars <- c("lot_size", "income"), 
#'  plot = TRUE, save = TRUE)
#'  
#'  }
#' }
#' @rdname nano_pdp
#' @export 

nano_pdp <- function (nano, model_no = NA, vars, row_index = -1, plot = TRUE,
                      save = FALSE, subdir = NA, file_type = "html") {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!all(is.na(model_no))) {
    if (!is.integer(as.integer(model_no))) {
      stop("`model_no` must be numeric.", 
           call. = FALSE)
    }
    
    if (min(model_no) <= 0) {
      stop("`model_no` must be greater than 0", 
           call. = FALSE)
    }
    
    if (max(model_no) > nano$n_model) {
      stop("`model_no` cannot be greater than number of models in `nano`.", 
           call. = FALSE)
    }
  }
  
  if (missing(vars)) {
    stop("`vars` must be entered, there are no defaults.",
         call.= FALSE)
  }
  
  if (!is.character(vars)) {
    stop("`vars` must be a vector of character.",
         call. = FALSE)
  }
  
  # if model_no not entered, then use last model as default
  if (all(is.na(model_no))) model_no = nano$n_model
  
  for (i in model_no) {
    if (!all(vars %in% nano$model[[i]]@parameters$x)) {
      stop("`vars` must be predictors in each of the specified models.",
           call. = FALSE)
    }
  }
  
  if (plot & length(model_no) > 1) {
    for (i in 1:(length(model_no) - 1)) {
      if (nano$model[[model_no[i]]]@parameters$y != nano$model[[model_no[i + 1]]]@parameters$y) {
        warning("the response variable of each of the specified models are not the same hence plots will not be produced.", 
             call. = FALSE)
      }
    }
  }
  
  if (!(file_type %in% c("html", "jpeg", "png", "pdf"))) {
    stop("Invalid value for `file_type`. Must either be `html`, `jpeg`, `png`, `pdf`.")
  }
  
  
  # check which models do not already have PDP calculated
  models    <- list()
  vars_inc  <- list()
  data      <- list()
  model_inc <- c()
  j <- 1
  for (i in model_no) {
    if (all(is.na(nano$pdp[[i]]))) {
      new_vars <- vars
    } else {
      new_vars <- vars[!vars %in% nano$pdp[[i]]$var]
    }
    if (length(new_vars) > 0) {
      models[[j]]   <- nano$model[[i]]
      vars_inc[[j]] <- new_vars
      data[[j]]     <- nano$data[[i]]
      model_inc     <- c(model_inc, i)
      j <- j + 1
    }
    
    # write message for pdps which have already been created
    vars_prev_inc <- setdiff(vars, new_vars)
    if (length(vars_prev_inc) > 0) {
      message("For model_", model_no[i], ", pdps have already been created for the following variables:")
      for (var in vars_prev_inc) {
        message(" ", var)
      } 
    }
  }
  

  if (length(models) > 0) {
    ## need to calculate PDPs for required models and variables
    # variables which need to be calculated for all models
    vars_multi <- Reduce(intersect, vars_inc)
    # remaining variables for each model
    vars_single <- lapply(vars_inc, function(x) setdiff(x, vars_multi))
    
    
    # calculate pdps for variables common across all models
    if (length(vars_multi) > 0) {
      pdp_multi <- nano::nano_multi_pdp(models, data, vars_multi, row_index)
      # add calculated pdps to nano object
      for (i in 1:length(models)) {
        model_no <- model_inc[i]
        if (all(is.na(nano$pdp[[model_no]]))) {
          nano$pdp[[model_no]]      <- pdp_multi[[i]]
          names(nano$pdp)[model_no] <- paste0("pdp_", model_no)
        } else {
          nano$pdp[[model_no]] <- rbind(nano$pdp[[model_no]], pdp_multi[[i]])
        }
      }
    }
    
    # for each model, calculate pdps for remaining variables
    if (sum(sapply(vars_single, length)) > 0) {
      index            <- lapply(vars_single, length) > 0
      models_single    <- models[index]
      vars_single      <- vars_single[index]
      data_single      <- data[index]
      model_inc_single <- model_inc[index]
      for (i in 1:length(models_single)) {
        pdp_single <- nano::nano_single_pdp(model     = models_single[[i]], 
                                            data      = data_single[[i]], 
                                            vars      = vars_single[[i]],
                                            row_index = row_index)
        model_no <- model_inc_single[i]
        # add newly calculated pdps to nano object
        if (all(is.na(nano$pdp[[model_no]]))) {
          nano$pdp[[model_no]]      <- pdp_single
          names(nano$pdp)[model_no] <- paste0("pdp_", model_no)
        } else {
          nano$pdp[[model_no]] <- rbind(nano$pdp[[model_no]], pdp_single)
        }
      }
    }
  }
  
  # create plots
  if (plot) {
    pdp <- data.table::data.table()
    model_ids <- c()
    for (i in 1:length(model_no)) {
      # combined data for required pdps
      pdp <- rbind(pdp, nano$pdp[[model_no[i]]])
      # extract model ids
      model_ids <- c(model_ids, rep(nano$model[[model_no[i]]]@model_id, nrow(nano$pdp[[model_no[i]]])))
    }
    # keep required columns
    pdp <- pdp[, .(var_band, mean_response, var)]
    pdp[, model_id := model_ids]
    
    # function to create plots of pdps
    pdp_plot <- function(pdp, vars) {
      # initialise list to hold plots
      pdps <- rep(list(NA), length(vars))
      for (var_name in vars) { 
        # subset for required variable
        dat <- pdp[var == var_name]
        # create line plot by model
        fig <- nano:::quiet(plotly::plot_ly(data = dat,
                                            x = ~var_band, 
                                            y = ~mean_response, 
                                            split = ~model_id,
                                            type = "scatter", 
                                            color = ~model_id, 
                                            legendgroup = ~model_id,
                                            mode = "lines+markers",
                                            showlegend = TRUE) %>% 
                                      layout(xaxis = list(title = var_name,
                                                          hoverformat = ",.2s"),
                                             yaxis = list(hoverformat = ",.2s"),
                                             legend = list(orientation = "h")))
        pdps[[var_name]] <- fig
      }
      return(pdps)
    }
    
    # create plots of pdps
    pdp_plots <- pdp_plot(pdp, unique(pdp$var))
    # save plots in nano object
    nano$pdp[["plots"]] <- pdp_plots
    
    # export plots as separate files for each variable
    if (save) {
      folder_name <- paste0("PDP Plots for Models ", paste(model_no, collapse = "-"))
      # export as html
      if (file_type == "html") {
        if (is.na(sub_dir)) {
          # create folder to save plots if doesn't already exist
          dir <- paste0(".\\", folder_name)
          if(!dir.exists(dir)){
            dir.create(dir)
          }
          for (var in vars) {
            htmlwidgets::saveWidget(pdp_plots[[var]], file = paste0(dir, "\\", var, "_pdp_plot.html"))
          }
        } else {
          dir <- paste0(".\\", sub_dir, "\\", folder_name)
          if(!dir.exists(dir)){
            dir.create(dir)
          }
          for (var in vars) {
            htmlwidgets::saveWidget(pdp_plots[[var]], file = paste0(dir, "\\", var, "_pdp_plot.html"))
          }
        }
      } else { # export as static image
        if (is.na(sub_dir)) {
          dir <- paste0(".\\", folder_name)
          if(!dir.exists(dir)){
            dir.create(dir)
          }
          for (var in vars) {
            plotly::export(pdp_plots[[var]], file= paste0(dir, "\\", var, "_pdp_plot.", file_type))
          }
        } else {
          dir <- paste0(".\\", sub_dir, "\\", folder_name)
          if(!dir.exists(dir)){
            dir.create(dir)
          }
          for (var in vars) {
            plotly::export(pdp_plots[[var]], file = paste0(dir, "\\", var, "_pdp_plot.", file_type))
          }
        }
      }
    }
  }
  
  return(nano)
}  