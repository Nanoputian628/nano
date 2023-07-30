#' @title Plot actual vs predicted from fitted model 
#' @description Predict on data from fitted model and plots the actual vs predicted.
#' @param nano a nano object containing the fitted models. 
#' @param data a list of datasets. If the underlying dataset is the same for each model, can
#' only input a list with a single element.
#' @param model_no the positions of each model in the list of models in the nano object for which
#' the PDP should be calculated. If not entered, the last model is taken by default.
#' @param train_test a character. Variable in `data` which contains split for training, 
#' testing and holdout datasets (optional). Can only have the values: "train", "test", 
#' "holdout".
#' @param group a character variable in `data` which the plot is to be grouped by.
#' @param size a character variable in `data` which determines the size of each point in the plot.
#' The `size` parameter is only used if the `group` parameter has been specified.
#' @param save a logical specifying whether to save the output to the nano object (if \code{save = TRUE})
#' otherwise output as separate object..
#' @return if \code{save = TRUE} then returns nano object with the actual vs predicted for the specified models.
#' If \code{save = FALSE} then returns a list with the actual vs predicted for the specified models.
#' @details Functions checks whether the data contains the `train_test` column. If it does then the
#' actual vs predicted is calculated for each split specified in the `train-test` column. Otherwise, the actual 
#' vs predict is calculated based on the total data. 
#' If the plot is desired to be performed on a subset of the data (e.g. to see performance of the model on
#' a specific part of the data) then the `data` argument can be used to supply the data subseted
#' in the desired manner. If the `data` argument is not used, then by default the data used to 
#' train the model is used by the function.
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
#'  # score on both models
#'  obj <- nano_residuals(nano = obj, model_no = 1:2, save = TRUE)
#'  }
#' }
#' @rdname nano_residuals
#' @export




nano_residuals <- function (nano, data = NA, model_no = NA, train_test = "data_id", 
                            group = NA, size = NA, save = TRUE) {
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
  
  # if model_no not entered, then use last model as default
  if (all(is.na(model_no))) model_no <- nano$n_model
  
  if (class(save) != "logical") {
    stop("`save` must be a logical.", 
         call. = FALSE)
  }
  
  # convert data to list of datasets if required
  if ("data.frame" %in% class(data)) {
    data <- rep(list(data), length(model_no)) 
  }
  
  # if choosing not to save to nano object, save outputs to a separate list
  if (!save) {
    final_out <- list()
  }
  
  for (i in 1:length(model_no)) {
    # obtain required model data
    response <- nano$meta[[model_no[i]]]$y
    mod <- nano$model[[model_no[i]]]
    if (all(is.na(data))) {dat <- nano$data[[model_no[i]]]} else {dat <- data[[i]]}
    dat[["response"]] <- dat[[response]] 
      
    # checks on group and size parameters
    if (!(group %in% colnames(dat))) {
      stop("`group` must be a column name in data.",
           call. = FALSE)
    }
    if (!(size %in% colnames(dat))) {
      stop("`size` must be a column name in data.",
           call. = FALSE)
    }
      
    # if data split by train/test/holdout, perform calculation for each subset
    if (train_test %in% colnames(dat)) {
      splits <- unique(dat[[train_test]])
      for (split in splits) {
        # subset for holdout data
        split_dat <- dat[data_id == split]
        # predict on data
        split_dat[, predict := as.vector(h2o::h2o.predict(mod, h2o::as.h2o(split_dat)))]
        # relevel if group variable is factor type
        if ((!is.na(group)) & (is.factor(split_dat[[group]]))) {
          split_dat[[group]] <- as.factor(as.character(split_dat[[group]]))
        }
        
        ## plot data
        if (is.na(size) & is.na(group)) {
          fig <- plotly::plot_ly(data = split_dat,
                                 x = ~response, y = ~response, 
                                 type = "scatter", mode = "lines", 
                                 color = I("red"))
          fig <- plotly::add_trace(fig,
                                   x = ~response, 
                                   y = ~predict,
                                   type = "scatter",
                                   mode = "markers") %>%
            layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"),
                   xaxis = list(title = response),
                   yaxis = list(title = "Predicted"))
        } else if (is.na(size)) { # if use group
          fig <- plotly::plot_ly(data = split_dat,
                                 x = ~response, y = ~response, 
                                 type = "scatter", mode = "lines", 
                                 color = I("red"))
          fig <- plotly::add_trace(fig,
                                   x = ~response, 
                                   y = ~predict,
                                   color = ~.data[[group]],
                                   type = "scatter",
                                   mode = "markers",
                                   marker = list(showlegend = TRUE)) %>%
            layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"),
                   xaxis = list(title = response),
                   yaxis = list(title = "Predicted"))
        } else { # if use group and size
          fig <- plotly::plot_ly(data = split_dat,
                                 x = ~response, y = ~response, 
                                 type = "scatter", mode = "lines", 
                                 color = I("red"))
          fig <- plotly::add_trace(fig,
                                   x = ~response, 
                                   y = ~predict,
                                   color = ~.data[[group]],
                                   size = ~.data[[size]],
                                   type = "scatter",
                                   mode = "markers",
                                   text = ~paste(size, ": ", h2o::signif(.data[[size]], digits = 3)),
                                   marker = list(showlegend = TRUE)) %>%
            layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"),
                   xaxis = list(title = response),
                   yaxis = list(title = "Predicted"))
        }

        # save data to nano object or to separate list
        if (save) {
          nano$metric[[model_no[i]]]$residual_plot[[split]] <- fig
        } else {
          final_out[[split]] <- fig
        }
      }
    } else { # if data not split by train/test/holdout, perform calculation on total data level
      # predict on data
      dat[, predict := as.vector(h2o::h2o.predict(mod, h2o::as.h2o(dat)))]
      
      ## plot data
      if (is.na(size) & is.na(group)) {
        fig <- plotly::plot_ly(data = dat,
                               x = ~response, y = ~response, 
                               type = "scatter", mode = "lines", 
                               color = I("red"))
        fig <- plotly::add_trace(fig,
                                 x = ~response, 
                                 y = ~predict,
                                 type = "scatter",
                                 mode = "markers") %>%
          layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for Total Data"),
                 xaxis = list(title = response),
                 yaxis = list(title = "Predicted"))
      } else if (is.na(size)) { # if use group
        fig <- plotly::plot_ly(data = dat,
                               x = ~response, y = ~response, 
                               type = "scatter", mode = "lines", 
                               color = I("red"))
        fig <- plotly::add_trace(fig,
                                 x = ~response, 
                                 y = ~predict,
                                 color = ~.data[[group]],
                                 type = "scatter",
                                 mode = "markers",
                                 marker = list(showlegend = TRUE)) %>%
          layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for Total Data"),
                 xaxis = list(title = response),
                 yaxis = list(title = "Predicted"))
      } else { # if use group and size
        fig <- plotly::plot_ly(data = dat,
                               x = ~response, y = ~response, 
                               type = "scatter", mode = "lines", 
                               color = I("red"))
        fig <- plotly::add_trace(fig,
                                 x = ~response, 
                                 y = ~predict,
                                 color = ~.data[[group]],
                                 size = ~.data[[size]],
                                 type = "scatter",
                                 mode = "markers",
                                 text = ~paste0(size, ": ", h2o::signif(.data[[size]], digits = 3)),
                                 marker = list(showlegend = TRUE)) %>%
          layout(title = paste0("Residual Plot ", stringr::str_to_title(response), " for Total Data"),
                 xaxis = list(title = response),
                 yaxis = list(title = "Predicted"))
      }
      
      # save data to nano object or to separate list
      if (save) {
        nano$metric[[model_no[i]]]$residual_plot[["all"]] <- fig
      } else {
        final_out[["all"]] <- fig
      }
    }
  }
  
  if (save) {
    return(nano)
  } else {
    return(final_out)
  }
}



