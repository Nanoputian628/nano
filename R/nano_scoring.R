#' @title Score data from fitted model and compare with response by percentiles 
#' @description Predict on data from fitted model and compares the mean prediction with the
#' mean response by the inputted percentiles.
#' @param nano a nano object containing the fitted models. 
#' @param data a list of datasets. If the underlying dataset is the same for each model, can
#' only input a list with a single element.
#' @param model_no the positions of each model in the list of models in the nano object for which
#' the PDP should be calculated. If not entered, the last model is taken by default.
#' @param train_test a character. Variable in `data` which contains split for training, 
#' testing and holdout datasets (optional). Can only have the values: "train", "test", 
#' "holdout".
#' @param save a logical specifying whether to save the output to the nano object (if \code{save = TRUE})
#' otherwise output as separate object..
#' @return if \code{save = TRUE} then returns nano object with the specified models scored. If
#' \code{save = FALSE} then returns a list with the specified models scored.
#' @details Functions checks whether the data contains the `train_test` column. If it does then
#' scoring is done for each split specified in the `train-test` column. Otherwise, the scoring 
#' is done on the total data. 
#' If desire to perform scoring on a subset of the data (e.g. to see performance of the model on
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
#'  obj <- nano_scoring(nano = obj, model_no = 1:2, percentiles = seq(0, 1, 0.02), save = TRUE)
#'  }
#' }
#' @rdname nano_scoring
#' @export



nano_scoring <- function (nano, data = NA, model_no = NA, percentiles, 
                                train_test = "data_id", save = TRUE) {
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
  
  if (class(percentiles) != "numeric") {
    stop("`percentiles` must be numeric.", 
         call. = FALSE)
  }
  
  if (min(percentiles) < 0 | max(percentiles) > 1) {
    stop("`percentiles` must be between 0 and 1.", 
         call. = FALSE)
  }
  
  if (class(save) != "logical") {
    stop("`save` must be a logical.", 
         call. = FALSE)
  }
  
  # if choosing not to save to nano object, save outputs to a separate list
  if (!save) {
    final_out <- list()
    final_out[["pred vs actual"]][["data"]] <- list()
    final_out[["pred vs actual"]][["plot"]] <- list()
    final_out[["actual vs pred"]][["data"]] <- list()
    final_out[["actual vs pred"]][["plot"]] <- list()
  }
  
  # convert data to list of datasets if required
  if ("data.frame" %in% class(data)) {
    data <- rep(list(data), length(model_no)) 
  }
  
  # remove last percentile if it is 1
  if (percentiles[length(percentiles)] == 1) {percentiles <- percentiles[1:length(percentiles)-1]}

  # custom function to round down to significant figures
  down_signif <- function(x, digits = 0) {
    if (x == 0) {
      return(0)
    } else {
      m <- 10^(ceiling(log(x, 10)) - digits)
      return((x %/% m)*m)
    }
  }
    
  for (i in 1:length(model_no)) {
    # obtain required model data
    response <- nano$meta[[model_no[i]]]$y
    mod <- nano$model[[model_no[i]]]
    if (all(is.na(data))) {dat <- nano$data[[model_no[i]]]} else {dat <- data[[i]]}
    
    # if data split by train/test/holdout, perform calculation for each subset
    if (train_test %in% colnames(dat)) {
      splits <- unique(dat[[train_test]])
      for (split in splits) {
        # subset for holdout data
        split_dat <- dat[data_id == split]
        # predict on data
        split_dat[, predict := as.vector(h2o::h2o.predict(mod, h2o::as.h2o(split_dat)))]
        
        ## Score for predicted vs actuals
        # band actuals into required percentiles
        var_percentiles <- as.vector(quantile(split_dat[[response]], percentiles, na.rm = TRUE))
        var_percentiles[1] <- down_signif(var_percentiles[1], 4) # round down first value to ensure all values are captured in the band
        var_percentiles <- h2o::signif(var_percentiles)
        intervals <- list()
        intervals[[response]] <- var_percentiles
        split_dat <- nano::band_data(split_dat, intervals)
        data.table::setnames(split_dat, paste0(response, "_bnd"), "response_bnd")
        # summarised data by percentiles
        predict_sum <- split_dat[, .(response = mean(get(response), na.rm = TRUE), 
                                     predict = mean(predict, na.rm = TRUE)), 
                                 by = response_bnd]
        predict_sum <- predict_sum[order(response)]
        
        # plot data
        fig <- plotly::plot_ly(data = predict_sum,
                               x = ~response_bnd, 
                               y = ~response, 
                               type = "scatter",
                               mode = "lines+markers",
                               name = paste0("Actual ", stringr::str_to_title(response)))
        fig <- plotly::add_trace(fig,
                                 y = ~predict, 
                                 type = "scatter",
                                 mode = "lines+markers",
                                 name = "Predict") %>% 
          layout(xaxis = list(title = paste0("Percentiles of Actual ", stringr::str_to_title(response)),
                              hoverformat = ",.2s"),
                 yaxis = list(title = "Value", hoverformat = ",.2s"),
                 title = paste0("Predicted vs Actual ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"))

        # save data to nano object or to separate list
        if (save) {
          nano$metric[[model_no[i]]]$pred_vs_actual$scoring_dat[[split]] <- predict_sum
          nano$metric[[model_no[i]]]$pred_vs_actual$scoring_fig[[split]] <- fig
        } else {
          final_out[["pred vs actual"]][["data"]][[paste0("data_", model_no[i])]][[split]] <- predict_sum
          final_out[["pred vs actual"]][["plot"]][[paste0("plot_", model_no[i])]][[split]] <- fig
        }
        
        
        ## Score for actuals vs predicted
        # band predictions into required percentiles
        var_percentiles <- as.vector(quantile(split_dat[["predict"]], percentiles, na.rm = TRUE))
        var_percentiles[1] <- down_signif(var_percentiles[1], 4) # round down first value to ensure all values are captured in the band
        var_percentiles <- h2o::signif(var_percentiles)
        intervals <- list()
        intervals[["predict"]] <- var_percentiles
        split_dat <- nano::band_data(split_dat, intervals)
        # summarised data by percentiles
        predict_sum <- split_dat[, .(response = mean(get(response), na.rm = TRUE), 
                                     predict = mean(predict, na.rm = TRUE)), 
                                 by = predict_bnd]
        predict_sum <- predict_sum[order(predict)]
        
        # plot data
        fig <- plotly::plot_ly(data = predict_sum,
                               x = ~predict_bnd, 
                               y = ~predict, 
                               type = "scatter",
                               mode = "lines+markers",
                               name = "Predict")
        fig <- plotly::add_trace(fig,
                                 y = ~response, 
                                 type = "scatter",
                                 mode = "lines+markers",
                                 name = paste0("Actual ", stringr::str_to_title(response))) %>% 
          layout(xaxis = list(title = paste0("Percentiles of Predicted ", stringr::str_to_title(response)),
                              hoverformat = ",.2s"),
                 yaxis = list(title = "Actual", hoverformat = ",.2s"),
                 title = paste0("Actual vs Predicted ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"))
        
        # save data to nano object or to separate list
        if (save) {
          nano$metric[[model_no[i]]]$actual_vs_pred$scoring_dat[[split]] <- predict_sum
          nano$metric[[model_no[i]]]$actual_vs_pred$scoring_fig[[split]] <- fig
        } else {
          final_out[["actual vs pred"]][["data"]][[paste0("data_", model_no[i])]][[split]] <- predict_sum
          final_out[["actual vs pred"]][["plot"]][[paste0("plot_", model_no[i])]][[split]] <- fig
        }
      }
    } else { # if data not split by train/test/holdout, perform calculation on total data level
      # predict on data
      dat[, predict := as.vector(h2o::h2o.predict(mod, h2o::as.h2o(dat)))]
      
      
      ## Score for predicted vs actuals
      # band predictions into required percentiles
      var_percentiles <- as.vector(quantile(dat[[response]], percentiles, na.rm = TRUE))
      var_percentiles[1] <- down_signif(var_percentiles[1], 4) # round down first value to ensure all values are captured in the band
      var_percentiles <- h2o::signif(var_percentiles)
      intervals <- list()
      intervals[[response]] <- var_percentiles
      dat <- nano::band_data(dat, intervals)
      data.table::setnames(dat, paste0(response, "_bnd"), "response_bnd")
      # summarised data by percentiles
      predict_sum <- dat[, .(response = mean(get(response), na.rm = TRUE), 
                             predict = mean(predict, na.rm = TRUE)), 
                         by = response_bnd]
      predict_sum <- predict_sum[order(response)]
      
      # plot data
      fig <- plotly::plot_ly(data = predict_sum,
                             x = ~response_bnd, 
                             y = ~response, 
                             type = "scatter",
                             mode = "lines+markers",
                             name = paste0("Actual ", stringr::str_to_title(response)))
      fig <- plotly::add_trace(fig,
                               y = ~predict, 
                               type = "scatter",
                               mode = "lines+markers",
                               name = "Predict") %>% 
        layout(xaxis = list(title = paste0("Percentiles of Actual ", stringr::str_to_title(response)),
                            hoverformat = ",.2s"),
               yaxis = list(title = "Value", hoverformat = ",.2s"),
               title = paste0("Predicted vs Actual ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"))

      # save data to nano object or to separate list
      if (save) {
        nano$metric[[model_no[i]]]$pred_vs_actual$scoring_dat[["all"]] <- predict_sum
        nano$metric[[model_no[i]]]$pred_vs_actual$scoring_fig[["all"]] <- fig
      } else {
        final_out[["pred vs actual"]][["data"]][[paste0("data_", model_no[i])]][["all"]] <- predict_sum
        final_out[["pred vs actual"]][["plot"]][[paste0("plot_", model_no[i])]][["all"]] <- fig
      }
      
      
      ## Score for actuals vs predicted
      # band predictions into required percentiles
      var_percentiles <- as.vector(quantile(dat[["predict"]], percentiles, na.rm = TRUE))
      var_percentiles[1] <- down_signif(var_percentiles[1], 4) # round down first value to ensure all values are captured in the band
      var_percentiles <- h2o::signif(var_percentiles)
      intervals <- list()
      intervals[["predict"]] <- var_percentiles
      dat <- nano::band_data(dat, intervals)
      # summarised data by percentiles
      predict_sum <- dat[, .(response = mean(get(response), na.rm = TRUE), 
                             predict = mean(predict, na.rm = TRUE)), 
                         by = predict_bnd]
      predict_sum <- predict_sum[order(predict)]
      
      # plot data
      fig <- plotly::plot_ly(data = predict_sum,
                             x = ~predict_bnd, 
                             y = ~predict, 
                             type = "scatter",
                             mode = "lines+markers",
                             name = "Predict")
      fig <- plotly::add_trace(fig,
                               y = ~response, 
                               type = "scatter",
                               mode = "lines+markers",
                               name = paste0("Actual ", stringr::str_to_title(response))) %>% 
        layout(xaxis = list(title = paste0("Percentiles of Predicted ", stringr::str_to_title(response)),
                            hoverformat = ",.2s"),
               yaxis = list(title = "Actual", hoverformat = ",.2s"),
               title = paste0("Predicted vs Actual ", stringr::str_to_title(response), " for ", stringr::str_to_title(split), " Data"))
      
      # save data to nano object or to separate list
      if (save) {
        nano$metric[[model_no[i]]]$actual_vs_pred$scoring_dat[["all"]] <- predict_sum
        nano$metric[[model_no[i]]]$actual_vs_pred$scoring_fig[["all"]] <- fig
      } else {
        final_out[["actual vs pred"]][["data"]][[paste0("data_", model_no[i])]][["all"]] <- predict_sum
        final_out[["actual vs pred"]][["plot"]][[paste0("plot_", model_no[i])]][["all"]] <- fig
      }
    }
  }
  
  if (save) {
    return(nano)
  } else {
    return(final_out)
  }
}



