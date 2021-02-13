#' @title Calculate and Plot Variable Importance
#' @description Calculates and plots variable importance of model in nano object.
#' @rawNamespace import(h2o, except = c(hour, week, month, year))
#' @param nano nano object containing models
#' @param model_no the position of the model in the list of models in the nano object. If not 
#' entered, the last model is taken by default.
#' @param plot a logical specifying whether the variable importance should be plotted.
#' @param n_var only relevant if \code{plot = TRUE}. Specifies the maximum number of variables 
#' which should be shown in the plot.
#' @param subtitle subtitle for the plot.
#' @param save a logical specifying whether the plot should be saved into working directory.
#' @param subdir sub directory in which the plot should be saved.
#' @param file_name file name of the saved plot.   
#' @return nano object with variable importance of specified model calculated. Also returns a plot
#' if \code{plot = TRUE}.
#' @details Function first checks if the variable importance of the specified model has already 
#' been calculated (by checking in the list \code{nano$varimp}). If it has not been calculated, 
#' then the variable importance will be calculated and the relevant slot in \code{nano$varimp}
#' will be filled out. 
#' 
#' If \code{plot = TRUE}, a plot of the variable importance will also be returned. The plot can
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
#'  obj <- create_nano(grid = list(grid_1),
#'                     data = list(property_prices),  
#'                     ) # since model is not entered, will take best model from grid
#'  
#'  # calculate varimp
#'  nano <- nano_varimp(nano = nano, model_no = 1, plot = FALSE)
#'  
#'  }
#' }
#' @rdname nano_varimp
#' @export 

nano_varimp <- function (nano, model_no = NA, plot = TRUE, n_var = 10, subtitle = NA, save = FALSE, 
                         subdir = NA, file_name = "viz_importance.png") {

  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.", 
         call. = FALSE)
  }
  
  if (!any(is.na(model_no))) {
    if (!is.integer(as.integer(model_no))) {
      stop("`model_no` must be numeric.", 
           call. = FALSE)
    }
    
    if (min(model_no) <= 0) {
      stop("`model_no` must be greater than 0", 
           call. = FALSE)
    }
    
    if (max(model_no) > nano$n_model) {
      stop("`model_no` cannot be greater than the number of models.",
           call. = FALSE)
    }
  }
  
  if (!is.integer(as.integer(n_var))) {
    stop("`n_var` must be numeric.", 
         call. = FALSE)
  }
  
  if (n_var <= 0) {
    stop("`n_var` must be greater than 0", 
         call. = FALSE)
  }
  
  # if model_no not entered, then use last model as default
  if (all(is.na(model_no))) model_no = nano$n_model
  for (i in 1:length(model_no)) {
    # if varimp has not already been calculated for the model:
    num <- model_no[i]
    if (all(is.na(nano$varimp[[num]]))) {
      nano$varimp[[num]] <- data.table::setDT(h2o::h2o.varimp(nano$model[[num]]))
      nano$varimp[[num]][, percentage := percentage * 100]
      names(nano$varimp)[num] <- paste0("varimp_", num)
    }
  }

  # colours <- "deepskyblue"
  # out <- data.frame(var = var, imp = 100 * imp, Type = colours)
  # if (length(var) < limit) 
  #   limit <- length(var)
  # output <- out[1:limit, ]
  # p <- ggplot(output, aes(x = reorder(.data$var, .data$imp), 
  #                         y = .data$imp, label = formatNum(.data$imp, 1))) + geom_col(aes(fill = .data$Type), 
  #                                                                                     width = 0.08, colour = "transparent") + geom_point(aes(colour = .data$Type), 
  #                                                                                                                                        size = 6.2) + coord_flip() + geom_text(hjust = 0.5, size = 2.1, 
  #                                                                                                                                                                               inherit.aes = TRUE, colour = "white") + labs(title = paste0("Most Relevant Variables (top ", 
  #                                                                                                                                                                                                                                           limit, " of ", length(var), ")"), x = NULL, 
  #                                                                                                                                                                                                                            y = NULL) + scale_y_continuous(position = "right", 
  #                                                                                                                                                                                                                                                           expand = c(0, 0), limits = c(0, 1.03 * max(output$imp))) + 
  #   theme_lares2()
  # if (length(unique(output$Type)) == 1) {
  #   p <- p + geom_col(fill = colours, width = 0.2, colour = "transparent") + 
  #     geom_point(colour = colours, size = 6) + guides(fill = FALSE, 
  #                                                     colour = FALSE) + geom_text(hjust = 0.5, size = 2, 
  #                                                                                 inherit.aes = TRUE, colour = "white")
  # }
  # if (!is.na(model_name)) 
  #   p <- p + labs(caption = model_name)
  # if (!is.na(subtitle)) 
  #   p <- p + labs(subtitle = subtitle)
  # if (save) 
  #   export_plot(p, file_name, subdir = subdir, width = 6, 
  #               height = 6)
  return(nano)
}