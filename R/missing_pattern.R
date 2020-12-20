#' @title Calculate Missing Pattern in Dataset 
#' @description Creates a summary of number of missing values by combination of variables. 
#' @importFrom plotly plot_ly
#' @param data dataset to be analysed.
#' @param ignore columns in dataset to be ignored. 
#' @param plot a logical indicating whether the results should be plotted.
#' @return a list of various summaries and plots on the missing pattern.
#' @details Identifies all combinations of variables which are missing values (NAs or blanks). 
#' Then calculates the frequency of missing values for each identified combination and then
#' produces a heatmap of the result.   
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  miss <- missing_pattern(property_prices)
#'  # plot heat map
#'  miss$plot()
#'  }
#' }
#' @rdname missing_pattern
#' @export 


missing_pattern <- function(data, ignore = c(), plot = TRUE) {
  if (!("data.frame" %in% class(data))) {
    stop("`data` must be a dataset.", 
         call. = FALSE)
  }
  
  if (!(all(ignore %in% names(data)))) {
    stop("'ignore` must be column names in `data`.", 
         call. = FALSE)
  }
  
  if (!is.logical(plot)) {
    stop("`plot` must either be TRUE or FALSE.",
         call. = FALSE)
  }
  
  
  is_missing <- function(x) {
    is.na(x) | x == ""
  }
  
  setDT(data)[, setdiff(names(data), ignore), with = FALSE]
  if (sum(is_missing(data)) == 0) {
    message("Hurray! Dataset has no missing values.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  data <- data[, names(data)[(colSums(is_missing(data))>0)], with = FALSE]
  
  n <- nrow(data)
  cn <- names(data)
  n_miss <- sapply(data, function(x) sum(is_missing(x)))
  imp <- rep(FALSE, ncol(data))
  tmp <- ifelse(is_missing(data), 1, 0)
  tab <- table(apply(tmp, 1, paste, collapse = ":"))
  miss_comb <- as.integer(tab)
  tabcomb <- sapply(names(tab), function(x) as.integer(unlist(strsplit(x, 
                                                                       ":", 
                                                                       fixed = TRUE))), 
                    USE.NAMES = FALSE)
  tabcomb <- as.data.frame(tabcomb)
  miss_comb_per <- miss_comb / nrow(data) * 100
  miss_label <- paste0(miss_comb, " (", miss_comb_per, "%)")
  tabcomb <- (t(tabcomb))
  
  miss_comb_plot <- function() {
    plot_ly(x = names(data), 
            y = miss_label,
            z = tabcomb, 
            type = "heatmap", 
            colorscale = "Dark2", 
            showscale = F)
  }
  res <- list(count   = miss_comb, 
              percent = miss_comb_per, 
              tabcomb = tabcomb, 
              plot    = miss_comb_plot)
}

