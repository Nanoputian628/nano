#' @title Step-wise VIF Selection
#' @description Detects and removes multi-collinearity via step-wise VIF selection.
#' @importFrom fmsb VIF
#' @param data dataset to be analysed.
#' @param ignore columns in dataset to be not removed. Only relevant if `remove` is \code{TRUE}. 
#' @param thresh threshold of VIF for variables to be removed.
#' @param trace a logical to indicate if the sequence of which variables are removed should be 
#' printed to the console. 
#' @param remove a logical to indicate if variables with VIF higher than `thresh` should be removed. #' If \code{FALSE}, only the VIF for each variable will be output and no variables will be removed. #' Default is \code{TRUE}.
#' @return data.table of VIF for final selected variables.
#' @details Calculates the VIF for each variable in the dataset. If `remove` is \code{FALSE}, then a
#' data.table of the VIFs will be output. If `remove` is \code{TRUE}, then if the variable with the 
#' highest VIF has a VIF greater than `thresh`, that variable will be removed. This process is 
#' repeated sequentially until all variables have a VIF lower than `thresh`. Specifying `trace` to be
#' \code{TRUE} will print the VIFs of the current selection of variables for each iteration of the
#' above process. The `ignore` argument only applies when `remove` has been set to \code{TRUE}. It is
#' a vector of variables in the dataset which should not be removed. However, the VIFs for these 
#' variables are still calculated and output.  
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data(property_prices)
#'  vif_data(property_prices, thresh = 2, trace = TRUE, remove = TRUE)
#'  }
#' }
#' @rdname vif_step
#' @export 


vif_step <-function(data, ignore = c(), thresh = 5, trace = TRUE, remove = TRUE){
  
  if(all(!"data.frame" %in% class(data))) {
    stop("`data` must be a dataset.", 
         call. = FALSE)
  }
  
  if (!(all(ignore %in% names(data)))) {
    stop("'ignore` must be column names in `data`.", 
         call. = FALSE)
  }

  if (!is.numeric(thresh)) {
    stop("`thresh` must be numeric.",
         call. = FALSE)
  }
  
  if (!is.logical(remove)) {
    stop("'remove' must either be TRUE or FALSE,", 
         call. = FALSE)
  }
  
  if (!is.logical(trace)) {
    stop("'trace' must either be TRUE or FALSE,", 
         call. = FALSE)
  }
  
  setDT(data)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(data)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init <- rbind(vif_init, c(val, VIF(lm(form_in, data = data))))
  }
  
  vif_init <- as.data.table(vif_init)
  names(vif_init) <- c("var", "vif")
  vif_init[, vif := as.numeric(vif)]

  if (!remove) {
    if (trace) {
      print(vif_init)
    }
    return(vif_init)
    quit()
  } else {
    vif_eval <- vif_init
    if (length(ignore) > 0) vif_eval <- vif_init[-match(ignore, vif_init[[1]]), ]
    vif_max <- max(as.numeric(vif_eval[[2]]), na.rm = TRUE)
    
    if (vif_max < thresh) {
      if (trace) { #print output of each iteration
        prmatrix(vif_init, collab = c('var','vif'), rowlab = rep('', nrow(vif_init)), quote=F)
        cat('\n')
        cat(paste('All variables have VIF < ', thresh,', max VIF ', round(vif_max, 2), sep=''),'\n\n')
      }
      return(vif_init)
    }
    else {
      in_dat <- copy(data)
      
      #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
      while(vif_max >= thresh) {
        
        vif_vals <- NULL
        var_names <- names(in_dat)
        
        for(val in var_names) {
          regressors <- var_names[-which(var_names == val)]
          form <- paste(regressors, collapse = '+')
          form_in <- formula(paste(val, '~', form))
          vif_add <- VIF(lm(form_in, data = in_dat))
          vif_vals <- rbind(vif_vals,c(val,vif_add))
        }
        
        vif_eval <- vif_vals
        if (length(ignore) > 0) vif_eval <- vif_vals[-match(ignore, vif_vals[,1]), ]
        max_row <- which(vif_eval[,2] == max(as.numeric(vif_eval[,2]), na.rm = TRUE))[1]
        
        vif_max <-as.numeric(vif_eval[max_row, 2])
        
        if (vif_max < thresh) break
        
        if (trace) { #print output of each iteration
          prmatrix(vif_vals, collab = c('var','vif'), rowlab = rep('', nrow(vif_vals)), quote = F)
          cat('\n')
          cat('removed: ', vif_eval[max_row, 1],vif_max,'\n\n')
          flush.console()
        }
        
        in_dat <- in_dat[, which(!names(in_dat) %in% vif_eval[max_row, 1]), with = FALSE]
        
      }
      
      vif_vals <- as.data.table(vif_vals)
      names(vif_vals) <- c("var", "vif")
      vif_vals[, vif := as.numeric(vif)]
      
      return(vif_vals)
    }
  }
}
