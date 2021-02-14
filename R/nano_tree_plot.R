# install.packages("collapsibleTree")
# library(collapsibleTree)
# test <- collapsibleTree(drf_tree, collapsed = FALSE)


#' @title Create Tree Diagram  
#' @description Create tree diagram from a tree-based model. 
#' @param nano a nano object. 
#' @param grid_no a numeric. Slot number of model to plot tree diagram of. 
#' @param tree_number a numeric. Number of tree to plot.
#' @param tree_class a character. Only valid for multinomial classification. Specific class of 
#' response variable to plot tree diagram of.
#' @details Plots a tree diagram of a specific tree built by a tree-based model (algorithm =
#' `drf` or `gbm`) stored in a nano object. Note, a `drf` or `gbm` usually builts hundreds of
#' different trees, whose individual predictions are averaged to obtain the final prediction. 
#' However, only a single of these trees can be plotted at a time using this function. 
#' 
#' This function is adapted from the code in the following webpage: "https://www.h2o.ai/blog/finally-you-can-plot-h2o-decision-trees-in-r/". This code converts the information from the 
#' h2o model into a `Node` object using the data.tree pacakge and then is visualised using the 
#' DiagrammeR package.  
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  library(h2o)
#'  library(nano)
#'  
#'  nano.init()
#'  
#'  # import dataset
#'  data(property_prices)
#'  
#'  # create model
#'  nano <- nano_grid(data     = data, 
#'                    response = "sale_price",
#'                    algo     = "gbm",
#'                    ntrees   = 10)
#'                      
#'  # create tree diagram of the 5th tree build by GBM model
#'  tree <- nano_tree_plot(nano, 1, 5)
#'  # plot tree diagram
#'  plot(tree)
#'  
#'  }
#' }
#' @rdname nano_tree_plot
#' @export 


nano_tree_plot <- function(nano, grid_no = nano$n_model, tree_number = 1, tree_class = NA) {
  
  if (class(nano) != "nano") {
    stop("`nano` must be a nano object.",
         call. = FALSE)
  }
  
  if (min(grid_no) < 1 | max(grid_no) > nano$n_model | any(grid_no %% 1 != 0)) {
    stop(paste0("`grid_no` must be an integers between 1 and ", nano$n_model),
         call. = FALSE)
  }
  
  if (tree_number < 1 | tree_number %% 1 != 0) {
    stop("`tree_number` must be a positive integer.",
         call. = FALSE)
  }
  
  if (!nano$meta[[grid_no]]$is_multinomial_classification & !is.na(tree_class)) {
    stop("`tree_class` is only valid for multinomial classification.",
         call. = FALSE)
  }
  
  if (!nano$model[[grid_no]]@algorithm %in% c("drf", "gbm")) {
    stop("Model must be tree-based. Use algorithm `drf or `gbm`.",
         call. = FALSE)
  }
  
  # extract tree information from h2o model
  tree <- h2o::h2o.getModelTree(model       = nano$model[[grid_no]], 
                                tree_number = tree_number, 
                                tree_class  = tree_class)
  
  # functions to convert h2o object to data.tree object 
  createDataTree <- function(h2oTree) {
    h2oTreeRoot   = h2oTree@root_node
    dataTree      = data.tree::Node$new(h2oTreeRoot@split_feature)
    dataTree$type = 'split'
    addChildren(dataTree, h2oTreeRoot)
    return(dataTree)
  }
  
  # function to add splits
  addChildren <- function(dtree, node) {
    
    if (class(node)[1] != 'H2OSplitNode') return(TRUE)
    
    feature      = node@split_feature
    id           = node@id
    na_direction = node@na_direction
    
    if (is.na(node@threshold)) {
      leftEdgeLabel = printValues(node@left_levels, na_direction=='LEFT', 4)
      rightEdgeLabel = printValues(node@right_levels, na_direction=='RIGHT', 4)
    } else {
      leftEdgeLabel  = paste("<" , node@threshold, ifelse(na_direction == 'LEFT',',NA',''))
      rightEdgeLabel = paste(">=", node@threshold, ifelse(na_direction == 'RIGHT',',NA',''))
    }
    
    left_node  = node@left_child
    right_node = node@right_child
    
    if (class(left_node)[[1]] == 'H2OLeafNode')
      leftLabel = paste("prediction:", left_node@prediction)
    else
      leftLabel = left_node@split_feature
    
    if (class(right_node)[[1]] == 'H2OLeafNode')
      rightLabel = paste("prediction:", right_node@prediction)
    else
      rightLabel = right_node@split_feature
    
    if (leftLabel == rightLabel) {
      leftLabel  = paste(leftLabel, "(L)")
      rightLabel = paste(rightLabel, "(R)")
    }
    
    dtreeLeft           = dtree$AddChild(leftLabel)
    dtreeLeft$edgeLabel = leftEdgeLabel
    dtreeLeft$type      = ifelse(class(left_node)[1] == 'H2OSplitNode', 'split', 'leaf')
    
    dtreeRight           = dtree$AddChild(rightLabel)
    dtreeRight$edgeLabel = rightEdgeLabel
    dtreeRight$type      = ifelse(class(right_node)[1] == 'H2OSplitNode', 'split', 'leaf')
    
    addChildren(dtreeLeft, left_node)
    addChildren(dtreeRight, right_node)
    
    return(FALSE)
  }
  
  # function to print edges and vertices 
  printValues <- function(values, is_na_direction, n=4) {
    l = length(values)
    if (l == 0)
      value_string = ifelse(is_na_direction, "NA", "")
    else
      value_string = paste0(paste0(values[1:min(n,l)], collapse = ', '),
                            ifelse(l > n, ",...", ""),
                            ifelse(is_na_direction, ", NA", ""))
    return(value_string)
  }
  
  # create data.tree object 
  tree_diag = createDataTree(tree)
  
  GetEdgeLabel <- function(node) {return (node$edgeLabel)}
  GetNodeShape <- function(node) {switch(node$type, split = "diamond", leaf = "oval")}
  GetFontName <- function(node) {switch(node$type, 
                                        split = 'Palatino-bold', 
                                        leaf  = 'Palatino')}
  SetEdgeStyle(tree_diag, fontname = 'Palatino-italic', 
               label = GetEdgeLabel, labelfloat = TRUE,
               fontsize = "26", fontcolor='royalblue4')
  SetNodeStyle(tree_diag, fontname = GetFontName, shape = GetNodeShape, 
               fontsize = "26", fontcolor='royalblue4',
               height ="0.75", width ="1")
  
  SetGraphStyle(tree_diag, rankdir = "LR", dpi = 70.)
  
  return(tree_diag)
}  



