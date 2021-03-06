% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nano_grid.R
\name{nano_grid}
\alias{nano_grid}
\title{Building H2O Grids}
\usage{
nano_grid(
  nano = nano::create_nano(),
  response,
  algo,
  data,
  test,
  train_test = NA,
  grid_id = paste0("grid_", nano$n_model + 1),
  ignore_vars = c(),
  weight_column = NULL,
  fold_column = NULL,
  nfolds = NA,
  thresh = 10,
  monotone_constraints = c(),
  hyper_params = NULL,
  strategy = "RandomDiscrete",
  max_models = 10,
  max_runtime_secs = 60 * 10,
  stopping_metric = NULL,
  stopping_tolerance = 0,
  stopping_rounds = 0,
  plots = TRUE,
  alarm = TRUE,
  quiet = FALSE,
  save = FALSE,
  subdir = NA,
  project = "ML Project",
  seed = 628,
  grid_description = "",
  ...
)
}
\arguments{
\item{nano}{nano object to store model in. If not specified, a new nano object will be
created the results will be stored in the new nano object.}

\item{response}{a character. Target variable for model.}

\item{algo}{a character. Algorithm of model to be built.}

\item{data}{a data.frame containing data to train model. May also contain testing and
holdout data, in which case, the \code{train_test} must be specified.}

\item{test}{a data.frame containing testing dataset. If this is provided, the \code{train_test},
\code{fold_column} and \code{nfolds} arguments cannot be used.}

\item{train_test}{a character. Variable in \code{data} which contains split for training,
testing and holdout datasets (optional). Can only have the values: "training", "test",
"holdout".}

\item{grid_id}{a character. Unique id for created grid.}

\item{ignore_vars}{vector of characters. Variables in the dataset which should not be used
for modelling. Note, if any of \code{train_test}, \code{weight_column} or \code{fold_column} arguments
are specified, those variables will be automatically included in \code{ignore_vars}.}

\item{weight_column}{a character. Column name in \code{data} containing weights if used.}

\item{fold_column}{a character. Column name in \code{data} containing fold assignments if used.
If this is provided, the \code{test} and \code{nfolds} arguments cannot be used. The \code{train_test}
argument can be used, however it cannot contain the values "test".}

\item{nfolds}{a numeric. Number of folds used in cross-validation. If this is provided, the
\code{test} and \code{nfolds} arguments cannot be used. The \code{train_test} argument can be used,
however it cannot contain the values "test".}

\item{thresh}{a numeric. Cutoff of number of unique values in response variable to
determine whether performing classification or regression. Default value is 10.}

\item{monotone_constraints}{a list. Mapping between variable names in \code{data} to values
+1 or -1. Use +1 to enforce an increasing constraint while use -1 for a decreasing
constraint. Constraints are only valid for numerical columns.}

\item{hyper_params}{a list. Contains model hyper-parameters for hyper-parameter tuning. The
possible hyper-parameters that can be used depends on the algorithm. Look at the \code{H2O}
functions for the specific algorithm to see full details on the available hyper-parameters:
h2o.gbm, h2o.glm, h2o.kmeans, h2o.deepLearning.}

\item{strategy}{a character. Specify whether to perform a Cartesian or random grid search.
Only required when more than 1 combination of hyper-parameters are entered.}

\item{max_models}{a numeric. Maximum number of models to be built.}

\item{max_runtime_secs}{a numeric. Maximum amount of time (sec) spent building models.}

\item{stopping_metric}{a character. Metric used to determine whether to terminate fitting
process.}

\item{stopping_tolerance}{a numeric. Minimum threshold for the \code{stopping_metric} to
improve by to continue the fitting process.}

\item{stopping_rounds}{a numeric. Number of rounds in which if the \code{stopping_metric} has
not at least improved by the \code{stopping_tolerance}, then terminate fitting process.}

\item{plots}{a logical. Whether to produce plots.}

\item{alarm}{a logical. Whether to beep when function has finished running.}

\item{quiet}{a logical. Whether to print messages to the console.}

\item{seed}{a numeric.}

\item{grid_description}{a character. Optional description of grid. Can be later accessed by
\code{nano$grid[[grid_no]]@meta$description}.}

\item{...}{further parameters to pass to \code{h2o.grid} depending on \code{algo}.}
}
\value{
nano object with new entry filled with grid produced.
}
\description{
Creates wide range of machine learning models and perform grid search using
\code{H2O}'s \code{h2o.grid} function implemented with \code{nano} objects.
}
\details{
This function used \code{H2O}'s \code{h2o.grid} function to easily and quickly build
difference machine learning models and perform grid search for hyper-parameter tuning. To
perform hyper-parameter tuning, input the desired hyper-parameters in the \code{hyper_params}
argument and set the range of values to build the models on. Importantly, an active H2O
connection is required (i.e. run \code{h2o.init()})) before using this function.

For more details, please see the documentation for \code{h2o.grid}.
}
\examples{
\dontrun{
if(interactive()){
 library(h2o)
 library(nano)
 
 h2o.init()
 
 # import dataset
 data(property_prices)
 # prepare data for modelling
 data_all <- nano::data_prep(data          = property_prices, 
                             response      = "sale_price",
                             split_or_fold = 0.7,
                             holdout_ratio = 0.1)
 data <- data_all$data
 
 # create models and perform hyper-parameter tuning on ntrees
 nano <- nano_grid(data               = data, 
                   response           = "sale_price", 
                   algo               = "drf", # random forest
                   train_test         = "data_id",
                   ignore_vars        = "data_id",
                   hyper_params       = list(ntrees = 1:5),
                   strategy           = "RandomDiscrete", # random grid search
                   stopping_metric    = "mse",
                   stopping_tolerance = 1e-3,
                   stopping_rounds    = 5)
 
 }
}
}
