% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_to_nano.R
\name{grid_to_nano}
\alias{grid_to_nano}
\title{Convert Grid to Nano Object}
\usage{
grid_to_nano(
  nano,
  grids_no = nano$n_model,
  n_top_model = length(nano$grid[[grid_no]]@model_ids)
)
}
\arguments{
\item{nano}{nano object containing the grid to be converted to a new nano object.}

\item{n_top_model}{a numeric. Vector of the n top number of models to select from each grid.}

\item{grid_no}{a numeric. Vector containing positions of grids to be converted to a nano
object.}
}
\value{
a \code{nano} object
}
\description{
Converts a grid in a nano object to a separate nano object.
}
\details{
Converts a specified grid in a nano object to a new nano object where a new slot
is created for each model in the specified grid. The purpose of this function is to be used
when performing hyper-parameter tuning (using the \code{nano_grid} function). When hyper-
parameter tunning is performed, a grid of models is created, however, only a single model
will be stored in the nano object. The diagnostic functions (e.g. \code{nano_pdp}, \code{nano_ice},
etc) are only able to analyse models in different slots of the nano object. Hence,
to compare the models in the grid with each other, this function should be run to create
a new nano object with a slot of each model. Then the usual diagnostic functions can be
run to compare the models. Later, if you wish to select a different model from the grid
(by default, the model with the best metric will be stored in the nano object), then the
\code{switch_model} function can be run.
}
\examples{
\dontrun{
if(interactive()){
 library(h2o)
 library(nano)
 
 h2o.init()
 
 # import dataset
 data(property_prices)
 
 # set the response and predictors
 response <- "sale_price"
 var <- setdiff(colnames(property_prices), response)
 
 # hyper-parameter tuning
 nano <- nano_grid(data         = property_prices, 
                   response     = response, 
                   algo         = "drf",
                   hyper_params = list(ntrees = 1:3))
                   
 # convert created grid to new nano object with 3 slots
 nano_1 <- grid_to_nano(nano, 1)
 
 }
}
}
