% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/band_data.R
\name{band_data}
\alias{band_data}
\title{Bands Variables in Dataset}
\usage{
band_data(
  data,
  intervals,
  buckets = NULL,
  na_bucket,
  unmatched_bucket,
  trunc_left = FALSE,
  trunc_right = FALSE,
  include_left = TRUE
)
}
\arguments{
\item{data}{dataset to be analysed.}

\item{intervals}{a list defining the bands for each of the variables.}

\item{buckets}{a list defining the names of the bands for each of the variables.}

\item{na_bucket}{a character or a list defining the bucket name for entries with \code{NA}.}

\item{unmatched_bucket}{a character or a list defining the bucket name for unmatched entries.}

\item{trunc_left}{a logical specifying whether the band to \code{-Inf} should be created.}

\item{trunc_right}{a logical specifying whether the band to \code{Inf} should be created.}

\item{include_left}{a logical specifying if should include the left or right endpoint for each
interval.}
}
\value{
a data.table with the original variables and new banded variables with the suffix "_bnd".
}
\description{
Creates a custom bands for variables in dataset.
}
\details{
The \code{intervals} parameter must be entered as a list with names matching the column names
in \code{data}. The elements of the list can be specified in two ways.

It can be specified as a vector of non-decreasing numbers (note the same number can be repeated,
this will correspond to a band of a single point). The intervals will then be derived from these
vectors of numbers in combination with the \code{trunc_left}, \code{trunc_right} and \code{include_left}
parameters. For example, the vector \code{c(1, 3, 3, 6)} with the default parameters will produce
the intervals \code{(-Inf, 1), [1, 3), [3, 3), [3, 6), [6, Inf)}. It can also be directly
specified as a character vector of the desired intervals. Note if this option is taken, the
\code{trunc_left}, \code{trunc_right} and \code{include_left} parameters become redundant.

The \code{na_bucket} and \code{unmatched_bucket} parameters can be specified either a single character or
as a list of the desired bucket names for each variable. If specified as a single character,
then this will be applied to all variables. If the \code{buckets} parameter is not specified, then
the bucket names will be set equal to the interval names.
\dontrun{
if(interactive()){
 data(property_prices)
 band_data(data = property_prices,
           intervals = list(crime_rate = seq(0.1, 1, 0.1), # example as numeric vector
                            income = c("[0,500)", "[500, 1000)") # example as character vector
                            )
           )                 
 }
}
}
