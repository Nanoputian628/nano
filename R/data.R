#' Sydney Property Prices in 2011
#'
#' @description Ruppert and Wand (2018) give a data set called SydneyRealEstate.
#' This dataset has data on several variables concerning houses sold in Sydney, 
#' Australia, during 2001. The original dataset has 37676 rows with 39 columns.
#' This was subset so the property_prices dataset has only 500 rows with 13
#' columns and was converted to data.table format.  
#' 
#' @usage data(property_prices)
#'
#' @format this dataset has 500 rows and the following 13 columns:
#' \describe{
#'   \item{sale_price}{sale price in Australian dollars.}
#'   \item{lot_size}{lot size in square meters but with some imputation.}
#'   \item{sale_qtr}{pfinancial quarter in which sale took place.}
#'   \item{crime_rate}{crime rate measure for the suburb in which the house is located.}
#'   \item{income}{average weekly income of the suburb in which the house is located.}
#'   \item{dist_to_coastline}{distance from house to the nearest coastline location (kilometers).}
#'   \item{dist_to_rail_station}{distance from house to the nearest railway station (kilometers).}
#'   \item{dist_to_highway}{distance from house to the nearest highway (kilometers).}
#'   \item{air_noise}{aircraft noise exposure measure.}
#'   \item{foreigner_ratio}{proportion of foreigners in the suburb in which the house is located.}
#'   \item{PM10}{particulate matter with a diameter of under 10 micrometers leve recorded at the air
#'   pollution monitoring station nearest to the house.}
#'   \item{dist_to_hospital}{distance from house to the nearest hospital (kilometers).}
#'   \item{dist_to_school}{distance from house to the nearest school (kilometers).}
#' }
#' @source \url{https://rdrr.io/cran/HRW/man/SydneyRealEstate.html}
"property_prices"
