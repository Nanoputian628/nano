# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(usethis, data.table)

# clean data ----
data <- data.table::fread("C:\\Users\\dilsh\\Documents\\R Packages\\data\\property_prices_2011_raw.csv")


colnames_cam <- colnames(data)
colnames_snk <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames_cam, perl = TRUE)
colnames_snk <- sub("^(.[a-z])", "\\L\\1", colnames_snk, perl = TRUE)
colnames(data) <- colnames_snk

data[, sale_price := exp(log_sale_price)
     ][, rtm := "Y"
       ][sample(nrow(data), 20), rtm := "N"
         ][, log_sale_price := NULL
]


set.seed(628)
vars <- sample(names(data), 10)
replace_vec <- function(x) {
  x[sample(1:length(x), as.integer(runif(1, 5, 25)))] <- replace
  x
}

replace <- NA
data[, (vars) := lapply(.SD, replace_vec), .SDcols = vars]

replace <- ""
vars_char <- sample(names(data)[sapply(data, is.character)], 5)
data[, (vars_char) := lapply(.SD, replace_vec), .SDcols = vars_char]

set.seed(628)
vars_num <- sample(names(data)[sapply(data, is.numeric)], 3)
replace <- 99999
data[, (vars_num) := lapply(.SD, replace_vec), .SDcols = vars_num]

set.seed(628)
dup_rows <- data[sample(nrow(data), 35)]
data <- rbind(data, dup_rows)
property_prices_unclean <- data.table::copy(data)[, V1 := NULL]

# write data in correct format to data folder ----
usethis::use_data(property_prices_unclean, overwrite = TRUE)
