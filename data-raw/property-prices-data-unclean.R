# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(usethis, data.table)

# clean data ----
data <- data.table::fread("C:\\Users\\dilsh\\Documents\\R Packages\\data\\property_prices_2011_raw.csv")


colnames_cam <- colnames(data)
colnames_snk <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames_cam, perl = TRUE)
colnames_snk <- sub("^(.[a-z])", "\\L\\1", colnames_snk, perl = TRUE)
colnames(data) <- colnames_snk

# enrich data
set.seed(2020)
data[, sale_price := exp(log_sale_price)
     ][sample(nrow(data), 30), sale_price := NA
       ][, rtm := "Y"
         ][sample(nrow(data), 30), rtm := "N"
           ][, log_sale_price := NULL
]

# filter out any property prices greater than $2M
data <- data[sale_price < 2000000 | is.na(sale_price)]

set.seed(2020)
vars <- sample(names(data), 10)
replace_vec <- function(x) {
  x[sample(length(x), as.integer(runif(1, 5, 30)))] <- replace
  x
}

replace <- NA
data[, (vars) := lapply(.SD, replace_vec), .SDcols = vars]

replace <- ""
vars_char <- "rtm"
data[, (vars_char) := lapply(.SD, replace_vec), .SDcols = vars_char]

set.seed(2020)
vars_num <- sample(names(data)[sapply(data, is.numeric)], 3)
replace <- 999999
data[, (vars_num) := lapply(.SD, replace_vec), .SDcols = vars_num]

set.seed(2020)
dup_rows <- data[sample(nrow(data), 50)]
data <- rbind(data, dup_rows)
property_prices_unclean <- data.table::copy(data)[, V1 := NULL]

# write data in correct format to data folder ----
usethis::use_data(property_prices_unclean, overwrite = TRUE)
