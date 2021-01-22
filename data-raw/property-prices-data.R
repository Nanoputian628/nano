# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(HWR, dplyr, usethis, data.table)

# clean data ----
data("SydneyRealEstate")
delayedAssign("prop_dat", SydneyRealEstate)
rm(SydneyRealEstate)

setDT(prop_dat)
colnames_cam <- colnames(prop_dat)
colnames_snk <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames_cam, perl = TRUE)
colnames_snk <- sub("^(.[a-z])", "\\L\\1", colnames_snk, perl = TRUE)
colnames(prop_dat) <- colnames_snk

prop_dat[, sale_price := exp(log_sale_price)]
vars <- c("sale_price",
          "lot_size",
          "sale_qtr",
          "crime_rate",
          "income",
          "dist_to_coastline",
          "dist_to_rail_station",
          "dist_to_highway",
          "air_noise", 
          "foreigner_ratio",
          "PM10",
          "dist_to_hospital",
          "dist_to_school")
prop_dat <- prop_dat[, vars, with = FALSE]
prop_dat[, sale_qtr := as.factor(sale_qtr)]

set.seed(628)
property_prices <- prop_dat[sample(nrow(prop_dat), 500)]

# write data in correct format to data folder ----
usethis::use_data(property_prices, overwrite = TRUE)

