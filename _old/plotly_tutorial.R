# https://plotly.com/r/

# Follow examples from above link. Has heaps of different grpahs you can make using plotly 
# and has all the code you need



library(nano)
library(plotly)

# load data and band some variables in data for illustration purposes
data("property_prices")
y <- band_data(property_prices, list(crime_rate = seq(0, 1, 0.1), dist_to_coastline = seq(0, 50, 10)))
# select categorical variables
y <- y[, .(sale_qtr, crime_rate_bnd, dist_to_coastline_bnd, sale_price)]

# variables to plot one ways off
vars <- setdiff(names(y), "sale_price")

# initialise empty list to hold one way plots for each variable
# note: when initialising a variable, always allocate the correct number of spaces at the beginning
plots <- rep(list(NA), length(vars))
# rename elements of the list
names(plots) <- vars

# for loop to create bar plot for each variable in "vars"
for (var in vars) {
  # summarised by "var" and calculates the count and mean sale_price
  x <- y[, .(count = .N, mean = mean(sale_price)), by = var]
  # plot bar graph
  fig <- plot_ly(
    x = x[[var]],
    y = x$mean,
    name = "SF Zoo",
    type = "bar"
  )
  # save in list
  plots[[var]] <- fig
}
# show plots
plots$sale_qtr
plots$crime_rate_bnd
plots$dist_to_coastline_bnd



### Example of Multi Graph ###

# calculate count and mean by sale_qtr
x <- y[, .(count = .N, mean = mean(sale_price)), by = sale_qtr]

# parameters for second axis
ay <- list(tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "second y axis"
)

# initialise empty plot
fig <- plot_ly()
# add first layer of plots - bar graph of mean sale_price
fig <- fig %>% add_bars(x = x$sale_qtr, y = x$mean, name = "Average sale_price")
# add second layer of plots - line graph of count (right y-axis)
fig <- fig %>% add_lines(x = x$sale_qtr, y = x$count, name = "Count of sale_qtr", yaxis = "y2")
# set layout so bar graph uses left axis and line graph uses right axis
fig <- fig %>% layout(
  title = "Double Y Axis", yaxis2 = ay,
  xaxis = list(title="x")
)
fig




###################################

pdp_plot <- function(nano, model_nos, vars) {
  
  #########
pdp <- rbind(nano$pdp$pdp_1, nano$pdp$pdp_2, nano$pdp$pdp_3, nano$pdp$pdp_4)
for (i in model_nos) {
  #action
}
pdp <- pdp[, .(var_band, mean_response, var)]
pdp[, model_id := rep(c(nano$model$model_1@model_id, nano$model$model_2@model_id, nano$model$model_3@model_id), each = 180)]
###########

  pdps <- rep(list(NA), length(vars))
  for (var in vars) {
    pdp <- as.data.frame(pdp)
    dat <- pdp[pdp$var == var,]
    fig <- nano:::quiet(plot_ly(data = dat,
                                x = ~var_band, 
                                y = ~mean_response, 
                                group_by = ~model_id,
                                type = "scatter", 
                                color = ~model_id, 
                                mode = "lines+markers") %>% 
                          layout(xaxis = list(title = var)))
    pdps[[var]] <- fig
  }
  return(pdps)
}

# plots pdps
pdp_plots <- pdp_plot(pdp, unique(pdp$var))


pdp_plot(nano, model_nos, vars)



