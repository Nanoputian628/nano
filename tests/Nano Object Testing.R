# parameter ----
response <- "sale_price"
path <- "C:\\Users\\dilsh\\Documents\\R Packages\\data\\property_prices_2011_raw.csv"

# read data 
data <- data.table::fread(path)

# fix column names
colnames_cam <- colnames(data)
colnames_snk <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames_cam, perl = TRUE)
colnames_snk <- sub("^(.[a-z])", "\\L\\1", colnames_snk, perl = TRUE)
colnames(data) <- colnames_snk

# clean data
rm_vars <- c("V1", "log_sale_price", "crime_density", "dist_to_coastline", "dist_to_rail_line", "dist_to_tunnel", "dist_to_gPO", "NO2", "SO2", "dist_to_ferry")   
data[, sale_price := exp(log_sale_price)
     ][, sale_qtr := as.factor(sale_qtr)
       ][, post_code := as.factor(post_code)
        ][, (rm_vars) := NULL]
data <- data[sale_price < 2000000 | is.na(sale_price)]

set.seed(2020)
data <- data[sample(nrow(data), 1000)]


### Build Models

## Random Forest
hyper_params <- list(ntrees                   = 500,
                     max_depth                = 5,
                     min_rows                 = 10,
                     distribution             = "gamma",
                     sample_rate              = 0.8,
                     min_split_improvement    = 1e-05)

nano <- nano::nano_grid(response     = response, 
                        grid_id      = "drf_1",
                        algo         = "drf", 
                        data         = data, 
                        nfolds       = 5, 
                        hyper_params = hyper_params)


## GBM
hyper_params <- list(ntrees                   = 500,
                     max_depth                = 5,
                     min_rows                 = 10,
                     distribution             = "gamma",
                     learn_rate               = 0.01,
                     sample_rate              = 0.8,
                     col_sample_rate          = 0.6,
                     col_sample_rate_per_tree = 0.8, 
                     min_split_improvement    = 1e-05)

nano <- nano::nano_grid(nano         = nano,
                        response     = response, 
                        grid_id      = "gbm_1",
                        algo         = "gbm", 
                        data         = data, 
                        nfolds       = 5, 
                        hyper_params = hyper_params)

plot(nano$model$model_2, timestep = "number_of_trees", metric = "deviance") 


## Neural network
nano <- nano::nano_grid(nano         = nano,
                        response     = response, 
                        grid_id      = "nn_1",
                        algo         = "deeplearning",
                        distribution = "gamma",
                        data         = data, 
                        nfolds       = 5)
plot(nano$model$model_3, timestep = "epochs", metric = "deviance") 


## GLM
nano <- nano::nano_grid(nano         = nano,
                        response     = response, 
                        grid_id      = "glm_gau",
                        algo         = "glm",
                        family       = "gaussian",
                        link         = "identity",
                        data         = data, 
                        nfolds       = 5)

nano <- nano::nano_grid(nano             = nano,
                        response         = response, 
                        grid_id          = "glm_gam",
                        algo             = "glm",
                        family           = "gamma",
                        link             = "log",
                        lambda           = 0,
                        compute_p_values = TRUE,
                        data             = data, 
                        nfolds           = 5)

h2o.coef_norm(nano$model$model_5)
nano$model$model_5@model$coefficients_table
nano::nano_metrics(nano, 1:5, "cv")

## target encoding on postcode
data_process <- data_prep(data          = data,
                          response      = response,
                          split_or_fold = 5,
                          target_encode = TRUE,
                          blend         = TRUE,
                          encode_cols   = "post_code",
                          noise         = 20000)
data_te <- data.table::copy(data_process$data)

hyper_params <- list(ntrees                   = 500,
                     max_depth                = 5,
                     min_rows                 = 10,
                     distribution             = "gamma",
                     learn_rate               = 0.01,
                     sample_rate              = 0.8,
                     col_sample_rate          = 0.6,
                     col_sample_rate_per_tree = 0.8, 
                     min_split_improvement    = 1e-05)

nano <- nano::nano_grid(nano         = nano,
                        response     = response, 
                        grid_id      = "gbm_te",
                        algo         = "gbm", 
                        data         = data_te,
                        nfolds       = 5,
                        hyper_params = hyper_params)
nano::nano_metrics(nano, 1:6, "cv")



## Naive Bayes (not comparable with other models but for the purpose of illustration)

nano <- nano::nano_grid(nano         = nano,
                        response     = "sale_qtr", 
                        grid_id      = "nb_1",
                        algo         = "naivebayes",
                        data         = data, 
                        nfolds       = 5)
nano::nano_metrics(nano, 6, "cv")


## PDP
nano <- nano::nano_pdp(nano     = nano,
                       model_no = 1:5,
                       vars     = c("longitude", "crime_rate"))

pdp <- rbind(nano$pdp$pdp_1, nano$pdp$pdp_2, nano$pdp$pdp_3, nano$pdp$pdp_4, nano$pdp$pdp_5)
pdp <- pdp[, .(var_band, mean_response, var)]
pdp[, model_id := rep(c(nano$model$model_1@model_id, nano$model$model_2@model_id, nano$model$model_3@model_id, nano$model$model_4@model_id, nano$model$model_5@model_id), each = 40)]

pdp_plot <- function(pdp, vars) {
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
                                legendgroup = ~model_id,
                                mode = "lines+markers",
                                showlegend = TRUE) %>% 
                          layout(xaxis = list(title = var,
                                              hoverformat = ",.2s"),
                                 yaxis = list(hoverformat = ",.2s"),
                                 legend = list(orientation = "h")))
    pdps[[var]] <- fig
  }
  return(pdps)
}

# plots pdps
pdp_plots <- pdp_plot(pdp, unique(pdp$var))
pdp_plots$longitude
pdp_plots$crime_rate
