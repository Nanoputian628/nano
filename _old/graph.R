oneway(data,class)

format

library(plotly)


data("property_prices")
data <- property_prices
data
m<-ls(data)
m
y<-data[,"sale_price"]
x<-data[,"sale_qtr"]

fig <- plot_ly(data, y=~sale_price, x=~sale_qtr, type = 'scatter', mode = 'markers')

fig <- plot_ly(data, x=~sale_qtr, y=~sale_price, name = 'sale_qtr', 
               type = 'scatter', mode = 'markers',
               marker = list(color = 'green', width = 1))

fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')

plot_ly(y=y, x=x, type = 'scatter', mode = 'lines')


#NEW

weight_all_12 <- mvs_all_weight[[16]]


graphx <- plot_ly()
graphxy <- add_trace(graphx,
                         x = data[["sale_qtr"]],
                         type = "histogram") %>% 
  layout(title = "Histogram",
         xaxis = list(title = "Weight in Portfolio",
                      zeroline = FALSE),
         yaxis = list(title = "Count",
                      zeroline = TRUE))
graphxy



#testtt

import plotly.express as px
df = px.data.tips()
fig = px.histogram(df, x="total_bill", nbins=20)
fig.show()

