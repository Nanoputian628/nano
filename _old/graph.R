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





#start again


data("property_prices")
#data <- property_prices
#data
#m<-ls(data)
#m

y<-data[,"sale_price"]
x<-data[,"sale_qtr"]

fig <- plot_ly(data, y=~sale_price, x=~sale_qtr, type = 'scatter', mode = 'markers')

fig <- plot_ly(data, y=~sale_price, x=~sale_qtr, type = 'scatter', mode = 'markers')


fig <- plot_ly(data, y=~sale_price/sale_qtr, x=~sale_qtr, type = 'histogram')
fig

summary(x)

p1 <- plot_ly(data, y = ~sale_price, type = "histogram")
p1 

p2 <- plot_ly(y = ~sale_qtr, type = "histogram")
p2 

p3 <- plot_ly(y = ~income, type = "histogram")
p3 

p4 <- plot_ly(y = ~lot_size, type = "histogram")
p4 



# Simple Bar Plot
counts <- table(data$sale_qtr)
barplot(counts, main="sale_qtr",
        xlab="sale_qtr")


vars <- names(data)
plots <- list()
for(var in vars) {
  counts <- table(data[[var]]) #data[, p, with = FALSE]
  plots[[var]] <- function() {barplot(counts, main = var, xlab = var)}
}
plots$income()

a<- plot(1:2, 1:2)
a 
