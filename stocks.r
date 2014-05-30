require(ggplot2)

#set seed for replicability
set.seed(955)

# t = time, from period 1 to 100
t <- 1:100 

# price is a noisy function of t
price <- exp(0.05*(t))/3 + rnorm(100,sd=3) + 15

# append t and price to a data frame object
data <- as.data.frame(cbind(t,price))

# plot the relationship
my_plot <- ggplot(data, aes(x=t, y=price)) +geom_line(color="blue") +geom_smooth(method="lm", color="purple", fill="pink") + ggtitle("Plot 1:\n  Stock Price vs. Time")
# my_plot

# append a new column, ln(price) to the data frame object
data$ln_price <- log(data$price)

my_plot2 <- ggplot(data, aes(x=t, y=ln_price)) +geom_line(color="blue") + geom_smooth(method="lm", color="purple", fill="pink") + ggtitle("Plot 2:\n  ln(Stock Price) vs. Time")
# my_plot2

fit <- lm(ln_price ~ t, data=data)
data$fitted <- fit$fitted

my_plot3 <- ggplot(data, aes(x=t)) +geom_line(aes(y=price, color="blue")) + geom_line(aes(y=exp(fitted), color="green")) + ggtitle("Plot 3:\n  Fit vs. Actual")
my_plot3

ggsave(file="stock.svg", plot=my_plot3, height=10, width=10)

# calculate cc returns just for fun
# data$cc_returns <- c(diff(log(data$price)),NA)

#plot cc returns, just for fun
# notice that it's hard to fit a line to returns

# my_plot4 <- ggplot(data, aes(x=t, y=cc_returns)) + geom_line(color="purple") + geom_smooth(color="green") + ggtitle("Plot 3:\n CC Returns")
# my_plot4
