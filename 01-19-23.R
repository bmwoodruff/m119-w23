f <- function(x){3^(-x)}
x <- seq(-2,2,0.01)
y <- f(x)
plot(x,y, type = "l")

exp(2)
exp(1)

x <- seq(-4,20,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')

x <- seq(-4,200,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')


library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)
hist(dist$percent_intensity)
