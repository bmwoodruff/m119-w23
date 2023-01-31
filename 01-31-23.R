library(data4led)
bulb <- led_bulb(seed = 123)
t <- bulb$hours
y <- bulb$percent_intensity

plot(t,y)

f1 <- function(x, a0 = 100, a1){a0+a1*x}
x <- seq(0,5000)
a1 <- 0.0006
plot(t,y)
lines(x,f1(x, a1 = a1))

ri <- y - f1(t,a1 =a1)
max(abs(ri))
hist(ri)






p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

data <- c(8,2,8,8,4)
lambda <- 9
p.3v1(data,lambda)

L <- function(lambda, data){p.3v1(data,lambda)}

lambda <- seq(1,10,0.1)
plot(lambda, L(lambda, data), type = "l")


#grab from class activity prep for the day
lambda <- seq(0,15,0.01)
data <- c(8,2,8,8,4)
p <- sapply(lambda, p.3v1, x = data)
plot(lambda,p,type='l')
mean(data)

lambda <- seq(5,7,0.01)
p <- sapply(lambda, p.3v1, x = data)
plot(lambda,p,type='l')



lambda <- seq(0,15,0.01)
data <- c(8,6,4,3,2,4,6)
p <- sapply(lambda, p.3v1, x = data)
plot(lambda,p,type='l')





rm(list=ls())

###Define the distribution###
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

###Define the likelihood function###
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Florida Hurricane Data (2000-2022)
data <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,2,4,6,7,4,7,13,3,3)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LP,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Poisson Likelihood')
