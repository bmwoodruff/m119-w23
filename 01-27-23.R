



f1 <- function(x,n=20,p=0.5){
  # x must be an whole number between 0 and n, endpoints included
  factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
}


x <- seq(0,10,1)
plot(x,f1(x, n = 10, p = 0.7))



f2 <- function(x,lambda=1){
  # x must be positive
  lambda*exp(-lambda*x)
}

x <- seq(0,10,0.1)
plot(x,f2(x, lambda = 2), type = "l")



f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}

x <- seq(-10,10,0.1)
plot(x,f3(x,mu=4,s=3.2), type = "l")

f4 <- function(x,lambda=1){
  
  out <- rep(0,length(x))
  out[(x > 0)] <- 1 - exp(-lambda*x[(x > 0)])
  
  return(out)
}


x <- seq(-10,10,0.1)
plot(x,f4(x, lambda = 1), type = "l")




f5 <- function(x,a=0,b=1){
  
  out <- rep(0,length(x))
  out[(a <= x) & (x <= b)] <- (x[(a <= x) & (x <= b)]-a)/(b-a)
  out[(x > b)] <- 1
  
  return(out)
}

x <- seq(-10,10,0.1)
plot(x,f5(x,a=-5,b=2), type = "l")









