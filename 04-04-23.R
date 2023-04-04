f <- function(x){(x-3)^2+1}
t <- seq(0,6,.1)
plot(t,f(t),type = "l")


Dfapprox <- function(x,h=0.00000001){(f(x+h)-f(x))/h}
Dfapprox(3)
Df <- function(x){Dfapprox(x)}

rate <- 0.1

x[1] <- 0.2
x[1]
n <- 50
for (i in 1:n) {
  x[i+1] <- x[i] - Df(x[i])*rate
}
x[n+1]

gradientdescent <- function(f, start, rate = 0.1, n = 50 ){
  Dfapprox <- function(x,h = 0.00000001){(f(x+h)-f(x))/h}
  Df <- function(x,h = 0.00000001){Dfapprox(x)}
  x[1] <- start
  for (i in 1:n) {
    x[i+1] <- x[i] - Df(x[i])*rate
  }
  x[n+1]
}

gradientdescent(f,start = 0.2, rate = 0.1, n=100)


gradientdescent(function(x){x^2-7*x+12},start = 0, rate = 0.1, 200)

f <- function(x){x^2-7*x+12}
plot(t,f(t))
