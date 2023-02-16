f <- function(x){sqrt(x^2+(8/x)^2)}
f(0)
f(1)
f(2)

curve <- function(x){8/x}

x <- seq(-100,7,1)
plot(x,curve(x))

x <- seq(0,7,0.01)
plot(x,curve(x), ylim = c(0,10))
plot(x,f(x), ylim = c(0,10))

fp <- function(x){
  1/2*(x^2+(8*x^-1)^2)^(-1/2)*(2*x+2*(8*x^-1)^1*(-8*x^(-2)))
}
fp(2)
fp(3)

uniroot(fp,c(2,3))$root

minx <- 2.828428
plot(x,f(x), ylim = c(0,10), type="l")
abline(v=minx)
