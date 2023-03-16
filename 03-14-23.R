f0 <- function(x,a=0,b=1){1/(b-a)+0*x}
a <- 0
b <- 10

x0 <- seq(a,b,0.1)
y0 <- f0(x0,a,b)

plot(x0,y0,type='l')




f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}

x1 <- seq(-20,20,0.1)
y1 <- f1(x1, 5, 10)

plot(x1,y1,type='l')



f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

x2 <- seq(0,20,0.1)
y2 <- f2(x2,10,2)

plot(x2,y2,type='l')


f3 <- function(x,lambda=1){lambda*exp(-lambda*x)}

x3 <- seq(0,20,0.1)
y3 <- f3(x3,.2)

plot(x3,y3,type='l')

