f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root
uniroot(f, interval)
my_root
#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")


f <- function(x){log(4*x-2)-5}
interval <- c(20,50)
my_root <- uniroot(f,interval)$root
x <- seq(20,50)
plot(x,f(x))
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")

f <- function(x){log(x+5)+log(x)-log(12*x)}
x <- seq(0,100,0.1)
plot(x,f(x), type = "l")
abline(h=0,col = "gray")
interval <- c(1,10)
uniroot(f,interval)



f <- function(x){x^3-x^2}
x <- seq(-5,5,0.1)
plot(x,f(x), type = "l")
abline(h=0,col = "gray")
interval <- c(-5,5)
uniroot(f,interval)$root


x <- seq(0.5,1.5,0.1)
plot(x,f(x), type = "l")
abline(h=0,col = "gray")
interval <- c(0.5,1.5)
uniroot(f,interval)$root


x <- seq(-.5,.5,0.1)
plot(x,f(x), type = "l")
abline(h=0,col = "gray")
interval <- c(-.5,.5)
uniroot(f,interval)$root

uniroot(f,c(10,40))


g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,10))$root

x <-seq(0,50,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")
