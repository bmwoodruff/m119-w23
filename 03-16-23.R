draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}

#Draws rectangles over the top of a given function.
#The midpoint of top of each rectangle passes through the function. 
#  f - a function f(x)
#  a - left end of graph
#  b - right end of graph
#  num_rectangles - how many rectangles to plot.
#  method - One of "left", "right", or "mid".  Defaults to mid.
draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}

g <- function(x){ifelse(x<2.5,2,4)}
draw_rug(g,0,4,num_points=1000)  


xi <- c(1.25,3.25)
Ai <- c(5,6)
c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))


f <- function(x){4/x}
a <- 1
b <- 5
n <- 8
draw_rect_approx(f,a,b,n,method = "left")


dx <- (b-a)/n
xi <- seq(a,b-dx,dx)
c(riemann_sum_using_left_endpoints = sum(f(xi)*dx))






rm(list=ls())
library(data4soils)
Ng <- cfbp_fpjuliet$ng

mean(Ng)
var(Ng)


f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}

mu <- mean(Ng)
sigma <- sqrt(var(Ng))
#sigma <- sd(Ng)

x1 <- seq(-20,20,0.1)
y1 <- f1(x1,m=mu,s=sigma)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Normal",xlim=c(-5,12))
lines(x1,y1,col=2)




f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

alpha <- mean(Ng)^2/var(Ng)
alpha
beta <- mean(Ng)/var(Ng)
beta

x2 <- seq(0,20,0.1)
y2 <- f2(x2,a=alpha,b=beta)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Gamma", breaks = 20)
lines(x2,y2,col=2)



m = mean(Ng)
s = sqrt(var(Ng))

set.seed(123)
tmp <- rnorm(25000, mean=m, sd=s)
tmp

x <- length(which(tmp > 10))
p <- x/length(tmp)
p

