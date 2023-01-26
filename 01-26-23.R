f <- function(x){log(3*x, base=2)-2}

x <- seq(0,6, 0.1)
plot(x, f(x))
abline(h=0)

uniroot(f, c(1,10))$root

solve_me <- function(x){f(x)-2}
uniroot(solve_me, c(1,10))$root

uniroot(function(x){f(x)-2},c(1,10))$root


f <- function(x){3*x-5}
interval <- c(1,10)
x <- seq(interval[1],interval[2], 0.1)
plot(x, f(x))

solve_me <- function(x){f(x)-0}
uniroot(solve_me, interval)$root

solve_me <- function(x){f(x)-7}
uniroot(solve_me, interval)$root

solve_me <- function(x){f(x)-exp(-x)}
uniroot(solve_me, interval)$root

solve_me <- function(x){f(x)-log(x)}
uniroot(solve_me, interval)$root



f <- function(x){x^2+x-6}
interval <- c(-5,5)
x <- seq(interval[1],interval[2], 0.1)
plot(x, f(x), type= "l")
abline(h=0)

solve_me <- function(x){f(x)-0}
uniroot(solve_me, interval)$root

solve_me <- function(x){f(x)-0}
uniroot(solve_me, c(-4,0))$root

solve_me <- function(x){f(x)-0}
uniroot(solve_me, c(0,5))$root



f <- function(x){x^2-8*x+12}
interval <- c(0,10)
x <- seq(interval[1],interval[2], 0.1)
plot(x, f(x), type= "l")
abline(h=0)

solve_me <- function(x){f(x)-0}
uniroot(solve_me, c(0,4))$root

solve_me <- function(x){f(x)-0}
uniroot(solve_me, c(4,8))$root










rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=0,a2=1){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f2(x,a0=100,a1=0,a2=0)
y1 <- f2(x,a0=100,a1=7e-4,a2=0)
y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
y3 <- f3(x,a1=-1.9,a2=0.00114)
y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)


f4(12000)
f4(12000,a0=100,a1=-1.81e-4,a2=0.83)

solve_me <- function(x){f3(x,a1=-1.9,a2=0.00114)-90}
uniroot(solve_me, c(-10000,80000))$root

solve_me <- function(x){f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)-97}
uniroot(solve_me, c(0,80000))$root

solve_me <- function(x){f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)-95}
uniroot(solve_me, c(0,80000))$root

f5(25000,a0=100,a1=6.23e-3,a2=5.06e-5)
