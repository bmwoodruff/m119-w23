data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2


c11 <- 50
c12 <- sum(x)
c21 <- c12
c22 <- sum(x^2)
b1 <- sum(y)
b2 <- sum(x*y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c12, c22, b2)
best_b <- sol[1] 
best_m <- sol[2] 

best_b
best_m


fxx <- -c11 
fxy <- -c12 
fyy <- -c22
D <- fxx*fyy - fxy^2
D
fxx


h <- function(x, b = best_b, m = best_m){b + m*x}

x_in <- seq(min(x),max(x),0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x_in,h(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')




rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y1 <- data$y1


m_best <- sum(x*y1)/sum(x^2)


f <- function(x,b=0,m=m_best){b + m*x}

x_in <- seq(-10,10,0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y1,type='p',pch=16)
lines(x_in,f(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')



L <- function(m,x,y){ prod((1/sqrt(2*pi))*exp(-(y-m*x)^2/2)) }
logL <- function(m,x,y){ log(prod((1/sqrt(2*pi))*exp(-(y-m*x)^2/2)))}

x_val <- seq(-2,2,0.01)
y_L <- sapply(x_val,FUN=L,x=x,y=y1)
y_logL <- sapply(x_val,FUN=logL,x=x,y=y1)

par(mfrow=c(1,2))
par(mar=c(2.5,2.5,0.25,0.25))
plot(x_val,y_L,type='l')
abline(v=m_best,col=4)
plot(x_val,y_logL,type='l')
abline(v=m_best,col=4)



rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice2.csv"))

x <- data$x
y1 <- data$y1

b_best <- sum(y1 *exp(-x))/sum(exp(-2*x))
b_best


f2 <- function(x,b = b_best){b*exp(-x)}

x_in <- seq(-10,10,0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y1,type='p',pch=16)
lines(x_in,f2(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')





c11 <- sum(1+x*0)
c12 <- sum(exp(-x))
c21 <- c12
c22 <- sum(exp(-x)^2)
b1 <- sum(y1)
b2 <- sum(exp(-x)*y1)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c12, c22, b2)
best_a <- sol[1] 
best_b <- sol[2] 

best_a
best_b


fxx <- -c11 
fxy <- -c12 
fyy <- -c22
D <- fxx*fyy - fxy^2
D
fxx


f2 <- function(x,a=best_a,b = best_b){a+b*exp(-x)}

x_in <- seq(-10,10,0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y1,type='p',pch=16)
lines(x_in,f2(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')

