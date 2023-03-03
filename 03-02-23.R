fxx <- 2
fxy <- 4
fxz <- 0
fyy <- 2
fyz <- 2
fzz <- 2

(D1 <- fxx)
(D2 <- det(matrix(c(fxx,fxy,fxy,fyy),2,2,byrow=TRUE)))
(D3 <- det(matrix(c(fxx,fxy,fxz, fxy,fyy,fyz, fxz,fyz,fzz),3,3,byrow=TRUE)))



rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y

best_a <- sum(y*exp(-x))/sum(exp(-x)^2)

f <- function(x,a = best_a){a*exp(-x)}
plot(x,y)
x2 <- seq(-2,4,0.1)
lines(x2,f(x2))


rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
par(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
plot(x,y,pch=16)

best_a <- sum(y*log(x))/sum(log(x)^2)

f <- function(x,a = best_a){a*log(x)}
plot(x,y)
x2 <- seq(-2,4,0.1)
lines(x2,f(x2))




  

rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y

c11 <- sum(2*x) 
c12 <- sum(2*x^2)
c21 <- sum(2+0*x)
c22 <- sum(2*x)
b1 <- sum(2*y*x)
b2 <- sum(2*y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c21, c22, b2)
sol


f <- function(x,m = sol[2],b=sol[1]){b+m*x}
plot(x,y)
x2 <- seq(-5,5,0.1)
lines(x2,f(x2))

