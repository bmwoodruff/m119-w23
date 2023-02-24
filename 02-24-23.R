library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

b <- sum((y-100)*t) 
d <- sum(t^2)
b/d


f1 <- function(x, a1 = b/d){100 + a1*x}
x <- seq(0,5000,10)
plot(t,y)
lines(x,f1(x),type = "l") 




c.11 <- sum(t^2)
c.12 <- sum(t^3)
b.1 <- sum((y-100)*t)
c.21 <- sum(t^3)
c.22 <- sum(t^4)
b.2 <- sum((y-100)*t^2)
  
y <- (c.11*b.2 - c.21*b.1)/(c.11*c.22 - c.21*c.12)
x <- (b.1-c.12*y)/c.11

y
x



solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}
solvesystem(2,3,4,5,6,7)
sol <- solvesystem(2,3,4,5,6,7)
sol[1]
sol[2]


sol <- solvesystem(c.11,c.12,b.1,c.21,c.22,b.2)
## Now we can solve the problem at hand. 
sol
a1 <- sol[1]
a2 <- sol[2]


## Define the model f2, and and create the plot. 
f2 <- function(x, a1 = sol[1], a2 = sol[2]){100 + a1*x + a2*x^2}
y <- bulb$percent_intensity
x <- seq(0,5000,10)
plot(t,y)
lines(x,f2(x),type = "l") 





# Florida Tropical Storm Data (2000-2020) from Wikipedia
year <- seq(2000,2020,1)
storms <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,3,4,5,7,4,7,13)

a <- sum(storms)
b <- sum(log(factorial(storms)))
  # You should get a=124 and b=144.5211. 
  # You'll need the factorial() when entering x! into R. 


mean(storms)

sum(storms)/21



rm(list=ls())
# Florida Tropical Storm Data (2000-2020)
storms <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,3,4,5,7,4,7,13)

L <- function(lambda,x){
  # Remember x must be a whole number.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

logL <- function(lambda,x){
  # Remember x must be a whole number.
  sum(log((lambda^x/factorial(x))*exp(-lambda)))
}


parm.l <- seq(0,10,0.001)

c <- sum(storms)
best.l <- c/21

y.L <- as.numeric(lapply(parm.l,FUN=L,x=storms))
y.logL <- as.numeric(lapply(parm.l,FUN=logL,x=storms))

par(mfrow = c(1,2), mar=c(2.5,2.5,3,0.25))
plot(parm.l,y.logL,type='l',main='logLikelihood',ylim=c(-100,-40))
abline(v=best.l,col=2)
plot(parm.l,y.L,type='l',main='Likelihood')
abline(v=best.l,col=2)

best.l
mean(storms)

