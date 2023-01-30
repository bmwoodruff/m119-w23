p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(x=1, lambda =1)
p(1,1)
p(0,1)
p(2,1) + p(3,1)+p(4,1)
x <- seq(2, 10,1)
sum(p(x,1))

x <- seq(0, 100,1)
sum(p(x,1))

1 - (p(1,1)+p(0,1))



x <- seq(0, 7,1)
sum(p(x,3))

x <- seq(0, 7,1)
sum(p(x,6))
1-sum(p(x,6))

lambda <- seq(6.9,7.1,0.01)
p(x=7,lambda)

plot(lambda,p(x=7,lambda))


lambda <- seq(0,10,0.1)
plot(lambda,p(x=7,lambda), type = "l")


lambda <- seq(6.99,7.01,0.0001)
plot(lambda,p(x=7,lambda), type = "l")


lambda <- seq(0,10,0.001)
plot(lambda,p(x=4,lambda), type = "l")


p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after (using $\lambda = 2$ as assumed).
p.3v1(c(4,4,8))
#The same probability as above, using the other version
p.3v2(4,4,8)
#The same probabilty as above by just multiplying probabilities of independent events together. 
p(4)*p(4)*p(8)

#The probability of 2 Florida tropical storms this year, 5 Florida tropical storms next year, and 3 Florida tropical storms the year after.
p.3v1(c(2,5,3))

#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after, assuming the parameter $\lambda = 5$.
p.3v1(c(4,4,8),5)

#Repeat the above, but with lambda = 1, and then with lambda = 10
p.3v1(c(4,4,8),1)
p.3v1(c(4,4,8),10)

p.3v1(c(4,4,8),3)
p.3v1(c(4,4,8),4)
p.3v1(c(4,4,8),5)
p.3v1(c(4,4,8),6)
p.3v1(c(4,4,8),7)
p.3v1(c(4,4,8),8)

p.3v1(c(4,4,8),4)
p.3v1(c(4,4,8),4.5)
p.3v1(c(4,4,8),5)
p.3v1(c(4,4,8),5.5)
p.3v1(c(4,4,8),6)

lambda <- seq(5.3333, 5.3334, 0.000001)
x <- c(4,4,8)
p <- sapply(lambda, p.3v1, x = x)
plot(lambda,p,type='l')
