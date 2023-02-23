b11 <- pi 
b12 <- log(2)
c1  <- 7
i <- seq(1,3)
b21 <- sum(i^2)
b22 <- sum(i-1)
c2  <- sum(i-i^2)

x <- (c1*b22-b12*c2)/(b11*b22-b12*b21) 
y <- (b11*c2-c1*b21)/(b11*b22-b12*b21) 

a1 <- x
a2 <- y

a1
a2


#Check
b11*x+b12*y 
c1
b11*x+b12*y == c1
b21*x+b22*y == c2


all.equal(b11*x+b12*y,c1)
all.equal(b21*x+b22*y,c2)



n <- seq(1,44)
b11 <- sum(n)
b12 <- 3*44
c1  <- sum(7+0*n)
b21 <- sum(5+0*n)
b22 <- sum(n)
c2  <- sum(n^2)

x <- (c1*b22-b12*c2)/(b11*b22-b12*b21) 
y <- (b11*c2-c1*b21)/(b11*b22-b12*b21) 
x
y


#Check
b11*x+b12*y 
c1
b11*x+b12*y == c1
b21*x+b22*y == c2


all.equal(b11*x+b12*y,c1)
all.equal(b21*x+b22*y,c2)

