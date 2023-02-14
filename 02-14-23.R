A <- function(w){(300-w)/2*w}
A(10)

w <- seq(0,300,1)
plot(w, A(w), type = "l")


A <- function(h){(h)*(300-2*h)}
A(10)

h <- seq(0,150,1)
plot(h, A(h), type = "l")
