
x <- c(2,3,4,5,6,7,8,9,10,11,12)
p <- c(1,2,3,4,5,6,5,4,3,2,1)/36

sum(x*p)



#Shades a rug diagram for a probability mass function. 
#Inputs: 
#  x - a vector of data points
#  p - a corresponding vector of probabilities or frequencies
#All widths are 1 unit wide. 
draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

x <- c(-10,1,2)
p <- c(0.1,0.6,0.3)
draw_pmf(x,p)

x <- seq(1,6)
p <- rep(1/6,6)
draw_pmf(x,p)

x <- seq(1,6)
p <- c(1/21,2/21,3/21,4/21,5/21,6/21)
draw_pmf(x,p)
