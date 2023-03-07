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

x <- c(  1,  2,  3,  4)
p <- c(0.1,0.3,0.3,0.3)
draw_pmf(x,p)


x <- c(1,2,3,4)
p <- c(.1,.3,.3,.3)
sum(x*p)
my_table <- data.frame(x = x, prob = p)
my_table

#If we drew (with replacement) a slip of paper
#from this bag many times, and each time recorded the result,
#then the average of our results would be 2.8. 
