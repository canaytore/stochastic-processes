"
The Yule process arises in physics and biology and describes the growth of a population 
in which each member has a probability of giving birth to a new member during an interval 
of time of length.

The total population birth rate is directly proportional to the population size, the proportionality 
constant being the individual birth rate. As such, the Yule process forms a stochastic analog of 
the deterministic population growth model.
"

yuleProcessSim <- function(i, l, t) {
  x <- numeric() # initialize X
  x[1] <- i # initial state of the process
  tl <- numeric() # initialize arrival times
  
  tl <- c(0, cumsum(rexp(l-1,rep(i, l-1) + 1:(l-1))))
  
  for (j in 1:length(t)) {
    for (k in 2:length(tl)) {
      if(t[j] >= tl[k-1] & t[j] <= tl[k]) {
        x[j+1] <- x[1] + (k-1)
      }
    }
  }
  # return simulated Yule process
  return(x)
}

arrivalTimes <- seq(0, 3000, 1)
yuleSim <- yuleProcessSim(i = 1, l = 500, t = arrivalTimes)

plot(yuleSim, type = 'l', ylab = 'X(t)', xlab = 't', main = 'Simulated Yule Process')
