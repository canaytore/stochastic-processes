

poissonProcessSim <- function(i, l, t, lambda) {
  x <- numeric() # initialize X
  x[1] <- i # initial state of the process
  tl <- numeric() # initialize arrival times
 
  # generate arrival times: t0 = 0, t1 = e1, t2 = e1 + e2, ..., tl = e1 + ... + el
  tl <- c(0, cumsum(rexp(l-1,lambda)))
  
  for (j in 1:length(t)) {
    for (k in 2:length(tl)) {
      if(t[j] >= tl[k-1] & t[j] <= tl[k]) {
        x[j+1] <- x[1] + (k-1)
      }
    }
  }
  # return simulated poisson process
  return(x)
}

arrivalTimes <- seq(0, 300, 1)
poissonSim <- poissonProcessSim(i = 2, l = 25, t = arrivalTimes, lambda = 0.4)

plot(poissonSim, type = 's', ylab = 'X(t)', xlab = 't', main = 'Simulated Poisson Process')

