

# simple random walk simulation
simpleRandomWalk <- function(initialState, n, theta) {
  yt <- numeric(n)
  yt[1] <- initialState # initial state 
  
  if(theta > 1 || theta < 0) { # probabilities are between 0 and 1
    return('Invalid value for theta')
  } 
  else {
    # stepSize := et = 1 or -1 with P(stepSize = 1) = theta, and P(stepSize = -1) = 1-theta
    stepSize = sample(c(1, -1), size = n, prob = c(theta, 1-theta), replace = TRUE)
    
    for(i in 2:length(yt)) {
      yt[i] = yt[i-1] + stepSize[i]
    }
    
    return(yt)
  }
}

expectedValueSRW <- function(initialState, n, theta) {
  if(theta > 1 || theta < 0) {
    return('Invalid value for theta')
  } 
  else {
    # expected value of Xn given Xo in a simple random walk
    expectedValueXn <- initialState + abs(n)*(2*theta - 1)
    return(expectedValueXn)
  }
}

expectedValueVectorSRW <- function(initialState, n, thetas) {
  # initialize an empty expected values vector that's going to
  # store a collection of expected values for different values of theta
  expectedValueVector <- numeric()
  for (i in 1:length(thetas)) {
    expectedValueVector[i] <- expectedValueSRW(initialState, n, thetas[i])
  }
  
  return(expectedValueVector)
}

simExpectedValueSRW <- function(initialState, n, theta) {
  simulatedExpectedValue <- numeric()
  
  for (i in 1:n) {
    simulatedExpectedValue[i] <- mean(simpleRandomWalk(initialState, n, theta))
  }
  
  # return a simulated Expected value of Xn given Xo
  return(mean(simulatedExpectedValue))
}

expectedValueVectorSRW(1,5,0.5)
simExpectedValueSRW(1,5,0.5)


varianceSRW <- function(n, theta) {
  if(theta > 1 || theta < 0){
    return('Invalid value for theta')
  } 
  else {
    # variance of Xn given xo in a simple random walk
    varianceXn <- 4*n*theta*(1 - theta)
    return(varianceXn)
  }
}

varianceVectorSRW <- function(n, thetas) {
  # initialize an empty variance vector that's going to
  # store a collection of variances for different values of theta
  varianceVector <- numeric()
  
  for(i in 1:length(thetas)) {
    varianceVector[i] <- varianceSRW(n, thetas[i])
  }
  
  return(varianceVector)
}
