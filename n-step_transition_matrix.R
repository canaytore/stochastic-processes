

# function for calculating matrix powers
matrixPower <- function(mat, n) {
  matPow <- mat # initialize matrix
  if(n == 0) {
    # return matrix to the power 0
    return(1)
  } 
  else if(n == 1) {
    # return matrix to the power of 1 (i.e. initial matrix)
    return(matPow)
  } 
  else {
    for (i in 1:(n-1)) {
      # calculate matrix powers
      matPow <- matPow %*% mat
    }
    # return mat to the power n
    return(matPow)
  }
}

# MC n-step probability distribution given initial distribution
markov <- function(initialDistribution, probabilityMatrix, n) {
  # n-step probability matrix
  nstepProbabilityMatrix <- matrixPower(probabilityMatrix, n)

  # probability distribution given initial distribution
  probabilityDistribution <- initialDistribution %*% nstepProbabilityMatrix
  
  return(nstepProbabilityMatrix)
}
 
init <- c(1,0,0)
P <- matrix(c(0.2,0.5,0.3,
              0,0,1,
              0.1,0.5,0.4),
              nrow=3, byrow=T)
P

markov(init, P, 1) # 1st degree
markov(init, P, 2)
markov(init, P, 3)
markov(init, P, 4)
markov(init, P, 5) # 5th degree

