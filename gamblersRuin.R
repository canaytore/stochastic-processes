

simGamblersRuin <- function(gamblerInitialCap, bankInitialCap, theta) {
  # total capital involved in the game
  K <- gamblerInitialCap + bankInitialCap
  
  Xn <- numeric()
  Xn[1] <- gamblerInitialCap
  
  i <- 2
  while (Xn[i-1] > 0 && Xn[i-1] < K) {
    Xn[i] = Xn[i-1] + sample(c(1, -1), size = 1, prob = c(theta, 1-theta))
    i = i + 1
  }
  
  return(Xn)
}

gamblersRuinResults <- function(gamblerInitialCap, bankInitialCap, theta) {
  Xn <- simGamblersRuin(gamblerInitialCap, bankInitialCap, theta)
  
  if(Xn[length(Xn)] == 0) {
    print('Gambler is Bankrupt!')
  } else {
    print('Bank is Bankrupt!')
  }
  
  return(Xn)
}

gamblersRuinResults(gamblerInitialCap = 100, bankInitialCap = 5, theta = 0.9)


#################################################

simProbabilityOfRuin <- function(gamblerInitialCap, bankInitialCap, theta, n) {
  numberOfRuins <- 0 # number of games the gambler is ruined/bankrupt
  Xn <- numeric() # initialize vector to store simulated path
  
  for(i in 1:n) {
    # simulate gambler's ruin
    Xn <- simGamblersRuin(gamblerInitialCap, bankInitialCap, theta)
    
    if(Xn[length(Xn)] == 0) { # if gambler is bankrupt add 1 to numberOfRuins
      numberOfRuins <- numberOfRuins + 1
    }
  }
  
  # return the probability of ruin
  return(numberOfRuins/n)
}

simProbabilityOfRuin(gamblerInitialCap = 100, bankInitialCap = 10, theta = 0.5, n = 30)



