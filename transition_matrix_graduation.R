"
University administrators have developed a Markov model to
simulate graduation rates at their school. Student might drop
out, repeat a year or move on to the next year. Student have
a 3% chance of repeating a year. First years and second years
have a 6% of dropping out. For third years and fourth years
the drop out rate is 4%. The transition matrix for the model
is P. Simulate the long term probability that a new student
graduates.
"

P <- matrix(c(1, 0, 0, 0, 0, 0,
              .06, .03, .91, 0, 0, 0, 
              .06, 0, .03, .91, 0, 0, 
              .04, 0, 0, .03, .93, 0, 
              .04, 0, 0, 0, .03, .93, 
              0, 0, 0, 0, 0, 1),
            nrow = 6, ncol = 6, byrow = TRUE)

n <- 10000
x <- array(dim = n)
for(i in 1:n) {
  yr <- 2
  while(TRUE) {
    yr <- sample(c(1:6), 1, replace = F, P[yr,])
    
    if(yr==1) {
      grad <- 0
      break
    }
    
    if(yr==6) {
      grad <- 1
      break
    }
  }
  
  x[i] <- grad
}

prob <- sum(x)/n; prob
