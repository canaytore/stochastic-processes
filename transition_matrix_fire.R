"
The Canadian forest fire weather index is widely as means of to estimate 
the risk of wildfire. The Ontario Ministry of Natural Resources uses the 
index to classify each day's risk of forest fire as either nil, low, 
moderate, high or extreme. Transition probability matrix for the five 
state Markov chain for the daily changes in the index is given as:
"

A <- matrix(c(0.575, 0.118, 0.172, 0.109, 0.026,
              0.453, 0.243, 0.148, 0.123, 0.033,
              0.104, 0.343, 0.367, 0.167, 0.019,
              0.015, 0.066, 0.318, 0.505, 0.096,
              0.000, 0.060, 0.149, 0.567, 0.224),
            nrow=5, byrow=TRUE)
states <- c("Nil", "Low", "Moderate", "High", "Extreme")
rownames(A) <- states
colnames(A) <- states
A

X <- diag(5)
n <- 100
for (i in 1:n) {
  X <- A %*% A
}
X
