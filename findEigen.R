
# Find eigen values
M <- matrix(c(0.3,0.2,0,
              0.3,0.6,0,
              0.4,0.2,1), 
            nrow = 3, ncol = 3)

powEigen <- function(M, n) {
  eigen <- eigen(t(M))
  eigenValue <- eigen$values
  eigenVector <- eigen$vectors
  Q <- as.matrix(eigenVector)
  Dm <- diag(eigenValue^n)
  Mres <- Q %*% Dm %*% solve(Q)
  return (t(Mres))
}

findEigen <- function(M){
  eigen <- eigen(t(M))
  eigen$values
}

# Power 40th
Mres <- powEigen(M, 40); Mres
findEigen(Mres)

