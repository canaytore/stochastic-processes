# Gambler's Ruin
# A gambler's ruin. If P1 begins with $3 and P2 with $2, and 
# P1 has a .46 chance of winning while P2 has .54, then the
# probability that either can win can be described as a matrix with
# six states (0, 1, 2, 3, 4, 5). State 1 would mean that P1 has $0
# and has reached failure, and State 6 means that P1 has $5 and won.

P <- matrix(c(1, 0, 0, 0, 0, 0,
              .54, 0, .46, 0, 0, 0,
              0, .54, 0, .46, 0, 0,
              0, 0, .54, 0, .46, 0,
              0, 0, 0, .54, 0, .46,
              0, 0, 0, 0, 0, 1),
            nrow=6, byrow=T) 
P

# Multiplying this matrix several times will give us a clear indication
# of P1's chance of failure, given any amount P1 starts with.
P2 <- P %*% P; round(P2, 3)
P4 <- P2 %*% P2; round(P4, 3)
P8 <- P4 %*% P4; round(P8, 3)
P16 <- P8 %*% P8; round(P16, 3)
P32 <- P16 %*% P16; round(P32, 3)
P64 <- P32 %*% P32; round(P64, 3) 

# After multiplying the matrix six times against itself, we get a clear 
# indication of P1's probability of failure. P2 has a .498 chance of failure 
# if P1 starts the match with $3, and P2 has a .502 chance of winning 
# starting with $2. 
# Now we will verify the above results through simulation;

# Gambler's Ruin Simulation
n <- 1000
t = a = numeric(n)
y <- rbinom(1, 1, .54)

for (i in 1:n) {
  x = cs = 3                      # start in state 3
  while ((cs > 0) & (cs < 5)) {   # while in a transient state
    cs <- y + cs                  # update current state (cs)
    y <- rbinom(1, 1, .54) 
    if (y==1) y =-1 else y = 1
    x <- c(x, cs)                 # 'x' is record of all states visited
  }
  t[i] <- length(x) - 1           # 't' is steps before absorption = rolls of die
  a[i] <- cs                      # 'a' is state where absorbed
}

mean(t)                           # Mean rounds until absorption
mean(t >= 8)                      # Probability of the game lasting at least 8 rounds
mean(a==0)                        # P1's chance a failure
mean(a==5)                        # P2's chance of failure
