# There is one server and a capacity of 2 within the system.
# States are accordingly; 
# 0 - the server is free
# 1 - the server is busy and nobody is waiting
# 2 - one customer is waiting and one is being worked on.

set.seed(1)
N = 2
lam = 4         # arrival rate
mu = 5          # service rate
rho = lam/mu
m = 50000
x = t = numeric(m)  # # 't' is the probability that a customer either arrives or departs
x[1] = 0        # 'x' represents the state changes

for (i in 2:m) {
	if (x[i-1] == 0) {
		x[i] <- 1
		t[i - 1] <- rexp(1, lam)
	} 
  else {
	  if (x[i-1] == N) {
		  x[i] <- N-1
		  t[i-1] <- rexp(1, mu)
		} 
    else {
		  x[i] <- sample(x[i-1]+c(-1,1), 1, prob=c(mu, lam))
			t[i-1] <- rexp(1, lam+mu)
		}
  }
}

# t.avg measures the mean time customers waited per state
t.avg <- numeric(N+1)

# p is the exact amount of time they spent in the system
states <- 0
Np <- rho^states *(1-rho)/(1-rho^(N+1))

for (j in 1:(N+1)) {
  t.avg[j] <- sum(t[x==(j-1)]) / sum(t)
}

# below is the time average distribution according to the states
round(cbind(states, p, t.avg), 3)

# the effective rate of entry
lam.e = (1-t.avg[3])*lam; lam.e

# average number waiting in the queue
L = sum((0:2)*t.avg); L

# average number of customers waiting in the system
L.q = t.avg[3]; L.q

# average time in the system
W = L/lam.e; W

# average customers' wait time in the queue
W.q = W - 1/mu; W.q

########################################################

# discrete-time markov chain
P <- matrix((1/60)*c(56, 4, 0,
                     5, 51, 4,
                     0, 5, 55),
            byrow=T, nrow=3)

set.seed(1)
m <- 10^6
y <- numeric(m)
y[1] <- 2

for (i in 2:m) {
  y[i] <- sample(1:3, 1, prob=P[y[i-1],])
}

# simulation is very similar to the discretized version
round(summary(as.factor(y))/m, 4) # DTMC
p # simulation



