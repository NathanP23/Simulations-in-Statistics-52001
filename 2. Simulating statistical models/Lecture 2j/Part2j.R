##########
# Part2j #
##########

# Simulate marginals of AR(1)
n <- 10^5
X <- rep(0,n)

X <- 0.8*X + rnorm(n)
plot(ecdf(X))

X <- 0.8*X + rnorm(n)
lines(ecdf(X), col = 2)

X <- 0.8*X + rnorm(n)
lines(ecdf(X), col = 3)

# Convergence to the stationary distribution

for (i in 1:10^3){
  X <- 0.8*X + rnorm(n)
}
lines(ecdf(X), col = 4)

var(X)
1/(1-0.8^2)
