##########
# Part3h #
##########

# Data
X <- c(-1.99, -1.33, -2.56,  0.85, -0.65,
       0.21, -0.22,  1.44,  0.11,  3.21)
n <- length(X)

# Estimate skewness
gamma.hat <- function(X){
  m1 <- mean(X)
  m2 <- mean((X-m1)^2)
  m3 <- mean((X-m1)^3)
  return(m3/m2^(3/2))
}

gam <- gamma.hat(X)
gam

# MSE of gamma.hat (Nonparametric)
gam.sim.np <- replicate(10^5,
                 gamma.hat(sample(X, n, rep = TRUE)))
mean(gam.sim.np) - gam
sd(gam.sim.np)
var(gam.sim.np) + (mean(gam.sim.np) - gam)^2


# MSE of gamma.hat (Parametric)
mu <- mean(X)
sig <- sd(X)
gam.sim.p <- replicate(10^5,
                gamma.hat(rnorm(n, mu, sig)))
mean(gam.sim.p) - 0
sd(gam.sim.p)
var(gam.sim.p) + (mean(gam.sim.p) - 0)^2
