##########
# Part3g #
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


# Distribution of gamma.hat (Normal)
gam.sim.normal <- replicate(10^5,gamma.hat(rnorm(n)))
summary(gam.sim.normal)
sd(gam.sim.normal)*sqrt(n)



# Distribution of gamma.hat (Laplace)
gam.sim.laplace <- replicate(10^5,
            gamma.hat(rexp(n)*(2*rbinom(n,1,0.5)-1)))
summary(gam.sim.laplace)
sd(gam.sim.laplace)*sqrt(n)

# Confidence interval
gam + c(-1,1)*1.96*sd(gam.sim.laplace)

# Significant level
mean(abs(gam.sim.laplace)/sd(gam.sim.laplace) > 1.96)
