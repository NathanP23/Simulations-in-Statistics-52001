##########
# Part4f #
##########

# A function to simulate random-walk MH
sim.MH <- function(N, sigma, ratio, f, X0, burn.in = 100){
  X <- X0
  n.sim <- length(X0)
  
  # Burn in period
  for(i in seq(burn.in)){
    Y <- X + rnorm(n.sim, sd = sigma)
    U <- runif(n.sim)
    alpha <- pmin(ratio(X,Y),1)
    X[U <= alpha] <- Y[U <= alpha]
  }

  S <- A <- rep(0, n.sim)
  # Compute average
  for(i in seq(N - burn.in)){
    Y <- X + rnorm(n.sim, sd = sigma)
    U <- runif(n.sim)
    alpha <- pmin(ratio(X,Y),1)
    X[U <= alpha] <- Y[U <= alpha]
    S <- S + f(X)
    A <- A + (U <= alpha)
  }
  return(list(Z = S/(N-burn.in), accept.rate = A/(N-burn.in)))
}

ratio <- function(X,Y){
  (abs(Y) <= 3*pi)*((X*sin(Y))/(Y*sin(X)))^2
}

f <- function(x) x^2  
X0 <- rep(1,10^3)

# sd = 1
out <- sim.MH(10^5, 1, ratio, f, X0, burn.in = 100)
mean(out$Z)
mean(out$accept.rate)
sd(out$Z)

# sd = 6
out <- sim.MH(10^5, 6, ratio, f, X0, burn.in = 100)
mean(out$Z)
mean(out$accept.rate)
sd(out$Z)


