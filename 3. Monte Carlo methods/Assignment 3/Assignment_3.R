# Assignment 3

# Q1: Replace "# YOUR CODE HERE" by your code
a3q1 <- function(x, n, p0, p1, N){
  Y <- rbinom(N, n, p1)
  W <- exp(dbinom(Y, n, p0, log = TRUE) - dbinom(Y, n, p1, log = TRUE))
  sapply(x, function(z) mean((Y > z) * W))
}


# Q2: Replace "# YOUR CODE HERE" by your code
a3q2 <- function(f, gen.X, g, m, N){
  X <- gen.X(N)
  F <- f(X)
  G <- g(X)
  c0 <- cov(F, G) / var(G)
  mean(F - c0 * (G - m))
}

# Q3: Replace "# YOUR CODE HERE" by your code
a3q3 <- function(X, m, B){
  n <- length(X)
  sums <- c(0, cumsum(X))
  statistic <- max(sums[(m + 1):(n + 1)] - sums[1:(n - m + 1)])
  mu <- mean(X)
  sig <- sd(X)
  Y <- matrix(rnorm(B * n, mean = mu, sd = sig), nrow = B)
  current <- rowSums(Y[, 1:m, drop = FALSE])
  max.stat <- current
  if (m < n) {
    for (i in 2:(n - m + 1)) {
      current <- current - Y[, i - 1] + Y[, i + m - 1]
      max.stat <- pmax(max.stat, current)
    }
  }
  list(statistic = statistic, p.value = mean(max.stat > statistic))
}
