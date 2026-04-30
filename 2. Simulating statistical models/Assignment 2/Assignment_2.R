# Assignment 2

# Q1: Replace "# YOUR CODE HERE" by your code
a2q1 <- function(n, mu, Sigma){
  Z <- matrix(rnorm(n * length(mu)), nrow = n)
  X <- Z %*% chol(Sigma)
  sweep(X, 2, mu, "+")
}


# Q2: Replace "# YOUR CODE HERE" by your code
a2q2 <- function(n, m, a, b, sd, sd.0){
  eps <- matrix(rnorm(n * (m + 1), sd = sd), nrow = n)
  Y <- matrix(0, nrow = n, ncol = m)
  prev <- rnorm(n, sd = sd.0)
  for (i in seq_len(m)) {
    Y[, i] <- a * prev + b * eps[, i] + eps[, i + 1]
    prev <- Y[, i]
  }
  Y
}

# Q3: Replace "# YOUR CODE HERE" by your code
a2q3 <- function(n, rN, rX){
  N <- rN(n)
  total <- sum(N)
  S <- numeric(n)
  if (total > 0) {
    X <- rX(total)
    cs <- c(0, cumsum(X))
    ends <- cumsum(N)
    S <- cs[ends + 1] - cs[ends - N + 1]
  }
  S
}
