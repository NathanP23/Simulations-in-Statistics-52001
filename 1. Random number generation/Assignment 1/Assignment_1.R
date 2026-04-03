# Assignment 1

# Q1: Replace "# YOUR CODE HERE" by your code
a1q1 <- function(n, j, k, m, seed){
  init <- numeric(k)
  init[k] <- seed
  if (k > 1) {
    for (idx in seq_len(k - 1)) {
      init[k - idx] <- (1103515245 * init[k - idx + 1] + 12345) %% (2^32)
    }
  }
  full_seq <- c(init, numeric(n))
  for (idx in (k + 1):(k + n)) {
    full_seq[idx] <- (full_seq[idx - j] + full_seq[idx - k]) %% m
  }
  full_seq[(k + 1):(k + n)]
}

# Q2: Replace "# YOUR CODE HERE" by your code
a1q2 <- function(n, shape){
  r <- shape
  m_int <- floor(r)
  lam <- m_int / r
  delta <- r - m_int
  accepted_x <- numeric(0)
  accepted_u <- numeric(0)
  while (length(accepted_x) < n) {
    batch_size <- ceiling(1.5 * (n - length(accepted_x)))
    proposals <- rowSums(matrix(rexp(batch_size * m_int, rate = lam), nrow = batch_size, ncol = m_int))
    u_vals <- runif(batch_size)
    ratio <- (proposals / r)^delta * exp(delta * (1 - proposals / r))
    is_acc <- u_vals <= ratio
    accepted_x <- c(accepted_x, proposals[is_acc])
    accepted_u <- c(accepted_u, u_vals[is_acc])
  }
  data.frame(X = accepted_x[1:n], U = accepted_u[1:n])
}

# Q3: Replace "# YOUR CODE HERE" by your code
a1q3 <- function(n){
  bound <- sqrt(2 / exp(1))
  accepted_x <- numeric(0)
  accepted_u1 <- numeric(0)
  accepted_u2 <- numeric(0)
  while (length(accepted_x) < n) {
    batch_size <- ceiling(1.5 * (n - length(accepted_x)))
    u1 <- runif(batch_size, 0, 1)
    u2 <- runif(batch_size, -bound, bound)
    is_acc <- u1^2 <= exp(-0.5 * u2^2 / u1^2)
    accepted_x <- c(accepted_x, (u2 / u1)[is_acc])
    accepted_u1 <- c(accepted_u1, u1[is_acc])
    accepted_u2 <- c(accepted_u2, u2[is_acc])
  }
  data.frame(X = accepted_x[1:n], U1 = accepted_u1[1:n], U2 = accepted_u2[1:n])
}
