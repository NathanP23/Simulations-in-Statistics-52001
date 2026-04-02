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

# So basically what I did here is split the function into two parts.
# First I build the initial seed sequence of length k using the LCG
# formula they gave us (the one with the big multipler 1103515245).
# X_0 is the seed, and then each X_{-(i+1)} is computed from X_{-i}.
# I store them in a vector where position k corresponds to X_0,
# position k-1 to X_{-1}, and so on backwards.
# Then in the second part I just concatenate that init vector with
# an empty vector of length n, and fill it in using Knuths recursion:
# X_n = (X_{n-j} + X_{n-k}) mod m. At the end I only retrun the
# last n elements which are the ones from step 2.

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

# For Q2 I used envelope rejection sampling like they described.
# The idea is that for a non-integer shape r, we cant directly sample
# Gamma(r,1), so we use a proposal distribuiton g which is
# Gamma(m, lambda) where m = floor(r) and lambda = m/r.
# To generate from Gamma(m, lambda) I just sum up m independent
# Exp(lambda) random variables (thats a known property).
# Then the acceptance ratio simplifies to (x/r)^delta * exp(delta*(1-x/r))
# where delta = r - m, which is always between 0 and 1.
# I generate in batches (1.5x what I still need) using a while loop
# untill I have enough accepted samples, then trim to exactly n.
# The U values that are returned are the uniforms used for the
# accept/reject decison for the accepted proposals.

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

# Here I implemented the ratio-of-uniforms method for the normal.
# The normal kernel is h(x) = exp(-x^2/2), and from the formulas
# in the lecture we get that m = 1 (supremum of h), and the bounds
# for U2 are a = -sqrt(2/e) and b = sqrt(2/e).
# So I sample U1 ~ Uniform(0,1) and U2 ~ Uniform(-sqrt(2/e), sqrt(2/e)),
# and accept if U1^2 <= exp(-0.5 * (U2/U1)^2), which is equivelant
# to checking that the point (U1, U2) falls inside the set A.
# The accepted X = U2/U1 then follows a standard normal distribtuion.
# Same as Q2 I use a while loop with batches to collect exactly n
# accepted samples. The output includes X and both U1, U2 for the
# accepted ones so the test can verify that X = U2/U1.
