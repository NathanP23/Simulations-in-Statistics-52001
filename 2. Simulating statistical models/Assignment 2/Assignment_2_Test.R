########################
# Testing Assignment 2 #
########################
rm(list=ls())

# RUN THIS CODE IN THE SAME WORKING DIRECTORY WHERE YOUR 
# Assignment_2.R FILE IS LOCATED
file_name <- "Assignment_2.R" 

####################################
Test_name <- c("Strings not allowed:",
               "a2q1. Output is a matrix:",
               "a2q1. Type of output:",
               "a2q1. Dimension of matrix:",
               "a2q1. KS test of rows:",
               "a2q1. KS test of columns:",
               "a2q1. Running time:",
               "a2q2. Output a data frame:",
               "a2q2. Output's type:",
               "a2q2. Output dimensions:",
               "a2q2. KS test, p_value:",
               "a2q2. Autocorrelations:",
               "a2q2. Running times:",
               "a2q3. Output is a vector:",
               "a2q3. Output is numeric:",
               "a2q3. Length of output:",
               "a2q3. KS test, p_value:",
               "a2q3. Running times:")
Status <- rep(NA, length(Test_name))



# Uploading the required libraries. 
# Make sure to have them installed.
require(stringr)
require(microbenchmark)
require(MASS)

# Check that the code does not contain "mvrnorm", "rmvnorm", "rmvn",
# "arima.sim", nor "Compound"
strings_not_allowed <- c("library", "require",
                         "mvrnorm", "rmvnorm", "rmvn",
                         "arima.sim",
                         "Compound")
script <- paste(scan(file_name, what = "a"), collapse = "")
detected <- strings_not_allowed[str_detect(script,strings_not_allowed)]
Status[1] <- ifelse(length(detected) == 0, "Pass",
                    paste(detected, collapse = ", ")) 

source(file_name)

######
# Q1 #
######
# Checks that the code for "a2q1" produces a numeric matrix 
# of dimension n times length(mu) 
n <- sample(200:300,1)
m <- sample(20:40,1)
mu <- rnorm(m)
Sigma <- matrix(rnorm(m^2),m,m)
Sigma <- Sigma %*% t(Sigma)
X <- a2q1(n,mu,Sigma)

Status[2] <- ifelse(is.matrix(X), "Pass",
                    "Does not produce a matrix")
Status[3] <- ifelse(is.numeric(X), "Pass",
                    "Type is not numeric")
Status[4] <- ifelse(all(dim(X) == c(n,m)), "Pass",
                   "Wrong dimensions")

# Checks the distribution of the rows
X <- a2q1(n,mu,Sigma)
a <- runif(m)
a_X <- as.vector(X %*% a)
a_mean <- sum(a*mu)
a_sd <- as.vector(sqrt(a %*% Sigma %*% a)) 
KS <- ks.test(a_X, "pnorm", mean = a_mean, sd = a_sd)
Status[5] <- ifelse(KS$p.value > 0.0001, "Pass",
                    "The distribution produced by a2q1 is not multinormal")

# Checks the distribution of the columns
p_val <- rep(0,m)
for (j in seq(m)){
  KS <- ks.test(X[,j], "pnorm", mean = mu[j], sd = sqrt(Sigma[j,j]))
  p_val[j] <- KS$p.value
}
Status[6] <- ifelse(min(p_val) > 0.0001, "Pass",
                    "The distribution produced by a2q1 is not multinormal")

# Check running time
n <- 10^4
m <- 10
mu <- rnorm(m)
Sigma <- matrix(rnorm(m^2),m,m)
Sigma <- Sigma %*% t(Sigma)
asses <- microbenchmark(a2q1(n,mu,Sigma),
                        mvrnorm(n, mu, Sigma))
Status[7] <- paste(summary(asses)[,"median"], collapse = ", ")

######
# Q2 #
######
# Checks that the code for "a2q2" produces a numeric matrix 
# of dimension n times m 
n <- sample(200:300,1)
m <- sample(20:40,1)
a <- runif(1, min = -0.8, max = 0.8)
b <- runif(1, min = -0.8, max = 0.8)
sd <- runif(1, min = 0, max = 5)
sd.0 <- runif(1, min = 0, max = 5)
X <- a2q2(n, m, a, b, sd, sd.0)

Status[8] <- ifelse(is.matrix(X), "Pass",
                    "Does not produce a matrix")
Status[9] <- ifelse(is.numeric(X), "Pass",
                    "Type is not numeric")
Status[10] <- ifelse(all(dim(X) == c(n,m)), "Pass",
                   "Wrong dimensions")


# Checks the marginal distributions
n <- 10^4
m <- 10
a <- runif(1, min = -0.8, max = 0.8)
b <- runif(1, min = -0.8, max = 0.8)
s <- runif(1, min = 1, max = 5)
s.0 <- s*sqrt(1+(a+b)^2/(1-a^2))
X <- a2q2(n, m, a, b, s, s.0)
j <- sample(5:10,1)
KS <- ks.test(X[,j]/s.0, "pnorm")
Status[11] <- ifelse(KS$p.value > 0.0001, "Pass",
                 "The marginal distribution is wrong")

# Check autocorrelation
n <- 10^6
m <- 10
a <- runif(1, min = -0.8, max = 0.8)
b <- runif(1, min = -0.8, max = 0.8)
s <- runif(1, min = 1, max = 5)
s.0 <- s*sqrt(1+(a+b)^2/(1-a^2))
X <- a2q2(n, m, a, b, s, s.0)
j <- sample(5:10,1)
W1 <- X[,j] - a*X[,j-1]
W2 <- X[,j-1] - a*X[,j-2]
V <- c(var(W1),var(W2),cov(W1,W2))
v <- s^2*c(1+b^2,1+b^2,b)
Status[12] <- ifelse(max(abs(V-v)) <= 0.1, "Pass",
                     "The correlation structure is wrong")

# Check running time
n0 <- 10^2
n1 <- 10^4
m0 <- 10^1
m1 <- 10^3
a <- runif(1, min = -0.8, max = 0.8)
b <- runif(1, min = -0.8, max = 0.8)
sd <- runif(1, min = 0, max = 5)
sd.0 <- runif(1, min = 0, max = 5)
asses <- microbenchmark(a2q2(n0, m0, a, b, sd, sd.0),
                        a2q2(n1, m0, a, b, sd, sd.0),
                        a2q2(n0, m1, a, b, sd, sd.0))
Status[13] <- paste(summary(asses)[,"median"], collapse = ", ")

######
# Q3 #
######
# Checks that the code for "a2q3" produces a numeric 
# vector of the correct length
n <- sample(500:1000,1)
rN <- function(n) rpois(n, lambda = 20)
rX <- function(n) rexp(n)
S <- a2q3(n, rN, rX)

Status[14] <- ifelse(is.vector(S), "Pass",
                    "Not a vector")
Status[15] <- ifelse(is.numeric(S), "Pass",
                    "Type is not numeric")
Status[16] <- ifelse(length(S) == n, "Pass",
                   "Wrong length")

# Checks the marginal distributions
n <- 10^4
size <- sample(2:6,1)
prob <- runif(1, min = 0.3, max = 0.7)
rN <- function(n) rbinom(n, size = size, prob = prob)
rX <- function(n) rexp(n)
S <- a2q3(n, rN, rX)
CDF <- function(x) {
  p <- dbinom(seq(size),size,prob)
  D <- 0
  for(i in seq(size)) D <- D + p[i]*pgamma(x,i)
  return(D/sum(p))
}
KS <- ks.test(S[S > 0], CDF)
Status[17] <- ifelse(KS$p.value > 0.0001, "Pass",
                     "The marginal distribution is wrong")

# Check running times
n0 <- 10^2
n1 <- 10^4
asses <- microbenchmark(a2q3(n0, rN, rX),
                        a2q3(n1, rN, rX))
Status[18] <- paste(summary(asses)[,"median"], collapse = ", ")

print(data.frame(Test_name, Status), right = FALSE)
