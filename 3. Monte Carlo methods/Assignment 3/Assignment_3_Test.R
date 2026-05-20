########################
# Testing Assignment 3 #
########################
rm(list=ls())

# RUN THIS CODE IN THE SAME WORKING DIRECTORY WHERE YOUR 
# Assignment_3.R FILE IS LOCATED
file_name <- "Assignment_3.R" 

####################################
Test_name <- c("Strings not allowed:",
               "a3q1. Type of output:",
               "a3q1. Length of output:",
               "a3q1. Accuracy:",
               "a3q1. Distribution:",
               "a3q1. Running time:",
               "a3q2. Type of output:",
               "a3q2. Length of output:",
               "a3q2. Accuracy:",
               "a3q2. Distribution:",
               "a3q2. Running times:",
               "a3q3. Output is a list:",
               "a3q3. Names in the list:",
               "a3q3. Length of objects:",
               "a3q3. Numeric objects:",
               "a3q3. Compute statistic:",
               "a3q3. Compute p-value:",
               "a3q3. Distribution:",
               "a3q3. Running times:")
Status <- rep(NA, length(Test_name))

####################################

# Uploading the required libraries. 
# Make sure to have them installed.
require(stringr)
require(microbenchmark)
require(MASS)

# Check that the code does not contain "boot"
strings_not_allowed <- c("library", "require",
                         "boot")
script <- paste(scan(file_name, what = "a"), collapse = "")
detected <- strings_not_allowed[str_detect(script,strings_not_allowed)]
Status[1] <- ifelse(length(detected) == 0, "Pass",
                    paste(detected, collapse = ", ")) 

source(file_name)

######
# Q1 #
######
# Checks that the code for "a3q1" produces a numeric vector 
# of length equal to the length of x 

n <- sample(30:40,1)
x <- sample(20:30,sample(3:7,1))
p0 <- runif(1,0.1,0.3)
p1 <- runif(1,0.5,0.6)
N <- 1e3
S <- a3q1(x, n, p0, p1, N)

Status[2] <- ifelse(is.numeric(S), "Pass",
                    "Type is not numeric")
Status[3] <- ifelse(length(x) == length(S), "Pass",
                    "Not the same length as x")


# Checks the accuracy of the approximation
n <- sample(30:40,1)
x <- sample((n-10):(n-5),1)
p0 <- runif(1,0.3,0.4)
p1 <- x/n
S <- a3q1(x, n, p0, p1, 1e6)
S.R <- 1-pbinom(x,n,p0)

Status[4] <- ifelse(abs(S/S.R - 1) <= 0.1, "Pass",
                "The approximation by a3q1 is not accurate enough")


# Checks the distribution of the approximation
N <- 1e2
S <- replicate(1e5,a3q1(x, n, p0, p1, N))
S.R <- 1-pbinom(x,n,p0)
y <- (x+1):n
V.R <- sum(dbinom(y,n,p0)^2/dbinom(y,n,p1))-S.R^2
V <- var(S)*N

Status[5] <- ifelse(abs(V/V.R - 1) <= 0.1, "Pass",
                "The distribution of the approximation is wrong")

# Check running time
N1 <- 1e3
N2 <- 1e4
x <- 20:29
asses <- microbenchmark(a3q1(x[1], n, p0, p1, N1),
                        a3q1(x, n, p0, p1, N1),
                        a3q1(x[1], n, p0, p1, N2))

Status[6] <- paste(summary(asses)[,"median"], collapse = ", ")

######
# Q2 #
######
# Checks that the code for "a3q2" produces a numeric vector 
# of length 1 

f <- function(u) exp(-u)/(1 + u^2)
g <- function(u) exp(-0.5)/(1 + u^2)
m <- exp(-0.5)*pi/4
N <- 1e4
I <- a3q2(f, runif, g, m, N)

Status[7] <- ifelse(is.numeric(I), "Pass",
                    "Type is not numeric")
Status[8] <- ifelse(length(I) == 1, "Pass",
                    "Not of length 1")

# Checks the accuracy of the approximation
a <- runif(1,0.5,2)
b <- runif(1,1.5,2.5)
f <- function(u) exp(-a*u)/(1 + u^b)
g <- function(u) exp(-a*0.5)/(1 + u^b)
m <- integrate(g,0,1)$value
N <- 1e4
I <- a3q2(f, runif, g, m, N)
I.R <- integrate(f,0,1)$value

Status[9] <- ifelse(abs(I/I.R - 1) <= 0.01, "Pass",
                "The approximation by a3q2 is not accurate enough")


# Checks the distribution of the approximation
r <- runif(1,0.3,0.5)
f <- function(u) u
g <- function(u) r*u + sqrt(1-r^2)*rnorm(length(u))
m <- 0
N <- 1e4
S <- replicate(1e4,a3q2(f, rnorm, g, m, N))
V <- var(S)*N
V.R <- 1-r^2

Status[10] <- ifelse(abs(V/V.R - 1) <= 0.1, "Pass",
                  "The distribution of the approximation is wrong")

# Check running time
N1 <- 1e3
N2 <- 1e4
asses <- microbenchmark(a3q2(f, runif, g, m, N1),
                        a3q2(f, runif, g, m, N2))

Status[11] <- paste(summary(asses)[,"median"], collapse = ", ")

######
# Q3 #
######
# Checks that the code for "a3q3" produces a list
# with 2 numeric values of the correct type.
X <- rnorm(1e2)
m <- 5
B <- 1e4
out <- a3q3(X, m, B)
out.names <- c("p.value", "statistic")

Status[12] <- ifelse(is.list(out), "Pass",
                    "Output is not a list")
Status[13] <- ifelse(all(sort(names(out)) == out.names), "Pass",
                    "Not the correct names")


out <- simplify2array(out) 

Status[14] <- ifelse(!is.list(out) & (length(out) == 2), "Pass",
                     "Objects not of length 1")
Status[15] <- ifelse(is.numeric(out), "Pass",
                     "Objects not numeric")

# Checks the accuracy of the approximation
X <- rnorm(1e2)
m <- 3
B <- 1e2
out <- a3q3(X, m, B)
index <- 1:98
d <- out$statistic - max(X[index]+X[index+1]+X[index+2])

Status[16] <- ifelse(abs(d) <= 1e-10, "Pass",
                  "Error in the computation of the statistic")

m <- 1
B <- 1e5
out <- a3q3(X, m, B)
M <- max(X)
mu <- mean(X)
sig <- sd(X)
p <- pnorm(out$statistic, mean = mu, sd = sig)^100

Status[17] <- ifelse(abs(out$p.value/(1-p) - 1) <= 0.05, "Pass",
                     "Error in the computation of the p-value")


# Checks the distribution of the approximation
X <- rnorm(1e2)
m <- 5
out <- a3q3(X, m, 1e5)
P <- replicate(1e4,a3q3(X, m, 10)$p.value)
V <- var(P)*10
V.R <- out$p.value*(1-out$p.value)

Status[18] <- ifelse(abs(V/V.R - 1) <= 0.1, "Pass",
                     "Error in the distribution of the p-value")

# Check running time
X1 <- rnorm(1e2)
m0 <- 1
m1 <- 5
X2 <- rnorm(1e3)
m2 <- 50
B <- 1e3
asses <- microbenchmark(a3q3(X1,m0,B),
                        a3q3(X1,m1,B),
                        a3q3(X2,m2,B))

Status[19] <- paste(summary(asses)[,"median"], collapse = ", ")

print(data.frame(Test_name, Status), right = FALSE)

