########################
# Testing Assignment 1 #
########################
rm(list=ls())

# RUN THIS CODE IN THE SAME WORKING DIRECTORY WHERE YOUR 
# Assignment_1.R FILE IS LOCATED
file_name <- "Assignment_1.R"

####################################
Test_name <- c("Strings not allowed:",
               "a1q1. Length of output:",
               "a1q1. Type of output:",
               "a1q1. Updating formula:",
               "a1q1. Initiation:",
               "a1q1. Running time:",
               "a1q2. Output a data frame:",
               "a1q2. Output's dimensions:",
               "a1q2. Names of variables:",
               "a1q2. KS test, p_value:",
               "a1q2. Max U:",
               "a1q2. Running time:",
               "a1q3. Output a data frame:",
               "a1q3. Output's dimensions:",
               "a1q3. Names of variables:",
               "a1q3. KS test, p_value:",
               "a1q3. Value of ratio:",
               "a1q3. Running time:")
Status <- rep(NA, length(Test_name))

# Check that the code does not contain "rgeom" nor "rexp"
require(stringr)
require(microbenchmark)

strings_not_allowed <- c("library", "require", 
                         "rgamma", "rnorm")
script <- paste(scan(file_name, what = "a"), collapse = "")
detected <- strings_not_allowed[str_detect(script,strings_not_allowed)]
Status[1] <- ifelse(length(detected) == 0, "Pass",
   paste(detected, collapse = ", ")) 

source(file_name)

######
# Q1 #
######
# Checks that the code for "a1q1" produces a numeric vector 
# of length n
n <- sample(200:300,1)
j <- 36
k <- 99
m <- 2^(30)
seed <- 2022
X <- a1q1(n,j,k,m,seed)
Status[2] <- ifelse(length(X) == n, "Pass",
   "Does not produce a vector of length n")
Status[3] <- ifelse(is.numeric(X), "Pass",
   "Does not produce a numeric vector")

# Checks the updating formula
xk <- X[1:10]
xj <- X[1:10 + k-j]
xn <- X[1:10 + k]
Status[4] <- ifelse (all(xn == ((xk+xj) %% m)), "Pass",
   "The updating formula in ex1q1 is incorrect")

# Checks initiation
xn <- X[1:5]
ref <- c(763984640, 73105472, 1045667840, 925348928, 380944128)
Status[5] <- ifelse (all(xn == ref), "Pass",
    "The initialization in ex1q1 is incorrect")

# Check running time
n <- 10^5
asses <- microbenchmark(a1q1(n,j,k,m,seed))
Status[6] <- summary(asses)[,"median"]

######
# Q2 #
######
# Checks that the code for "a1q2" produces a data frame 
# of the correct dimensions
n <- sample(500:1000,1)
shape <- runif(1, 2.5, 6.5)
out <- a1q2(n,shape)
Status[7] <- ifelse(is.data.frame(out), "Pass", 
                "a1q2 does not produce a data frame")
Status[8] <- ifelse(all(dim(out) == c(n,2)), "Pass", 
                "a1q2 does not produce the right dimensions")
Status[9] <- ifelse(all(sort(names(out)) == c("U","X")), "Pass",
                 "a1q2 does give the required names to the variables")

# Checks the marginal distributions
KS <- ks.test(out$X, "pgamma", shape)
Status[10] <- ifelse(KS$p.value > 0.0001, "Pass",
                 "The marginal distribution in a1q2 is wrong")

# Check the rejection frequency
Status[11] <- ifelse(max(out$U) > 0.95, "Pass",
                 "The rejection frequency a1q2 is not optimal")

# Check running time
n <- 10^4
asses <- microbenchmark(a1q2(n,shape))
Status[12] <- summary(asses)[,"median"]


######
# Q3 #
######
# Checks that the code for "a1q3" produces a data frame 
# of the correct dimensions
n <- sample(500:1000,1)
out <- a1q3(n)
Status[13] <- ifelse(is.data.frame(out), "Pass", 
                    "a1q3 does not produce a data frame")
Status[14] <- ifelse(all(dim(out) == c(n,3)), "Pass", 
                    "a1q3 does not produce the right dimensions")
Status[15] <- ifelse(all(sort(names(out)) == c("U1","U2","X")), "Pass",
                    "a1q3 does give the required names to the variables")

# Checks the marginal distributions
KS <- ks.test(out$X, "pnorm")
Status[16] <- ifelse(KS$p.value > 0.0001, "Pass",
                    "The marginal distribution in a1q3 is wrong")

# Check the ratio
Status[17] <- ifelse(with(out, all(abs(X - U2/U1) < 10^(-6))), "Pass",
                     "The variable X in a1q3 is not equal to U2/U1")

# Check running time
n <- 10^4
asses <- microbenchmark(a1q3(n))
Status[18] <- summary(asses)[,"median"]

print(data.frame(Test_name, Status), right = FALSE)
