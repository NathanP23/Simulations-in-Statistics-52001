##########
# Part3f #
##########
N <- 1500

MC <- function(rho, N = 1500){
  X <- runif(N)
  I <- mean(1/(1+X)) + rho*mean(X-0.5)
  return(I)
} 

MC(0)
MC(0.5)
MC(0.4773)

sd(replicate(10^5,MC(0)))
sd(replicate(10^5,MC(0.5)))
sd(replicate(10^5,MC(0.4773)))
