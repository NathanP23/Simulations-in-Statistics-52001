##########
# Part2i #
##########
P <- matrix(c(1/2,0,1/5,1/2,1/2,0,0,1/2,4/5), 3, 3)
P

# Compute stationary distribution
evd <- eigen(t(P))
evd$values
temp <- evd$vectors[,1]
temp
temp <- Re(temp)
stationary <- temp/sum(temp)
stationary

# Convergence to the stationary distribution
d <- c(1,0,0)
for(i in 1:10^2){
  d <- d %*% P
}
d

d <- c(0,0,1)
for(i in 1:10^2){
  d <- d %*% P
}
d
