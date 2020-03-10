install.packages("mvtnorm")
library(mvtnorm)

# define parameters for K = 3 multiv. N dist
K <-3
mus.actual <- matrix(0,3,2)
mus.actual[1,] <- c(0,0)
mus.actual[2,] <- c(4,4)
mus.actual[3,] <- c(-4,4)

# generate covariance matrixes randomly

Sigmas.actual <- list()
for (k in 1:K) {
  mtx <- matrix(1,2,2)
  mtx[1,2] <- runif(1) * 2 - 1
  mtx[2,1] <- mtx[1,2]
  Sigmas.actual[[k]] <- mtx * exp(rnorm(1))
} 

# genrate random mixture weights

ws <- runif(K)
ws <- ws/sum(ws)

# genrate 1000 data points in R^2

n <- 1000
p <- 2
xs <- matrix(0,n,p)
cols <- rep(0,n)
for (i in 1:n) {
  k <- sample(K,1, prob = ws)
  
  xs[i,] <- rmvnorm(n = 1, mean = mus.actual[k,], sigma = Sigmas.actual[[k]])
  
  cols[i] <- k
}

plot(xs, col = cols, pch = 20)



# test code


n <- 500; p <- 50
K.actual <- 2
mix <- runif(K.actual); mix <- mix / sum(mix)
mus.actual <- matrix(runif(K.actual*p),K.actual,p)
zs.actual <- rep(0,n)
xs <- matrix(0,n,p)
for (i in 1:n) {
  cl <- sample(K.actual,size=1,prob=mix)
  zs.actual[i] <- cl
  xs[i,] <- (runif(p) < mus.actual[cl,])
}


# run EM algorithm on data

source("em_mixture_gaussians.R")
print(system.time(out <- em_mix_bernoulli(xs, K.actual)))


# check if calculated parameters are close to truth

v1 <- sum(abs(out$mus - mus.actual))

v2 <- sum(abs(out$mus[1:2,] - mus.actual))

vm <- min(v1, v2)/p/2

print(vm)

if (vm> 0.3) print("Probably  shit happened")else print("aaaa kurwa dziala")

