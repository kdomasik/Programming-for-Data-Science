## logsumexp(x) returns log(sum(exp(x))) but performs the computation in a more stable manner
logsumexp <- function(x) return(log(sum(exp(x - max(x)))) + max(x))

#computing log likelihood of bernoullis directly
compute_ll <- function(xs, mus, lws, gammas) {
  ll_ber <- 0
  n <- dim(xs)[1]
  K <- dim(mus)[1]
  A <- matrix(nrow = n, ncol = K)
  for (i in 1:n) {
    for (k in 1:K) {
      A[i,k] <- gammas[i,k]
      if (gammas[i,k] > 0) {
        ll_ber <- ll_ber + gammas[i,k] * (lws[k] + sum(log((mus[k,] ^ (xs[i,])) *
                                                             ((1 - mus[k,]) ^ (1 - xs[i,])) -
                                                             log(gammas[i,k]))))
      }
    }
  }
  
  return(ll_ber)
}


# EM algorithm for the mixture of Bernoullis model
em_mix_bernoulli <- function(xs,K) {
  p <- dim(xs)[2]
  n <- dim(xs)[1]
  
  # lws is log(ws)
  # starting with all lws equal
  lws <- rep(log(1/K),K)
  
  ## selecting initial cluster means randomly
  # Num is the the number of observations
  Num <- p * K
  mus <- matrix(rexp(Num, rate = 5), ncol=p, nrow=K)
  
  
  # creating matrix for gammas 
  gammas <- matrix(0,n,K)
  
  converged <- FALSE
  Iterations <- 0
  ll_ber <- -Inf
  print("iteration : log-likelihood")
  while(!converged) {
    Iterations <- Iterations + 1
    mus.old <- mus
    ll_ber.old <- ll_ber
    
    ## E step - calculating gammas
    for (i in 1:n) {
      
      lprs <- rep(0,K)
      for (k in 1:K) {
        lprs[k] <- lws[k] + sum(log((mus[k,] ^ (xs[i,])) *
                                      ((1 - mus[k,]) ^ (1 - xs[i,]))))
        #dbinom(xs[i,], 1, mus[k,], log = TRUE)
      }
      gammas[i,] <- exp(lprs - logsumexp(lprs))
    }
    
    ll_ber <- compute_ll(xs,mus,lws, gammas)
    print(paste(Iterations,": ",ll_ber))
    
    # M step - updating lws and mus
    Ns <- rep(0,K)
    for (k in 1:K) {
      Ns[k] <- sum(gammas[,k])
      lws[k] <- log(Ns[k])-log(n)
      
      mus[k,] <- rep(0,p)
      
      for (i in 1:n) {
        
        mus[k,] <- mus[k,]+gammas[i,k]/Ns[k]*xs[i,]
        
        # updating mu as suggested in Lab 4
        mus[which(mus > 1,arr.ind=TRUE)] <- 1 - 1e-15
        
      }
    }
    # stoping when the increase in the log-likelihood is close to 0 enough for our purposes
    if (abs(ll_ber-ll_ber.old) < 1e-5) converged <- TRUE
  }
  return(list(lws=lws,mus=mus,gammas=gammas,ll_ber=ll_ber))
}


# testing using th eexample from lab 4

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
# Assignment data

load("20newsgroups.Rdata")
documents1 <- documents[1:500, 1:50]

documents2 <- matrix(rep(0), nrow = 500)

for (i in 1: 50) {
  documents2  <- cbind(documents2, as.double(documents1[,i]))
}
documents2 <- documents2[,2:51] 

