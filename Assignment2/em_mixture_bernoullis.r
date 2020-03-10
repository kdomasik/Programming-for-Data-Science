## EM algorithm for a mixture of Bernoullis

## logsumexp(x) returns log(sum(exp(x))) but performs the computation in a more stable manner
logsumexp <- function(x) return(log(sum(exp(x - max(x)))) + max(x))

prob <- function(x,mu,return.log=FALSE) {
  l <- sum(log(mu[x==1]))+sum(log(1-mu[x==0]))
  if (return.log) {
    return(l)
  } else {
    return(exp(l))
  }
}

compute_ll <- function(xs,mus,lws,gammas) {
  ll <- 0
  n <- dim(xs)[1]
  K <- dim(mus)[1]
  for (i in 1:n) {
    for (k in 1:K) {
      if (gammas[i,k] > 0) {
        ll <- ll + gammas[i,k]*(lws[k]+prob(xs[i,],mus[k,],return.log=TRUE)-log(gammas[i,k]))
      }
    }
  }
  return(ll)
}



em_mix_bernoulli <- function(xs,K,start=NULL,max.numit=Inf) {
  p <- dim(xs)[2]
  n <- dim(xs)[1]
  
  # lws is log(ws)
  # we work with logs to keep the numbers stable
  # start off with ws all equal
  lws <- rep(log(1/K),K)
  
  if (is.null(start)) {
    mus <- .2 + .6*xs[sample(n,K),]
  } else {
    mus <- start
  }
  gammas <- matrix(0,n,K)
  
  converged <- FALSE
  numit <- 0
  ll <- -Inf
  print("iteration : log-likelihood")
  while(!converged && numit < max.numit) {
    numit <- numit + 1
    mus.old <- mus
    ll.old <- ll
    
    ## E step - calculate gammas
    for (i in 1:n) {
      # the elements of lprs are log(w_k * p_k(x)) for each k in {1,...K}
      lprs <- rep(0,K)
      for (k in 1:K) {
        lprs[k] <- lws[k] + prob(xs[i,],mus[k,],return.log=TRUE)
      }
      # gammas[i,k] = w_k * p_k(x) / sum_j {w_j * p_j(x)}
      gammas[i,] <- exp(lprs - logsumexp(lprs))
    }
    
    ll <- compute_ll(xs,mus,lws,gammas)
    # we could also compute the log-likelihood directly below
    # ll <- compute_ll.direct(xs,mus,lws)
    
    # M step - update ws and mus
    Ns <- rep(0,K)
    for (k in 1:K) {
      Ns[k] <- sum(gammas[,k])
      lws[k] <- log(Ns[k])-log(n)
      
      mus[k,] <- rep(0,p)
      for (i in 1:n) {
        mus[k,] <- mus[k,]+gammas[i,k]/Ns[k]*xs[i,]
      }
    }
    # to avoid a numerical issue since each element of mus must be in [0,1]
    mus[which(mus > 1,arr.ind=TRUE)] <- 1 - 1e-15
    if (numit < 50) {
      print(paste(numit,": ",ll))}
    # we stop once the increase in the log-likelihood is "small enough"
    if (abs(ll-ll.old) < 1e-5) converged <- TRUE
  }
  return(list(lws=lws,mus=mus,gammas=gammas,ll=ll))
}

# check on documents
load("20newsgroups.Rdata")
sampl <- sample(nrow(documents), 100)
m <- 0
for (i in 1:10) {
documents1 <- documents[sampl,]
newsgroups.onehot2 <- newsgroups.onehot[sampl,]
res <- em_mix_bernoulli(documents1,4)
preds <- matrix(0, nrow = dim(documents1)[1], ncol = 4)
for (i in 1:dim(documents1)[1]) {
  best <- 0
  for (j in 1:4) {
    if (res$gammas[i,j] > best ) {
      best <- res$gammas[i,j]
      c <- j
    }

  } 
  preds[i,c] <- 1

}

preds2 <- matrix(0, nrow = dim(documents1)[1], ncol = 4)
preds2[,1] <- preds[,4]
preds2[,2] <- preds[,3]
preds2[,3] <- preds[,1]
preds2[,4] <- preds[,2]
loss <- 0
for (i in 1:dim(documents1)[1]) {
  for (j in 1:4) {
    if(preds2[i,j] == 1 && newsgroups.onehot2[i,j] != preds2[i,j])
      loss <- loss + 1
    }
    
}   
m <- m + (1-(loss/dim(documents1)[1]))

}
m