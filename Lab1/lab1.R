bubble.sort <- function(a) {
  n <- length(a)
  if (n == 1) return(a)
  okay <- FALSE
  while (!okay) {
    okay <- TRUE
    
    for  ( i in 1:(n-1)) {
      if(a[i] > a[i+1]) {
        tmp <- a[i]
        a[i] <- a[i+1]
        a[i+1] <- tmp
        okay <- FALSE
      }
    }
  }
  return(a)
}


qsort <- function(a) {
  if (length(a) > 1) {
    pivot <- a[1]
    l <- a[a<pivot]
    e <- a[a==pivot]
    h <- a[a>pivot]
    a <- c(qsort(l), e, qsort(h))
  }
  return(a)
}


randomized.qsort <- function(a) {
  n <- length(a)
  if (n > 1) {
    pivot <- a[sample(n,size=1)]
    l <- a[a<pivot]
    e <- a[a==pivot]
    g <- a[a>pivot]
    a <- c(randomized.qsort(l),e,randomized.qsort(g)) }
  
  return(a)
}


countingsort <- function(a) {
  n <- length(a); N <- max(a)
  c <- rep(0, N)
  for  (i in 1:n) {
    c[a[i]] <- c[a[i]] + 1
  }
  b <- rep(0, n)
  i <- 1
  for( j in 1:N) {
    if(c[j] > 0) {
      for (k in 1:c[j]) {
        b[i] <- j
        i <- i+1
      }
    }
  }
  return(b)
}
count