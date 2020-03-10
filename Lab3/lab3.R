library(Matrix)
load("web-google.rdata")
n <- max(links)
numlinks <- dim(links)[1]

A.sparse <- sparseMatrix(i=links[,1], 
                         j=links[,2], x=rep(1,numlinks), dims=c(n,n))


outlinks <- rep(0,n)

for (i in 1:numlinks) {
  outlinks[links[i,1]] <- outlinks[links[i, 1]] +1
  
}

d <- outlinks == 0

vsH <- rep(0, numlinks)

for( k in 1:numlinks) {
  vsH[k] <- 1/outlinks[links[k,1]]
}

H.sparse <- sparseMatrix(i=links[,1], 
                        j=links[,2], x=vsH, dims = c(n,n))

alpha = 0.85
w <- rep(1/n, n)
p <- log(1+(1:n)/sum(log(1+1:n)))


m<- 150

diffs <- rep(0,n)
muT <- t(rep(1/n, n))
for (i in 1:m) {
  muT.old <- muT

  # my code
  diff <- sum(abs(muT-muT.old))
  diffs[i] <- diff(print(paste("iteration"), i, ":||muT - muT.old||_1 = diff", sep = " "))
}