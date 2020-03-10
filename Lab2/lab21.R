
# Part1

G <- matrix(rnorm(10), ncol = 5, nrow = 4)
tmp <- svd(G)
U <- tmp$u; d <- tmp$d; V <- tmp$v
print(U%*%diag(d)%*%t(V))

all.equal(G, U%*%diag(d)%*%t(V))
plot(d)

G_2 <- U[,1:2]%*%diag(d[1:2])%*%t(V[,1:2])
print(norm(G-G_2, type = 'F'))
# Part2



# Part2


load("viruses.rdata")
load("pictures.rdata")
source("svd.image.compression.R")
image.compression()
library(MASS)

res <- image.compress.param(1)
par(mfrow=c(1,2))
viewImage(res$mtx)
plot(res$svd$d)
abline(h=0, lty=2)
k <- 10
CompressedImage <- compute.compression(k, res$p, res$mtx, res$svd)
par(mfrow = c(1,2))
if (!is.null(CompressedImage))   {
  viewImage(res$mtx)
  viewImage(CompressedImage)
}
# part3

library(MASS)
?crabs
Crabs <- crabs[,4:8]
Crabs.class <- factor(paste(crabs[,1],crabs[,2],sep=""))
plot(Crabs,col=Crabs.class,pch=20)

Crabs.pca <- prcomp(Crabs,scale.=TRUE)
plot(Crabs.pca)
plot(Crabs.pca$x[,1:2],col=Crabs.class,pch=20)
pairs(Crabs.pca$x[,1:5],col=Crabs.class,pch=20)
scaledCrabs <- scale(Crabs)
Crabs.svd <- svd(scaledCrabs)
print(Crabs.svd$v - Crabs.pca$rotation)
Crabs.pcs <- scaledCrabs%*%Crabs.svd$v
print(norm(Crabs.pcs - Crabs.pca$x))



# Part4
X <- scale(allviruses)
viruses.pca <- prcomp(X)
groups <- rep(0,61)
groups[1:3] <- 1
groups[4:9] <- 2
groups[10:48] <- 3
groups[49:61] <- 4
group.names <- c("Hordeviruses","Tobraviruses",
                 "Tobamoviruses","furoviruses")
plot(viruses.pca)
pairs(viruses.pca$x[,1:5],col=groups,pch=20)
plot(viruses.pca$x[,1:2],pch=20,col=groups)

X <- scale(tobamoviruses)
tobamoviruses.pca <- prcomp(X)
plot(tobamoviruses.pca)

plot(tobamoviruses.pca$x[,1:2],pch=20)


