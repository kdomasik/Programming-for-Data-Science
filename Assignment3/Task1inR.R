# Gradient Descent function given
gradient.descent <- function(f, gradf, x0, iterations=1000, eta=0.2) {
  x<-x0
  for (i in 1:iterations) {
    if(i<50) {
    cat(i,"/",iterations,": ",x," ",f(x),"\n")
    x<-x-eta*gradf(x)
  }}
  x
 }
# example given
f <-function(x) { sum(x^2) }
gradf<-function(x) { 2*x }
gradient.descent(f,gradf,c(10,20),10,0.2)

# write short gradient ascend function

gradient.ascent <- function(f, df, x0, iterations=1000, eta=0.2) {
  gradient.descent(f, df, x0, iterations, -eta)
}

# test the function with the example privided
f <-function(x) { (1+x^2)^(-1) }
gradf<-function(x) { -2*x*(1+x^2)^(-2) }
gradient.ascent(f,gradf,3,40,0.5)

# write function do calculate gradient of  f
f <- function(x) (x[1]-1)^2 + 100*(x[1]^2-x[2])^2
gradf <- function(x) {
  return (c( 2 * x[1] - 2 + 400 * x[1]^3 - 400 * x[2] * x[1],
            -200 * x[1]^2 + 200* x[2]))
}
gradient.descent(f, gradf, c(3, 4), 50, 0.0007)

gradient.momentum <- function(f, gradf, x0, iterations = 1000, eta = 0.2, alpha) {
  x1 <- x0
  x2 <- x1 - eta * gradf(x1)
  cat(1, " : ", x1, ", ", f(x1), "\n")
  cat(2, " : ", x2, ", ", f(x2), "\n")
  for( i in 3:iterations) {
    x <- x2 - eta * gradf(x2) + alpha * (x2 - x1)
    
    x1 <- x2
    x2 <- x
    if(i < 20) {
    cat(i, " : ", x, ", ", f(x), "\n")
  }
  }
  x

}

gradient.momentum(f, gradf, c(3,4), 20000, 0.00049, 0.03)

 # exercise 2

load("mnist.tiny.RData")
train.X=train.X/255
test.X=test.X/255
library(grid)
grid.raster(array(aperm(array(train.X[1:50,],c(5,10,28,28)),c(4,1,3,2)),c(140,280)),
            interpolate=FALSE)
library(e1071)

svm(train.X,train.labels,type="C-classification",kernel="linear",cross=3)$tot.accuracy


svm(train.X,train.labels,type="C-classification",kernel="poly",degree=2,coef=1,cross=3)$tot.accuracy

svm(train.X,train.labels,type="C-classification",kernel="poly",degree=5,coef=1,cross=3)$tot.accuracy

svm(train.X,train.labels,type="C-classification",kernel="poly",degree=10,coef=1,cross=3)$tot.accuracy

svm(train.X,train.labels,type="C-classification",kernel="poly",degree=13,coef=1,cross=3)$tot.accuracy

svm(train.X,train.labels,type="C-classification",kernel="poly",degree=15,coef=1,cross=3)$tot.accuracy

svm(train.X,train.labels,type="C-classification",kernel="poly",degree=16,coef=1,cross=3)$tot.accuracy

set.seed(1)
svm(train.X, train.labels, type = "C-classification", kernel = "radial",
    gamma = 1, coef = 1, cross = 3)$tot.accuracy

set.seed(1)
svm(train.X, train.labels, type = "C-classification", kernel = "radial",
    gamma = 0.5, coef = 1, cross = 3)$tot.accuracy

set.seed(1)
svm(train.X, train.labels, type = "C-classification", kernel = "radial",
    gamma = 0.01, coef = 1, cross = 3)$tot.accuracy
set.seed(1)
svm(train.X, train.labels, type = "C-classification", kernel = "radial",
gamma = 0.03, coef = 1, cross = 3)$tot.accuracy

set.seed(1)
svm(train.X, train.labels, type = "C-classification", kernel = "radial",
    gamma = 0.02, coef = 1, cross = 3)$tot.accuracy


# For the RBF kernels, write a grid search function that takes two lists, log.C.range and
#log.gamma.range, and for each pair (lc,lg) of entries in the pair of lists attempts cross-validation
#with parameters cost = exp(lc) and gamma=exp(lg). Once you have found the model with the best
#cross-validation error, train it on the full tiny' training set and then test it on thetiny’ test
#set.


log.C.range <- log(c(0.001, 0.01, 0.1, 1, 10, 100))
log.gamma.range <- log(c(0.001, 0.01, 0.1, 1, 10, 100))

comb <- matrix(ncol = 2, nrow = length(log.gamma.range) * length(log.C.range))
count <- 0
results <- c()

for(i in 1:length(log.C.range)) {
  for(j in 1:length(log.gamma.range)) {
    count <- count + 1
    comb[count, ] <- c(gamma = exp(log.C.range[i]), exp(log.gamma.range[j]))
  }
}


for ( i in 1:nrow(comb)) {
  accuracy <- svm(train.X, train.labels, type = "C-classification", kernel = "rad",
                  cost = comb[i,1], gamma = comb[i,2], degree = 2, coef = 1, cross = 3)$tot.accuracy
  results <- c(results, accuracy)
}

row <- which.max(results)
comb[row, ]
max(results)

# train model on tiny training set

model <- svm(train.X, train.labels, type = "C-classification", kernel = "rad",
             cost = 100, gamma = 0.01, degree = 2, coef = 1)
test.predictions <- as.vector(predict(model, test.X), mode = "numeric")

accuracy <- sum(diag(table(test.labels, test.predictions)))/sum(table(test.labels, test.predictions))

accuracy