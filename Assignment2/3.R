distances.l1 <- function(x,y) {
  d <- 0
  for (i in 1:length(x)) {
    d <- d + abs(x[[i]]-y[[i]])
  }
  return(d)
}

distances.l2 <- function(x,y) {
  d <- 0
  for (i in 1:length(x)) {
    d <- d + (x[[i]]-y[[i]])^2
  }
  return(d)
}



knn.regression.test <- function(k,train.X,train.Y,test.X,test.Y,distances) {

  #algorithm can't run if k is larger than number of observations in training dataset
  if(k > nrow(train.X)) {
return("ERROR!!!")  }
      estimates <- numeric(0)

  # we need two loops
  # first over test data
  for(i in 1:nrow(test.X)) {
    indexes <- numeric(0)
    calculated_distances <- numeric(0)
    
    
    #second over train data
    for (j in 1: nrow(train.X)) {
      calculated_distances <- c(calculated_distances,
                                distances(c(test.X[i,], test.Y[i]), 
                                          c(train.X[j,], train.Y[j])))
    indexes <- c(indexes, j)
    }
    
    #creating the matrix  with euclidian distances between labaled points and the new point
    classification <- data.frame(calculated_distances, indexes)
    
    # using order function to sort distances in order to find nearest neighbors
    classification <- classification[order(classification$calculated_distances),]
    
    #selecting k nearest neighbors
    classification <- classification[1:k, ]
    
    #calculating inverse-distance weights as asked
    inv_weighted_distances <- sum(train.Y[classification$indexes] * calculated_distances[classification$indexes] ^ (-1))/
      sum(calculated_distances[classification$indexes] ^ (-1))
      estimates <- c(estimates, inv_weighted_distances)
    #print(estimates)
    
  }

  
  loss <- sum((as.vector(test.Y) - estimates) ^ 2)
  return(list(loss, estimates))
}


# toy dataset 1
set.seed(3000)
n <- 100
train.X <- matrix(sort(rnorm(n)),n,1)
train.Y <- (train.X < -0.5) + train.X*(train.X>0)+rnorm(n,sd=0.03)
plot(train.X,train.Y)
test.X <- matrix(sort(rnorm(n)),n,1)
test.Y <- (test.X < -0.5) + test.X*(test.X>0)+rnorm(n,sd=0.03)
k <- 2
knn.regression.test(5,train.X,train.Y,test.X,test.Y,distances.l1)


# toy dataset 2
train.X <- matrix(rnorm(200),100,2)
train.Y <- train.X[,1]
test.X <- matrix(rnorm(100),50,2)
test.Y <- test.X[,1]
k <- 3
knn.regression.test(k,train.X,train.Y,test.X,test.Y,distances.l1)

# FInd best values of k 
least_loss <- Inf
best_k <-0

for (k in 1:nrow(test.X)) {
  loss <- knn.regression.test(k,train.X,train.Y,test.X,test.Y,distances.l1)
  if(least_loss > loss) {
    least_loss <- loss
    best_k <- k
  }

}
print(best_k)
print(least_loss)



# Iowa data

library("lasso2")
library(ggplot2)
data(Iowa)
train.X=as.matrix(Iowa[seq(1,33,2),1:9])
train.Y=c(Iowa[seq(1,33,2),10])
test.X=as.matrix(Iowa[seq(2,32,2),1:9])
test.Y=c(Iowa[seq(2,32,2),10])
k <- 5
results <- knn.regression.test(k,train.X,train.Y,test.X,test.Y,distances.l1)
predictions <- data.frame(Year = seq(1931, 1961, by = 2), actual = test.Y,
                      predicted = results[[2]])

ggplot(predictions, aes(Year, y = Yield, color = Key)) +
  geom_line(aes(y = actual, col = "Actual")) +
  geom_line(aes(y = predicted, col = "Predicted")) +
  ggtitle("kNN with k = 5")

data <- data.frame(train.X, train.Y)
least_squares <- lm(train.Y ~ Rain0 + Rain1 + Rain2 + Rain3  + Temp1 + Temp2 + Temp3 + Temp4, data = data)
preds_ls <- predict(least_squares, as.data.frame(test.X))

library(MASS)
ridge <- lm.ridge(train.Y ~ Year + Rain0 + Rain1 + Rain2 + Rain3  + Temp1 + Temp2 + Temp3 + Temp4, data = data)
preds_r <- coef(ridge)[1]

for (i in 2:ncol(train.X)) {
  preds_r <- preds_r + coef(ridge)[i]*test.X[, i - 1]
}
