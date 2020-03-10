
merge <- function(a, b) {
  l <- length(a) + length(b)
  i <- 1
  j <- 1
  k <- 1
  d <- numeric(0)
  while (i <= length(a) && j <= length(b)) {
    if (a[i] < b[j]) {
      d[k] = a[i]
      i = i + 1
      k = k + 1
    } else {
      d[k] = b[j]
      j = j + 1
      k = k + 1
    }
  }
  while (i <= length(a)) {
    d[k] = a[i]
    i = i + 1
    k = k + 1
  }
  while (j <= length(b)) {
    d[k] = b[j]
    j = j + 1
    k = k + 1
  }
  return(d)
}


majority_element <- function(a) {
  len<- length(a)
  half_len <- length(a) / 2
  if (len == 1) {
    # if the array has length 1, this element is the majority  element. 
    return(a)
  }
  else if (len == 2) {
    # if an array has length 2, then the majority element exists
    # only is these elements are equal
    if (a[1] == a[2]) {
      return(a[1])
    } else {
      return("No majority element")
    }
  }
  else if (len > 2) {
    # if array has length > 2, we must halve it and call majority_element for both of the halves
    m1 <- majority_element(a[1:half_len])
    m2 <- majority_element(a[(half_len + 1):len])
    if (m1 == m2) {
      # if  both halves have the same majority element, it  must also be
      # the majority element of the whole array
      return(m1)
    }
    else if (m1 != "No majority element" && m2 =="No majority element") {
      # if only  1st half of the array has a majority element
      # we must check ifthis number is also a majority element
      # of the whole array
      lc <- sum(a == m1)
      if (lc > half_len) {
        return(m1)
      } 
      # if it is not, then the array has no majority element
      else {
        return("No majority element")
        3
      }
    }
    else if (m2 != "No majority element" && m1 == "No majority element") {
      # if only  2nd half of the array has a majority element
      # we must check ifthis number is also a majority element
      # of the whole array
      rc <- sum(a == m2)
      if (rc > half_len) {
        return(m2)
      }
      # if it is not, then the array has no majority element
      else {
        return("No majority element")
      }
    }
    else if (m2 != "No majority element" && m1 != "No majority element" && m1 != m2){
      # if both halves  have different majority elements,  we  must check
      # if any of  them is the majority element of the whole array
      lc <- sum(a == m1)
      rc <- sum(a == m2)
      if (lc > half_len) {
        return(m1)
      }
      else if (rc > half_len) {
        return(m2)
      } 
      # If not, that means, that there is no majority element of this array
      else {
        return("No majority element")
      }
    } else {
      # if none of the halves have a majority element, then there is no majority element
      return("No majority element")
    }
  }
}
# Testing the function
majority_element(c(1,2,3,4,1,2,3,6))
majority_element(c(4,5,4,6,4,7,4,4))
majority_element(c(1:20))




# load the image
load("pictures.rdata")
img <- images[[4]]

# calculate the dimensions ofimg

dims <- dim(img)
m <- dims[1]
n <- dims[2]

# matrix A holds grayscale version of the image

A <- matrix(0,m,n)

for (i in 1:m) {
  for (j in 1:n) {
    A[i,j] <- sum(img[i,j,])/3
  }
}

# SVD decomposition

dec <- svd(A)

best_loss <- Inf
best_k <- -1

# use a look between 1 and min(m,n) to find k that minimises the function

for(k in 1:min(m,n)) {
  if (k==1) {
    B <- dec$d[1] * dec$u[,1]%*%t(dec$v[,1])
  }
  else {
    B <- dec$u[,1:k]%*%diag(dec$d[1:k])%*%t(dec$v[,1:k])
  }
  
  # calculate Frobenius norm of A and approx B
  F_norm <- norm(A - B, type = "F")
  #loss specified by the function
  loss <- exp(F_norm) + k*(m+n+1)
  # best value ofk
  if(loss< best_loss) {
    best_loss <- loss
    best_k <- k
  }
}
print(best_k)