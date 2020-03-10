## thompson sampling

prob = c(0.6, 0.4)

this_arm <- function(plays, successes) {
  # vector of successes
  # we add a positive number to avoid zeros
  alpha_s <-  10 + successes
  # vector of failures
  alpha_f <-  10 + plays - successes
  
  # beta density for arm no 1
  m1 <- rbeta(1, alpha_s[1], alpha_f[1])
  # beta density for arm no 2
  m2 <- rbeta(1, alpha_s[2], alpha_f[2])
  
  #choosing which arm to play next
  if(m1>m2) {
    return(1)
  }
    else {
      return(2)
    }
  
}

# two-armed bandit is played using thompson sampling
thompson_bernoulli <- function(prob,n) {
  arm <- rep(0,n)
  reward <- rep(0,n)
  ## array consisting of number of plays for each arm
## array consiting of number of successes for each arm
  plays <- rep(0,2)
  successes <- rep(0,2)
  
  #thompson algorithm
  for (i in 1:n) {
    a <- this_arm(plays,successes)
    r <- runif(1) < prob[a]
     reward[i] <- r
    arm[i] <- a
    plays[a] <- plays[a] + 1
    successes[a] <- successes[a] + r
   
  }
  return(list(arm=arm,reward=reward))
}



# testing 
## computing thompson sampling for given probabilities and on 100000 trials

## checking the rate of success
# if algorithm works, the sum should be close to max(o.3, 0.6)
score <- thompson_bernoulli(prob = prob, n = 100000)

ratio = sum(score$reward) / length(score$reward)
ratio



## e-decreasing algorithms

epsilon_decreasing_n <- function(prob,n) {
  arm <- rep(0,n)
  reward <- rep(0,n)
  ## initial number of plays and number of successes is 0 for each arm
  plays <- rep(0,2) 
  successes <- rep(0,2)
  C = 5000
  ## at first, play each arm once
  for (i in 1:2) {
    a <- i
    r <- runif(1) < prob[a]
    plays[a] <- plays[a] + 1
    successes[a] <- successes[a] + r
    arm[i] <- a
    reward[i] <- r
  }
  ## now follow the epsilon decreasing strategy
  for (i in 3:n) {
    epsilon = min(1, C/n )
    # with probability epsilon, pick an arm uniformly at random
    if (runif(1) < epsilon) {
      a <- sample(2,1)
    } else { # otherwise, choose the "best arm so far".
      a <- which.max(successes/plays)
    }
    ## simulate the reward
    r <- runif(1) < prob[a]
    # update the number of plays, successes
    plays[a] <- plays[a] + 1
    successes[a] <- successes[a] + r
    # record the arm played and the reward received
    arm[i] <- a
    reward[i] <- r
  }
  return(list(arm=arm,reward=reward))
}

epsilon_decreasing_n2 <- function(prob,n) {
  arm <- rep(0,n)
  reward <- rep(0,n)
  ## initial number of plays and number of successes is 0 for each arm
  plays <- rep(0,2) 
  successes <- rep(0,2)
  C = 5000
  ## at first, play each arm once
  for (i in 1:2) {
    a <- i
    r <- runif(1) < prob[a]
    plays[a] <- plays[a] + 1
    successes[a] <- successes[a] + r
    arm[i] <- a
    reward[i] <- r
  }
  ## now follow the epsilon decreasing strategy
  for (i in 3:n) {
    epsilon = min(1, C/(n^2) )
    # with probability epsilon, pick an arm uniformly at random
    if (runif(1) < epsilon) {
      a <- sample(2,1)
    } else { # otherwise, choose the "best arm so far".
      a <- which.max(successes/plays)
    }
    ## simulate the reward
    r <- runif(1) < prob[a]
    # update the number of plays, successes
    plays[a] <- plays[a] + 1
    successes[a] <- successes[a] + r
    # record the arm played and the reward received
    arm[i] <- a
    reward[i] <- r
  }
  return(list(arm=arm,reward=reward))
}

# testing



score_t <- thompson_bernoulli(prob = prob, n = 100000)
mean(sum(score_t$reward) / length(score_t$reward))


# plots

avg_reward_t <- c(0)
for (i in 1:length(score_t$reward)) {
  avg_reward_t[i] <- mean(c(score_t$reward[1:i]))
}

plot(avg_reward_t, ylab = "Average rewardusing THompson Sampling Algorithm", 
     type = "l", xlab = "Number of trials", main = "Thompson Sampling",ylim = c(0.5, 0.65))


score_n <- epsilon_decreasing_n(prob = prob, n = 50000)
mean(sum(score_e_n$reward) / length(score_e_n$reward))
avg_reward_n <- c(0)
for (i in 1:length(score_n$reward)) {
  avg_reward_n[i] <- mean(c(score_n$reward[1:i]))
}

plot(avg_reward_n, ylab = "Average reward", 
     type = "l", xlab = "Number of trials", main = "Performance  using epsilon = C/n",ylim = c(0.4, 0.6))

