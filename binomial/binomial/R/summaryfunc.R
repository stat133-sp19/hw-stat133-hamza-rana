# private auxiliary function to check the mean of the binomial distribution
aux_mean <- function(trials = 10, prob = 0.5) {
  trials*prob
}

# private auxiliary function to check the variance of the binomial distribution
aux_variance <- function(trials = 10, prob = 0.5) {
  trials*prob*(1-prob)
}

# private auxiliary function to check the mode of the binomial distribution
aux_mode <- function(trials = 10, prob = 0.5) {
  n <- trials*prob + prob
  if (n %% 1 == 0) {
    return(c(n-1,n))
  }
  else {
    return(floor(n))
  }
}

# private auxiliary function to check the skewness of the binomial distribution
aux_skewness <- function(trials = 10, prob = 0.5) {
  (1-2*prob)/sqrt(trials*prob*(1-prob))
}

# private auxiliary function to check the kurtosis of the binomial distribution
aux_kurtosis <- function(trials = 10, prob = 0.5) {
  (1 - 6*prob*(1-prob))/(trials*prob*(1-prob))
}
