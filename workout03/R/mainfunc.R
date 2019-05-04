#' @title bin_choose
#' @description Calculates the number of combinations in which k successes occur within n trials
#' @param n number of trials
#' @param k number of successes
#' @return the number of combinations calculated
#' @export
#' @examples
#'
#' # default
#' bin_choose()
#'
#' # another example
#' bin_choose(n = 5, k = 3)
#'
bin_choose <- function(n = 10, k = 3) {
  if (sum(k > n) > 0) {
    stop("\n k cannot be greater than n")
  }

  factorial(n)/(factorial(k)*factorial(n - k))
}

#' @title bin_probability
#' @description Calculates the probability of a given number of successes in a given numbers of trials with a specified probability
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @param success number of successes
#' @return the probability of the given case
#' @export
#' @examples
#'
#' # default
#' bin_probability()
#'
#' # another example
#' bin_probability(success = 1, trials = 3, prob = 0.5)
#'
bin_probability <- function(success = 2, trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  bin_choose(trials, success)*(prob^success)*((1-prob)^(trials-success))
}

#' @title bin_distribution
#' @description creates a data frame with a distribution of probabilites for the specified number of trials
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the probability distribution specified by the parameters
#' @export
#' @examples
#'
#' # default
#' bin_distribution()
#'
#' # another example
#' bin_distribution(trials = 3, prob = 0.5)
#'
bin_distribution <- function(trials = 5, prob = 0.5) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  df <- data.frame(success, probability)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

#' @export
plot.bindis <- function(dis1) {
  barplot(dis1$probability, names.arg = dis1$success, xlab = "Successes", ylab = "Probability")
}

#' @title bin_cumulative
#' @description creates a data frame with a distribution of probabilites for the specified number of trials along with cumulative probabilities
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the probability and cumulative probability distribution specified by the parameters
#' @export
#' @examples
#'
#' # default
#' bin_cumulative()
#'
#' # another example
#' bin_cumulative(trials = 3, prob = 0.5)
#'
bin_cumulative <- function(trials = 5, prob = 0.5) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  cumulative <- bin_probability(0:trials, trials, prob)
  for (i in 2:(trials+1)) {
    cumulative[i] <- bin_probability(i-1, trials, prob) + cumulative[i-1]
  }
  df <- data.frame(success, probability, cumulative)
  class(df) <- c("bincum", "data.frame")
  return(df)
}

#' @export
plot.bincum <- function(dis1) {
  plot(x = dis1$success, y = dis1$cumulative, xlab = "Successes", ylab = "Probability", type = 'o')
}

#' @title bin_variable
#' @description creates a binomial variable object
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return a list with the number of trials and probability of success of the binomial variable
#' @export
#' @examples
#'
#' # default
#' bin_variable()
#'
#' # another example
#' bin_variable(trials = 3, prob = 0.5)
#'
bin_variable <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)

  x <- list("trials" = trials, "prob" = prob)
  class(x) = "binvar"
  return(x)
}

#' @export
print.binvar <- function(bin1) {
  cat('"Binomial Variable"\n\n')
  cat('Parameters\n')
  cat('- number of trials: ')
  cat(bin1$trials)
  cat('\n- prob of success: ')
  cat(bin1$prob)
}

#' @export
summary.binvar <- function(bin1) {
  x <- list("trials" = bin1$trials, "prob" = bin1$prob, "mean" = aux_mean(bin1$trials, bin1$prob), "variance" = aux_variance(bin1$trials, bin1$prob), "mode" = aux_mode(bin1$trials, bin1$prob), "skewness" = aux_skewness(bin1$trials, bin1$prob), "kurtosis" = aux_kurtosis(bin1$trials, bin1$prob))
  class(x) = "summary.binvar"
  return(x)
}

#' @export
print.summary.binvar <- function(binsum1) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat('- number of trials: ')
  cat(binsum1$trials)
  cat('\n- prob of success: ')
  cat(binsum1$prob)
  cat('\n\nMeasures')
  cat('\n- mean: ')
  cat(binsum1$mean)
  cat('\n- variance: ')
  cat(binsum1$variance)
  cat('\n- mode: ')
  cat(binsum1$mode)
  cat('\n- skewness: ')
  cat(binsum1$skewness)
  cat('\n- kurtosis: ')
  cat(binsum1$kurtosis)

}

#' @title bin_mean
#' @description calculates the mean of the given binomial distribution
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the numerical mean of the given data
#' @export
#' @examples
#'
#' # default
#' bin_mean()
#'
#' # another example
#' bin_mean(trials = 3, prob = 0.5)
#'
bin_mean <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)

}

#' @title bin_variance
#' @description calculates the variance of the given binomial distribution
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the numerical value of the variance of the given data
#' @export
#' @examples
#'
#' # default
#' bin_variance()
#'
#' # another example
#' bin_variance(trials = 3, prob = 0.5)
#'
bin_variance <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)

}

#' @title bin_mode
#' @description calculates the mode of the given binomial distribution
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the numerical mode of the given data
#' @export
#' @examples
#'
#' # default
#' bin_mode()
#'
#' # another example
#' bin_mode(trials = 3, prob = 0.5)
#'
bin_mode <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)

}

#' @title bin_skewness
#' @description calculates the skewness of the given binomial distribution
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the numerical skewness value of the given data
#' @export
#' @examples
#'
#' # default
#' bin_skewness()
#'
#' # another example
#' bin_skewness(trials = 3, prob = 0.5)
#'
bin_skewness <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)

}

#' @title bin_kurtosis
#' @description calculates the mean of the given binomial distribution
#' @param trials number of trials
#' @param prob probability of success within one trial
#' @return the numerical mean of the given data
#' @export
#' @examples
#'
#' # default
#' bin_kurtosis()
#'
#' # another example
#' bin_kurtosis(trials = 3, prob = 0.5)
#'
bin_kurtosis <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)

}
