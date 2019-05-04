# private function to check probability value
check_prob <- function(prob) {
  if (length(prob) > 1) {
    stop("\n 'prob' must be of length 1")
  }
  if (prob < 0 | prob > 1) {
    stop("\n 'prob' must be a value between 0 and 1")
  }

  TRUE
}

# private function to check trials value
check_trials <- function(trials) {
  if (length(trials) > 1) {
    stop("\n 'trials' must be of length 1")
  }
  if (trials < 0) {
    stop("\n 'trials' must be a value greater than 0")
  }
  TRUE
}

# private function to check success values
check_success <- function(success, trials) {
  for (i in 1:length(success)) {
    if (success[i] > trials) {
      stop("\n 'success' must be a value less than 'trials'")
    }
    if (success[i] < 0) {
      stop("\n 'success' must be a vector of values greater than or equal to 0")
    }
  }
  TRUE
}

