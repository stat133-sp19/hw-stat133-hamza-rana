context("Test 'checking' functions")

test_that("test that check_prob raises an error if prob is not between 0 and 1, raises an error if the length of prob is greater than one, and functions properly with a valid prob value", {
  expect_error(check_prob(125))
  expect_error(check_prob(c(0.1, 0.2)))
  expect_true(check_prob(0.3465))
})

test_that("test that check_trials takes positive values, only takes an argument of length 1, and raises an error if the input is negative", {
  expect_true(check_trials(15))
  expect_error(check_trials(c(11, 23)))
  expect_error(check_trials(-25))
})

test_that("test that check_success can take multiple positive values, raises an error if length of success is greater than trials, and raises an error if the value of success is negative", {
  expect_true(check_success(1:10, 20))
  expect_error(check_success(100, 10))
  expect_error(check_success(-5, 10))
})
