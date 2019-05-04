context("Test main functions")

test_that("test that bin_choose raises an error if number of successes (k) is greater than trials (n), returns only one value, and can correctly calculate specified choose values", {
  expect_error(bin_choose(10, 31))
  expect_length(bin_choose(25, 12), 1)
  expect_equal(bin_choose(10,5), 252)
})

test_that("test that bin_probability raises an error if its inputs are negative, returns multiple probabilities if needed, and returns the correct numerical probability value", {
  expect_error(bin_probability(-10, 31, 0.3))
  expect_error(bin_probability(10, -31, 0.3))
  expect_error(bin_probability(10, 31, -0.3))
  expect_length(bin_probability(1:5, 25, 0.12), 5)
  expect_equal(bin_probability(1, 5, 0.5), 0.15625)
})

test_that("test that bin_distribution raises an error if its inputs are negative, returns an object of class data.frame and bindis, and has two columns (success and probability)", {
  expect_error(bin_distribution(-10, 0.5))
  expect_error(bin_distribution(10, -0.5))
  expect_is(bin_distribution(10, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(25, 0.12), 2)
})

test_that("test that bin_cumulative raises an error if its inputs are negative, returns an object of class data.frame and bincum, and has three columns (success, probability, and cumulative)", {
  expect_error(bin_cumulative(-10, 0.5))
  expect_error(bin_cumulative(10, -0.5))
  expect_is(bin_cumulative(10, 0.5), c("bincum", "data.frame"))
  expect_length(bin_cumulative(25, 0.12), 3)
})
