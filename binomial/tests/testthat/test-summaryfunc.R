context("Test auxiliary summary functions")

test_that("test that aux_mean returns a value of type double, check that the length of the returned value is 1, and that the correct numerical value is calculated", {
  expect_type(aux_mean(10, 0.3), 'double')
  expect_length(aux_mean(10, 0.3), 1)
  expect_equal(aux_mean(10, 0.3), 3)
})

test_that("test that aux_variance returns a value of type double, check that the length of the returned value is 1, and that the correct numerical value is calculated", {
  expect_type(aux_variance(10, 0.3), 'double')
  expect_length(aux_variance(10, 0.3), 1)
  expect_equal(aux_variance(10, 0.3), 2.1)
})

test_that("test that aux_mode returns a value of type double, check that the length of the returned value is 1, and that the correct numerical value is calculated", {
  expect_type(aux_mode(10, 0.3), 'double')
  expect_length(aux_mode(10, 0.3), 1)
  expect_equal(aux_mode(10, 0.3), 3)
})

test_that("test that aux_skewness returns a value of type double, check that the length of the returned value is 1, and that the correct numerical value is calculated", {
  expect_type(aux_skewness(10, 0.3), 'double')
  expect_length(aux_skewness(10, 0.3), 1)
  expect_lte(aux_skewness(10, 0.3) - 0.2760262, 0.0000001)
})

test_that("test that aux_kurtosis returns a value of type double, check that the length of the returned value is 1, and that the correct numerical value is calculated", {
  expect_type(aux_kurtosis(10, 0.3), 'double')
  expect_length(aux_kurtosis(10, 0.3), 1)
  expect_lte(aux_kurtosis(10, 0.3) - (-0.1238095), 0.0000001)
})
