context("Dividing cores")

test_that("the numbers divide properly", {
  expect_identical(nrow(divide_cores(1000, ncores = parallel::detectCores())), parallel::detectCores())
})
