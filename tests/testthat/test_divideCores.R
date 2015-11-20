context("Dividing cores")

test_that("the numbers of tasks divide properly by number of cores", {
  expect_identical(nrow(divide_cores(1000, ncores = parallel::detectCores())), parallel::detectCores())
})

test_that("the output is a data frame", {
  expect_identical(class(divide_cores(1000)), class(data.frame(x=1)))
})

test_that("invalid args are detected", {
  expect_error(divide_cores("pie"))
})

