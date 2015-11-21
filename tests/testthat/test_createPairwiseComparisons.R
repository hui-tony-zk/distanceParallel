context("Creating Pairwise Comparisons")

test_that("length of output is correct", {
  matrix <- replicate(20, rnorm(10))
  expect_identical(
    nrow(create_pairwise_comparisons(matrix, ncores = 1)),
    ncol(combn(nrow(replicate(20, rnorm(10))),2))
    )
})

test_that("the output is a data.frame", {
  expect_identical(class(create_pairwise_comparisons(replicate(20, rnorm(10)), ncores = 1)), class(data.frame()))
})

test_that("invalid args are detected", {
  expect_error(create_pairwise_comparisons("hi", ncores = 1))
})
