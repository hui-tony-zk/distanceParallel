context("Creating Distance Matrix")

test_that("length of output is correct", {
  matrix <- replicate(20, rnorm(10))
  expect_identical(
    nrow(matrix),
    nrow(data.matrix(create_distance_matrix(create_pairwise_comparisons(matrix))))
    )
})

test_that("the output is symmetrical", {
  matrix <- replicate(20, rnorm(10))
  tmp <- data.matrix(create_distance_matrix(create_pairwise_comparisons(matrix)))
  expect_true(isSymmetric(tmp))
})

test_that("invalid args are detected", {
  expect_error(create_distance_matrix("hi"))
})
