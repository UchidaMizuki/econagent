test_that("fixed_point_positive() keeps zero elements fixed", {
  x <- fixed_point_positive(\(x) sqrt(x * c(4, 9, 1)), c(1, 1, 0))

  expect_equal(x, c(4, 9, 0), tolerance = 1e-6)
})

test_that("fixed_point_positive() returns all-zero inputs unchanged", {
  expect_equal(fixed_point_positive(\(x) x + 1, c(0, 0)), c(0, 0))
})
