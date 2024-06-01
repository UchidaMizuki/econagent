test_that("util_linear() works", {
  set.seed(1234)

  size <- 5
  efficiency <- runif(1)
  weights <- runif(size)

  f <- util_linear(efficiency = efficiency,
                   weights = weights)

  prices <- runif(size)

  # Marshallian demand
  income <- runif(1)

  grad_quantities <- get_grad_quantities(f, prices, income = income)
  expect_equal(grad_quantities$analytic, grad_quantities$numerical)

  # Hicksian demand
  utility <- runif(1)

  grad_quantities <- get_grad_quantities(f, prices, utility = utility)
  expect_equal(grad_quantities$analytic, grad_quantities$numerical)
})
