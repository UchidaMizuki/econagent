test_that("Linear utility works", {
  set.seed(1234)

  size <- 5
  efficiency <- runif(1)
  weights <- runif(size)
  prices <- runif(size)
  quantities <- runif(size)
  quantities[size] <- 0
  income <- runif(1)
  utility <- runif(1)

  f <- util_linear(efficiency = efficiency,
                   weights = weights)

  test_util_demand(f, prices,
                   income = income,
                   utility = utility)
  test_util_expenditure(f, prices,
                        utility = utility)
  test_util_indirect(f, prices,
                     income = income)
  test_util_gradient(f, quantities)
  test_util_demand_gradient(f, prices,
                            income = income,
                            utility = utility)
  test_util_expenditure_gradient(f, prices,
                                 utility = utility)
  test_util_indirect_gradient(f, prices,
                              income = income)
})
