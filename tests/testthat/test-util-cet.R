test_that("Homothetic CET utility works", {
  set.seed(1234)

  size <- 5
  prices <- runif(size)
  quantities <- runif(size)
  income <- runif(1)
  utility <- runif(1)

  substitution <- 1 + rexp(1)
  f <- util_cet(substitution) |>
    util_calibrate(prices = prices, quantities = quantities)

  test_util_calibrate(f, prices, quantities)
  test_util_demand(f, prices, income = income, utility = utility)
  test_util_expenditure(f, prices, utility = utility)
  test_util_indirect(f, prices, income = income)
  test_util_gradient(f, quantities)
  test_util_demand_gradient(f, prices, income = income, utility = utility)
  test_util_expenditure_gradient(f, prices, utility = utility)
  test_util_indirect_gradient(f, prices, income = income)
})

test_that("Non-homothetic CET utility works", {
  set.seed(1234)

  size <- 5
  prices <- runif(size)
  quantities <- runif(size)
  income <- runif(1)
  utility <- runif(1)

  substitution <- 1 + rexp(1)
  homogeneity <- runif(1)
  efficiency <- runif(1)
  weights <- runif(size)

  f <- util_cet(
    substitution,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights
  )

  test_util_demand(f, prices, income = income, utility = utility)
  test_util_expenditure(f, prices, utility = utility)
  test_util_indirect(f, prices, income = income)
  test_util_gradient(f, quantities)
  test_util_demand_gradient(
    f,
    prices,
    income = income,
    utility = utility,
    type = "marshallian"
  )
  test_util_indirect_gradient(f, prices, income = income)
})
