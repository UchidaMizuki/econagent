test_that("Sign-split utility works", {
  set.seed(1234)

  size <- 5
  prices <- runif(size)
  quantities <- runif(size)
  quantities[[size]] <- 0
  quantities[c(1, 2)] <- -quantities[c(1, 2)]
  income <- runif(1)
  utility <- runif(1)

  f <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = prices, quantities = quantities)

  test_util_calibrate(f, prices, quantities)
  test_util_demand(f, prices, income = income, utility = utility)
  test_util_expenditure(f, prices, utility = utility)
  test_util_indirect(f, prices, income = income)
})

test_that("util_sign_split() works with no negative quantities", {
  set.seed(1234)

  size <- 5
  prices <- runif(size)
  quantities <- runif(size)
  quantities[[size]] <- 0

  f_split <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = prices, quantities = quantities)
  f_plain <- util_cobb_douglas() |>
    util_calibrate(prices = prices, quantities = quantities)

  expect_equal(f_split(quantities), f_plain(quantities))
  test_util_calibrate(f_split, prices, quantities)
})

test_that("util_sign_split() works with all negative quantities", {
  set.seed(1234)

  size <- 5
  prices <- runif(size)
  quantities <- -runif(size)

  f <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = prices, quantities = quantities)

  expect_equal(f(quantities), sum(prices * quantities))
  test_util_calibrate(f, prices, quantities)
})

test_that("util_sign_split() preserves the calibrated branch ratio under demand", {
  prices <- c(1, 1, 1, 1)
  quantities <- c(8, 2, -3, -1)

  f <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = prices, quantities = quantities)

  ratio <- sum(quantities[1:2]) / sum(quantities[3:4])

  for (target in c(3, 6, 12)) {
    demand <- util_demand_hicksian(f, prices, utility = target)
    expect_equal(sum(demand), target, tolerance = 1e-6)
    expect_equal(
      sum(demand[1:2]) / sum(demand[3:4]),
      ratio,
      tolerance = 1e-6
    )
  }
})

test_that("util_sign_split() demand does not respond to relative price changes", {
  prices <- c(1, 1, 1, 1)
  quantities <- c(8, 2, -3, -1)

  f <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = prices, quantities = quantities)

  new_prices <- c(1.2, 1.2, 1, 1)
  demand <- util_demand_hicksian(f, new_prices, utility = f(quantities))

  expect_equal(demand, quantities, tolerance = 1e-6)
})

test_that("util_sign_split() gradient is not available", {
  f <- util_sign_split(util_cobb_douglas()) |>
    util_calibrate(prices = c(1, 1, 1), quantities = c(2, 1, -1))

  expect_snapshot(
    util_demand_marshallian(f, c(1, 1, 1), income = 1, gradient = TRUE),
    error = TRUE
  )
})
