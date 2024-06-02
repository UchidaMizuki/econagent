test_util_calibrate <- function(f, prices, quantities) {
  utility <- f(quantities)
  expect_equal(util_demand(f, prices, utility = utility), quantities,
               tolerance = 1e-6)
}

test_util_demand <- function(f, prices, income, utility, ...,
                             type = c("marshallian", "hicksian")) {
  if ("marshallian" %in% type) {
    quantities <- util_demand(f, prices,
                              income = income,
                              ...)
    expect_equal(sum(prices * quantities), income,
                 tolerance = 1e-6)
  }

  if ("hicksian" %in% type) {
    quantities <- util_demand(f, prices,
                              utility = utility,
                              ...)
    expect_equal(f(quantities), utility,
                 tolerance = 1e-6)
  }
}

test_util_expenditure <- function(f, prices, utility, ...) {
  income <- util_expenditure(f, prices,
                             utility = utility,
                             ...)
  quantities <- util_demand(f, prices,
                            income = income,
                            ...)
  expect_equal(sum(prices * quantities), income)
}

test_util_indirect <- function(f, prices, income, ...) {
  utility <- util_indirect(f, prices,
                           income = income,
                           ...)
  quantities <- util_demand(f, prices,
                            utility = utility,
                            ...)
  expect_equal(f(quantities), utility,
               tolerance = 1e-6)
}
