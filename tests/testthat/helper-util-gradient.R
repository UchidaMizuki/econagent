test_util_gradient <- function(f, quantities) {
  analytic <- util_gradient(f, quantities)
  numerical <- util_gradient_numerical(f, quantities)
  expect_equal(analytic, numerical,
               tolerance = 1e-6)
}

test_util_demand_gradient <- function(f, prices, income, utility, ...,
                                      type = c("marshallian", "hicksian")) {
  if ("marshallian" %in% type) {
    analytic <- util_demand(f, prices,
                            income = income,
                            gradient = TRUE,
                            ...)
    numerical <- util_demand_gradient_numerical(f, prices,
                                                income = income,
                                                ...)
    expect_equal(analytic, numerical,
                 tolerance = 1e-6)
  }

  if ("hicksian" %in% type) {
    analytic <- util_demand(f, prices,
                            utility = utility,
                            gradient = TRUE,
                            ...)
    numerical <- util_demand_gradient_numerical(f, prices,
                                                utility = utility,
                                                ...)
    expect_equal(analytic, numerical,
                 tolerance = 1e-6)
  }
}

test_util_expenditure_gradient <- function(f, prices, utility, ...) {
  analytic <- util_expenditure(f, prices, utility,
                               gradient = TRUE,
                               ...)
  numerical <- util_expenditure_gradient_numerical(f, prices, utility, ...)
  expect_equal(analytic, numerical,
               tolerance = 1e-6)
}

test_util_indirect_gradient <- function(f, prices, income, ...) {
  analytic <- util_indirect(f, prices, income,
                            gradient = TRUE,
                            ...)
  numerical <- util_indirect_gradient_numerical(f, prices, income, ...)
  expect_equal(analytic, numerical,
               tolerance = 1e-6)
}

util_gradient_numerical <- function(f, quantities,
                                    h = 1e-6,
                                    ...) {
  size <- length(quantities)
  gradient <- double(size)

  for (i in seq_len(size)) {
    quantities_plus <- quantities_minus <- quantities
    quantities_plus[[i]] <- quantities_plus[[i]] + h
    quantities_minus[[i]] <- quantities_minus[[i]] - h

    utility_plus <- f(quantities_plus, ...)
    utility_minus <- f(quantities_minus, ...)
    gradient[[i]] <- (utility_plus - utility_minus) / (2 * h)
  }
  gradient
}

util_demand_gradient_numerical <- function(f, prices,
                                           income = NULL,
                                           utility = NULL,
                                           h = 1e-6,
                                           ...) {
  size <- length(prices)
  gradient <- matrix(NA_real_, size, size)

  for (i in seq_len(size)) {
    prices_plus <- prices_minus <- prices
    prices_plus[[i]] <- prices_plus[[i]] + h
    prices_minus[[i]] <- prices_minus[[i]] - h

    quantities_plus <- util_demand(f, prices_plus,
                                   income = income,
                                   utility = utility,
                                   ...)
    quantities_minus <- util_demand(f, prices_minus,
                                    income = income,
                                    utility = utility,
                                    ...)
    gradient[, i] <- (quantities_plus - quantities_minus) / (2 * h)
  }
  gradient
}

util_expenditure_gradient_numerical <- function(f, prices, utility,
                                                h = 1e-6,
                                                ...) {
  size <- length(prices)
  gradient <- double(size)

  for (i in seq_len(size)) {
    prices_plus <- prices_minus <- prices
    prices_plus[[i]] <- prices_plus[[i]] + h
    prices_minus[[i]] <- prices_minus[[i]] - h

    expenditure_plus <- util_expenditure(f, prices_plus, utility, ...)
    expenditure_minus <- util_expenditure(f, prices_minus, utility, ...)
    gradient[[i]] <- (expenditure_plus - expenditure_minus) / (2 * h)
  }
  gradient
}

util_indirect_gradient_numerical <- function(f, prices, income,
                                             h = 1e-6,
                                             ...) {
  size <- length(prices)
  gradient <- double(size)

  for (i in seq_len(size)) {
    prices_plus <- prices_minus <- prices
    prices_plus[[i]] <- prices_plus[[i]] + h
    prices_minus[[i]] <- prices_minus[[i]] - h

    utility_plus <- util_indirect(f, prices_plus, income, ...)
    utility_minus <- util_indirect(f, prices_minus, income, ...)
    gradient[[i]] <- (utility_plus - utility_minus) / (2 * h)
  }
  gradient
}
