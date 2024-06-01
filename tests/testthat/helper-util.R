util_demand_gradient_numerical <- function (f, prices,
                                            income = NULL,
                                            utility = NULL,
                                            h = 1e-6,
                                            ...) {
  size <- length(prices)
  gradient <- matrix(NA_real_, size, size)

  for (i in seq_len(size)) {
    prices_plus <- prices_minus <- prices
    prices_plus[i] <- prices_plus[i] + h
    prices_minus[i] <- prices_minus[i] - h

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

get_gradient_quantities <- function(f, prices, income = NULL, utility = NULL, ...) {
  gradient_quantities_analytic <- util_demand(f, prices,
                                              income = income,
                                              utility = utility,
                                              gradient = TRUE)
  gradient_quantities_numerical <- util_demand_gradient_numerical(f, prices,
                                                                  income = income,
                                                                  utility = utility)
  list(analytic = gradient_quantities_analytic,
       numerical = gradient_quantities_numerical)
}
