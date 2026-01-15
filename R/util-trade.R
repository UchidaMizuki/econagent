util_trade_iceberg <- function(cost) {
  f <- function(quantities, cost) {
    quantities / cost
  }
  new_util_homothetic(f,
                      cost = cost,
                      class = "util_trade_iceberg")
}

#' @export
util_gradient.util_trade_iceberg <- function(f, quantities, ...) {
  rlang::check_dots_empty()

  1 / f$cost
}

#' @export
util_calibrate.util_trade_iceberg <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  f
}

#' @export
util_demand_marshallian.util_trade_iceberg <- function(f, prices, income,
                                                       gradient = FALSE,
                                                       ...) {
  rlang::check_dots_empty()

  if (gradient) {
    diag(income * -prices ^ -2)
  } else {
    income / prices
  }
}

#' @export
util_price.util_trade_iceberg <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  f$cost * prices
}

#' @export
type_sum.util_trade_iceberg <- function(x) {
  "Iceberg trade cost"
}
