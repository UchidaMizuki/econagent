#' Leontief utility function
#'
#' @param efficiency A scalar numeric of efficiency parameter. By default,
#' `NA_real_`.
#' @param weights A numeric vector of weight parameters. By default, `double()`.
#'
#' @return A `util_leontief` object.
#'
#' @export
util_leontief <- function(efficiency = NA_real_,
                          weights = double()) {
  check_efficiency_nonnegative(efficiency)
  check_weights_nonnegative(weights)

  f <- function(quantities, efficiency, weights) {
    efficiency * min(quantities / weights, na.rm = TRUE)
  }

  new_util_homothetic(f,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_leontief")
}

#' @export
util_gradient.util_leontief <- function(f, quantities, ...) {
  rlang::check_dots_empty()

  loc_min <- quantities / f$weights
  loc_min <- loc_min == min(loc_min, na.rm = TRUE)
  loc_min[is.na(loc_min)] <- FALSE

  size <- length(quantities)
  if (sum(loc_min, na.rm = TRUE) == 1) {
    gradient_utility <- rep(0, size)
    gradient_utility[loc_min] <- f$efficiency / f$weights[loc_min]
  } else {
    gradient_utility <- rep(NA_real_, size)
  }
  gradient_utility
}

#' @export
util_calibrate.util_leontief <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  f$weights <- quantities / sum(quantities)
  f$efficiency <- sum(prices * quantities) / min(quantities / f$weights, na.rm = TRUE)
  f
}

#' @export
util_demand_marshallian.util_leontief <- function(f, prices, income,
                                                  gradient = FALSE,
                                                  ...) {
  rlang::check_dots_empty()

  if (gradient) {
    outer(income * f$weights, -sum(prices * f$weights) ^ -2 * f$weights)
  } else {
    income * f$weights / sum(prices * f$weights)
  }
}

#' @export
util_demand_hicksian.util_leontief <- function(f, prices, utility,
                                               gradient = FALSE,
                                               ...) {
  rlang::check_dots_empty()

  if (gradient) {
    size <- length(prices)
    matrix(0, size, size)
  } else {
    NextMethod()
  }
}

#' @export
type_sum.util_leontief <- function(x) {
  "Leontief"
}

#' @export
obj_sum.util_leontief <- function(x) {
  type_sum(x)
}
