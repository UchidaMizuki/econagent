#' Cobb-Douglas utility function
#'
#' @param efficiency A scalar numeric of efficiency parameter. By default,
#' `NA_real_`.
#' @param weights A numeric vector of weight parameters. By default, `double()`.
#' Only linearly homogeneous functions (`sum(weights) == 1`) can be calibrated
#' with [util_calibrate()]; the default empty `weights` are set by calibration.
#'
#' @return A `util_cobb_douglas` object.
#'
#' @export
util_cobb_douglas <- function(
  efficiency = NA_real_,
  weights = double()
) {
  check_efficiency_nonnegative(efficiency)
  check_weights_nonnegative(weights)

  f <- function(quantities, efficiency, weights) {
    efficiency * prod(quantities^weights, na.rm = TRUE)
  }

  if (rlang::is_empty(weights) || sum(weights) == 1) {
    new_util_linear_homogeneous(
      f,
      efficiency = efficiency,
      weights = weights,
      class = "util_cobb_douglas"
    )
  } else {
    new_util(
      f,
      efficiency = efficiency,
      weights = weights,
      class = "util_cobb_douglas"
    )
  }
}

#' @export
util_gradient.util_cobb_douglas <- function(f, quantities, ...) {
  rlang::check_dots_empty()

  gradient_utility <- f$efficiency *
    prod(quantities^f$weights, na.rm = TRUE) *
    f$weights /
    quantities
  gradient_utility[quantities == 0] <- 0
  gradient_utility
}

#' @export
util_calibrate.util_cobb_douglas <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  if (!inherits(f, "util_linear_homogeneous")) {
    cli::cli_abort(
      "The utility function {.arg f} must be linearly homogeneous."
    )
  }

  weights <- prices * quantities
  weights <- weights / sum(weights)
  weights[is.nan(weights)] <- 0

  f$weights <- weights
  f$efficiency <- sum(prices * quantities) /
    prod(quantities^f$weights, na.rm = TRUE)
  f
}

#' @export
util_demand_marshallian.util_cobb_douglas <- function(
  f,
  prices,
  income,
  gradient = FALSE,
  ...
) {
  rlang::check_dots_empty()

  if (gradient) {
    diag(income * f$weights / sum(f$weights) * -prices^-2)
  } else {
    income * f$weights / sum(f$weights) / prices
  }
}

#' @export
type_sum.util_cobb_douglas <- function(x) {
  "Cobb-Douglas"
}
