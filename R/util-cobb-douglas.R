#' Cobb-Douglas utility function
#'
#' @param efficiency A scalar numeric of efficiency parameter. By default,
#' `NA_real_`.
#' @param weights A numeric vector of weight parameters. By default, `double()`.
#' @param homothetic A logical scalar. By default, `TRUE`.
#'
#' @return A `util_cobb_douglas` object.
#'
#' @export
util_cobb_douglas <- function(efficiency = NA_real_,
                              weights = double(),
                              homothetic = length(weights) == 0 || sum(weights) == 1) {
  check_efficiency_nonnegative(efficiency)
  check_weights_nonnegative(weights)

  f <- function(quantities, efficiency, weights) {
    efficiency * prod(quantities ^ weights, na.rm = TRUE)
  }

  if (homothetic) {
    if (length(weights) > 0 && sum(weights) != 1) {
      cli::cli_abort("The sum of {.code weights} must be equal to 1.")
    }

    new_util_homothetic(f,
                        efficiency = efficiency,
                        weights = weights,
                        class = "util_cobb_douglas")
  } else {
    new_util(f,
             efficiency = efficiency,
             weights = weights,
             class = "util_cobb_douglas")
  }
}

#' @export
util_gradient.util_cobb_douglas <- function(f, quantities, ...) {
  rlang::check_dots_empty()

  gradient_utility <- f$efficiency * prod(quantities ^ f$weights, na.rm = TRUE) * f$weights / quantities
  gradient_utility[quantities == 0] <- 0
  gradient_utility
}

#' @export
util_calibrate.util_cobb_douglas <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  if (!inherits(f, "util_homothetic")) {
    cli::cli_abort("The utility function {.arg f} must be homothetic.")
  }

  weights <- prices * quantities
  weights <- weights / sum(weights)

  f$weights <- weights
  f$efficiency <- sum(prices * quantities) / prod(quantities ^ f$weights, na.rm = TRUE)
  f
}

#' @export
util_demand_marshallian.util_cobb_douglas <- function(f, prices, income,
                                                      gradient = FALSE,
                                                      ...) {
  rlang::check_dots_empty()

  if (gradient) {
    diag(income * f$weights / sum(f$weights) * -prices ^ -2)
  } else {
    income * f$weights / sum(f$weights) / prices
  }
}

#' @export
type_sum.util_cobb_douglas <- function(x) {
  "Cobb-Douglas"
}

#' @export
obj_sum.util_cobb_douglas <- function(x) {
  type_sum(x)
}
