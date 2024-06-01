#' Linear utility function
#'
#' @param efficiency A scalar numeric of efficiency parameter. By default,
#' `NA_real_`.
#' @param weights A numeric vector of weight parameters. By default, `double()`.
#'
#' @return A `util_linear` object.
#'
#' @export
util_linear <- function(efficiency = NA_real_,
                        weights = double()) {
  f <- function(quantities, efficiency, weights,
                grad = FALSE) {
    if (grad) {
      efficiency * weights
    } else {
      efficiency * sum(weights * quantities)
    }
  }

  new_util_homothetic(f,
                      efficiency = efficiency,
                      weights = weights,
                      class = "util_linear")
}

#' @export
util_demand_marshallian.util_linear <- function(f, prices, income,
                                                grad = FALSE,
                                                ...) {
  rlang::check_dots_empty()

  loc_demand <- which.max(f$weights / prices)
  size <- length(prices)

  if (grad) {
    grad_quantities <- matrix(0, size, size)
    grad_quantities[loc_demand, loc_demand] <- -income / prices[loc_demand] ^ 2
    grad_quantities
  } else {
    quantities <- rep(0, size)
    quantities[loc_demand] <- income / prices[loc_demand]
    quantities
  }
}

#' @export
type_sum.util_linear <- function(x, ...) {
  "Linear"
}

#' @export
obj_sum.util_linear <- function(x, ...) {
  type_sum(x)
}
