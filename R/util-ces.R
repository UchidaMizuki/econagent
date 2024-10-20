#' Constant elasticity of substitution (CES) utility function
#'
#' @param substitution A scalar numeric of substitution parameter.
#' @param elasticity_of_substitution A scalar numeric of elasticity of
#' substitution.
#' @param homogeneity A scalar numeric of degree of homogeneity. By default,
#' `1`. When `homothetic = TRUE`, `homogeneity` must be equal to `1`.
#' @param efficiency A scalar numeric of efficiency parameter.
#' @param weights A numeric vector of weights.
#' @param homothetic A logical scalar. By default, `homogeneity == 1`.
#'
#' @return A `util_ces` object.
#'
#' @name util_ces
NULL

#' @rdname util_ces
#' @export
util_ces <- function(substitution,
                     homogeneity = 1,
                     efficiency = NA_real_,
                     weights = double(),
                     homothetic = homogeneity == 1) {
  check_efficiency_nonnegative(efficiency)
  check_weights_nonnegative(weights)

  f <- function(quantities, substitution, homogeneity, efficiency, weights) {
    efficiency * sum(weights * quantities ^ substitution, na.rm = TRUE) ^ (homogeneity / substitution)
  }

  if (homothetic) {
    if (homogeneity != 1) {
      cli::cli_abort("{.arg homogeneity} must be equal to 1.")
    }

    new_util_homothetic(f,
                        substitution = substitution,
                        homogeneity = homogeneity,
                        efficiency = efficiency,
                        weights = weights,
                        class = "util_ces")
  } else {
    new_util(f,
             substitution = substitution,
             homogeneity = homogeneity,
             efficiency = efficiency,
             weights = weights,
             class = "util_ces")
  }
}

#' @rdname util_ces
#' @export
util_ces2 <- function(elasticity_of_substitution,
                      homogeneity = 1,
                      efficiency = NA_real_,
                      weights = double(),
                      homothetic = homogeneity == 1) {
  util_ces(substitution = (elasticity_of_substitution - 1) / elasticity_of_substitution,
           homogeneity = homogeneity,
           efficiency = efficiency,
           weights = weights,
           homothetic = homothetic)
}

#' @export
util_gradient.util_ces <- function(f, quantities, ...) {
  rlang::check_dots_empty()

  gradient_utility <- f$efficiency *
    f$homogeneity * sum(f$weights * quantities ^ f$substitution, na.rm = TRUE) ^ (f$homogeneity / f$substitution - 1) *
    f$weights * quantities ^ (f$substitution - 1)
  gradient_utility[quantities == 0] <- 0
  gradient_utility
}

#' @export
util_calibrate.util_ces <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  if (!inherits(f, "util_homothetic")) {
    cli::cli_abort("The utility function {.arg f} must be homothetic.")
  }

  weights <- prices * quantities ^ (1 - f$substitution)
  weights <- weights / sum(weights)

  f$weights <- weights
  f$efficiency <- sum(prices * quantities) / sum(weights * quantities ^ f$substitution) ^ (1 / f$substitution)
  f
}

#' @export
util_demand_marshallian.util_ces <- function(f, prices, income,
                                             gradient = FALSE,
                                             ...) {
  rlang::check_dots_empty()

  if (gradient) {
    diag(f$weights ^ (1 / (1 - f$substitution)) / (f$substitution - 1) * prices ^ (1 / (f$substitution - 1) - 1) *
            income / sum(f$weights ^ (1 / (1 - f$substitution)) * prices ^ (f$substitution / (f$substitution - 1)))) +
      outer(f$weights ^ (1 / (1 - f$substitution)) * prices ^ (1 / (f$substitution - 1)),
            -income / sum(f$weights ^ (1 / (1 - f$substitution)) * prices ^ (f$substitution / (f$substitution - 1))) ^ 2 *
              f$weights ^ (1 / (1 - f$substitution)) * f$substitution / (f$substitution - 1) * prices ^ (1 / (f$substitution - 1)))
  } else {
    f$weights ^ (1 / (1 - f$substitution)) * prices ^ (1 / (f$substitution - 1)) *
      income / sum(f$weights ^ (1 / (1 - f$substitution)) * prices ^ (f$substitution / (f$substitution - 1)))
  }
}

#' @export
type_sum.util_ces <- function(x) {
  "CES"
}

#' @export
obj_sum.util_ces <- function(x) {
  if (inherits(x, "util_homothetic")) {
    paste0(type_sum(x), "(", big_mark(x$substitution), ")")
  } else {
    paste0(type_sum(x), "(", big_mark(x$substitution), ", ", big_mark(x$homogeneity), ")")
  }
}
