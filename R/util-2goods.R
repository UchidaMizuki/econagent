#' Budget line function factory for two goods
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param prices A numeric vector of length 2 with the prices of the goods.
#' @param income A scalar numeric of income.
#'
#' @return A function that takes a scalar numeric of quantity of good X and
#' returns a scalar numeric of quantity of good Y.
#'
#' @export
util_2goods_budget <- function(prices, income) {
  vctrs::vec_check_size(prices, 2)
  vctrs::vec_check_size(income, 1)

  function(quantity, axis = 1) {
    (income - prices[axis] * quantity) / prices[-axis]
  }
}

#' Indifference curve function factory for two goods
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param f A `econ_util` object.
#' @param utility A scalar numeric of utility.
#' @param otherwise Default value when the root is not found. By default,
#' `NA_real`.
#' @param interval Passed to [stats::uniroot()].
#' @param tol Passed to [stats::uniroot()].
#' @param ... Passed to [stats::uniroot()].
#'
#' @return A function that takes a scalar numeric of quantity of good X and
#' returns a scalar numeric of quantity of good Y.
#'
#' @export
util_2goods_indifference <- function(f, utility,
                                     otherwise = NA_real_,
                                     interval = c(1e-6, 1e6),
                                     tol = 1e-6,
                                     ...) {
  vctrs::vec_check_size(utility, 1)

  function(quantity, axis = 1) {
    purrr::map_dbl(
      quantity,
      purrr::possibly(
        \(quantity) {
          stats::uniroot(
            \(quantity_other, ...) {
              quantities <- rep(NA_real_, 2)
              quantities[axis] <- quantity
              quantities[-axis] <- quantity_other

              f(quantities) - utility
            },
            interval = interval,
            extendInt = "yes",
            tol = tol,
            ...
          )$root
        },
        otherwise = otherwise
      )
    )
  }
}

#' Utility function factory for two goods with a given quantity of good Y
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param f A `econ_util` object.
#' @param quantity_intercept A scalar numeric of quantity.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#'
#' @return A function that takes a scalar numeric of quantity of good X and
#' returns a scalar numeric of total utility (`gradient = TRUE`) or marginal
#' utility (`gradient = FALSE`).
#'
#' @export
util_2goods_utility <- function(f, quantity_intercept,
                                gradient = FALSE) {
  vctrs::vec_check_size(quantity_intercept, 1)

  if (gradient) {
    function(quantity, axis = 1) {
      quantity |>
        purrr::map_dbl(
          \(quantity) {
            quantities <- rep(NA_real_, 2)
            quantities[axis] <- quantity
            quantities[-axis] <- quantity_intercept

            util_gradient(f, quantities)[axis]
          }
        )
    }
  } else {
    function(quantity, axis = 1) {
      quantity |>
        purrr::map_dbl(
          \(quantity) {
            quantities <- rep(NA_real_, 2)
            quantities[axis] <- quantity
            quantities[-axis] <- quantity_intercept

            f(quantities)
          }
        )
    }
  }
}
