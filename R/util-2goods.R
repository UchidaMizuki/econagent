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

  function(quantity_x) {
    (income - prices[[1]] * quantity_x) / prices[[2]]
  }
}

#' Indifference curve function factory for two goods
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param f A `util` object.
#' @param quantities A numeric vector of length 2 with the quantities of the
#' goods.
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
util_2goods_indifference <- function(f, quantities,
                                     otherwise = NA_real_,
                                     interval = c(1e-6, 1e6),
                                     tol = 1e-6,
                                     ...) {
  vctrs::vec_check_size(quantities, 2)

  utility <- f(quantities)
  function(quantity_x) {
    purrr::map_dbl(
      quantity_x,
      purrr::possibly(
        \(quantity_x) {
          stats::uniroot(\(quantity_y, ...) f(c(quantity_x, quantity_y)) - utility,
                         interval = interval,
                         extendInt = "yes",
                         tol = tol,
                         ...)$root
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
#' @param f A `util` object.
#' @param quantity_y A scalar numeric of quantity of good Y.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#'
#' @return A function that takes a scalar numeric of quantity of good X and
#' returns a scalar numeric of total utility (`gradient = TRUE`) or marginal
#' utility (`gradient = FALSE`).
#'
#' @export
util_2goods_utility <- function(f, quantity_y,
                                gradient = FALSE) {
  vctrs::vec_check_size(quantity_y, 1)

  if (gradient) {
    function(quantity_x) {
      quantity_x |>
        purrr::map_dbl(
          \(quantity_x) util_gradient(f, c(quantity_x, quantity_y))[[1]]
        )
    }
  } else {
    function(quantity_x) {
      quantity_x |>
        purrr::map_dbl(
          \(quantity_x) f(c(quantity_x, quantity_y))
        )
    }
  }
}
