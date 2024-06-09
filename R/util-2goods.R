#' Budget line function factory for two goods
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
#' @param f A `util` object.
#' @param quantities A numeric vector of length 2 with the quantities of the
#' goods.
#' @param interval Passed to [stats::uniroot()].
#' @param tol Passed to [stats::uniroot()].
#' @param ... Passed to [stats::uniroot()].
#'
#' @return A function that takes a scalar numeric of quantity of good X and
#' returns a scalar numeric of quantity of good Y.
#'
#' @export
util_2goods_indifference <- function(f, quantities,
                                     interval = c(1e-6, 1e6),
                                     tol = 1e-6,
                                     ...) {
  utility <- f(quantities)
  function(quantity_x) {
    purrr::map_dbl(
      quantity_x,
      purrr::possibly(
        \(quantity_x) {
          stats::uniroot(\(quantity_y, ...) f(c(quantity_x, quantity_y)) - utility,
                         interval = interval,
                         extendInt = "upX",
                         tol = tol,
                         ...)$root
        },
        otherwise = NA_real_
      )
    )
  }
}
