#' Reprice goods
#'
#' @param data A `econ_goods` object.
#' @param prices A data frame or a list of data frames that contains columns
#' `price`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_reprice <- function(data,
                          prices = NULL) {
  if (is.data.frame(prices)) {
    prices <- list(prices)
  }

  for (i in seq_along(prices)) {
    data <- data |>
      dplyr::rows_update(prices[[i]],
                         by = setdiff(names(prices[[i]]), "price"))
  }

  data |>
    timbr::traverse(\(x, y) {
      quantities <- util_demand(x$utility[[1]], y$price,
                                utility = x$quantity)
      x$price <- sum(y$price * quantities) / x$quantity
      x
    })
}

#' Reprice goods recursively
#'
#' @param data A `econ_goods` object.
#' @param f A function that returns a data frame with columns `price`.
#' @param ... Additional arguments passed to `stats::optim()`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_reprice_recursively <- function(data, f, ...) {
  prices <- f(data)

  price <- prices$price
  reprice <- function(par) {
    prices$price <- par
    data <- goods_reprice(data, prices)
    prices_new <- f(data)
    prices_new$price
  }
  price <- FixedPoint::FixedPoint(reprice, price, ...) |>
    purrr::chuck("FixedPoint")
  prices$price <- price
  goods_reprice(data, prices)
}
