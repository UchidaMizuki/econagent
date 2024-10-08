#' Reprice goods
#'
#' @param data A `econ_goods` object.
#' @param prices A data frame or a list of data frames that contains columns
#' `price`.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_reprice <- function(data,
                          prices = NULL,
                          gradient = FALSE) {
  if (gradient) {
    data <- data |>
      dplyr::mutate(gradient = vctrs::vec_init(list()))
  }

  if (is.data.frame(prices)) {
    prices <- list(prices)
  }

  for (i in seq_along(prices)) {
    if (gradient) {
      prices[[i]] <- prices[[i]] |>
        tibble::add_column(gradient = prices[[i]] |>
                             dplyr::mutate(price = 1) |>
                             vctrs::vec_chop())
    }

    data <- data |>
      dplyr::rows_update(prices[[i]],
                         by = setdiff(names(prices[[i]]), c("price", "gradient")))
  }

  data |>
    timbr::traverse(\(x, y) {
      quantities <- util_demand(x$utility[[1]], y$price,
                                utility = x$quantity)
      x$price <- sum(y$price * quantities) / x$quantity

      if (gradient) {
        gradient_quantities <- util_demand(x$utility[[1]], y$price,
                                           utility = x$quantity,
                                           gradient = TRUE)
        gradient_new <- as.double(gradient_quantities %*% y$price) / x$quantity +
          quantities / x$quantity

        x$gradient[[1]] <- purrr::map2(
          gradient_new, y$gradient,
          \(gradient_new, gradient) {
            if (vctrs::vec_is_empty(gradient)) {
              NULL
            } else {
              gradient |>
                dplyr::mutate(price = .data$price * .env$gradient_new)
            }
          }
        ) |>
          dplyr::bind_rows()
      }
      x
    })
}

#' Reprice goods recursively
#'
#' @param data A `econ_goods` object.
#' @param f A function that returns a data frame with columns `price`.
#' @param gradient A function that returns a data frame with columns `gradient`.
#' @param ... Additional arguments passed to `stats::optim()`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_reprice_recursively <- function(data, f,
                                      gradient = NULL, ...) {
  prices <- f(data)

  par <- prices$price
  fn <- function(par) {
    prices$price <- par
    data <- goods_reprice(data, prices)
    prices_new <- f(data)

    sum((prices_new$price - par) ^ 2)
  }
  if (is.null(gradient)) {
    gr <- NULL
  } else {
    gr <- function(par) {
      prices$price <- par
      data <- goods_reprice(data, prices,
                            gradient = TRUE)
      prices_new <- f(data) |>
        dplyr::left_join(gradient(data),
                         by = setdiff(names(prices), "price"))

      purrr::map2_dbl(
        prices_new$price - par, prices_new$gradient,
        \(change_price, gradient) {
          2 * sum(change_price * (vctrs::vec_size(gradient) * gradient$price - 1))
        }
      )
    }
  }
  par <- stats::optim(par = par,
                      fn = fn,
                      gr = gr,
                      method = "BFGS", ...) |>
    purrr::chuck("par")

  prices$price <- par
  goods_reprice(data, prices)
}
