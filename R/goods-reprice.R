#' Reprice goods
#'
#' @param data A `econ_goods` object.
#' @param prices A data frame that contains columns `price`.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#'
#' @return A `econ_goods` object.
goods_reprice <- function(data, prices,
                          gradient = FALSE) {
  if (gradient) {
    prices <- prices |>
      tibble::add_column(prices = prices |>
                           dplyr::mutate(price = 1) |>
                           vctrs::vec_chop())
    data <- data |>
      dplyr::mutate(prices = vctrs::vec_init(list()))
  }

  data |>
    dplyr::rows_update(prices,
                       by = setdiff(names(prices), c("price", "prices"))) |>
    timbr::traverse(function(x, y) {
      quantities <- util_demand(x$utility[[1]], y$price,
                                utility = x$quantity)
      x$price <- sum(y$price * quantities) / x$quantity

      if (gradient) {
        gradient_quantities <- util_demand(x$utility[[1]], y$price,
                                           utility = x$quantity,
                                           gradient = TRUE)
        prices_new <- as.double(gradient_quantities %*% y$price) / x$quantity +
          quantities / x$quantity

        x$prices[[1]] <- purrr::map2(
          prices_new, y$prices,
          \(prices_new, prices) {
            prices |>
              dplyr::mutate(price = .data$price * .env$prices_new)
          }
        ) |>
          dplyr::bind_rows()
      }
      x
    })
}
