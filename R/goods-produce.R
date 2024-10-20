#' Produce goods
#'
#' @param data A `econ_goods` object.
#' @param quantities A data frame of a list of data frames that contains columns
#' `quantity`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_produce <- function(data,
                          quantities = NULL) {
  if (is.data.frame(quantities)) {
    quantities <- list(quantities)
  }

  for (i in seq_along(quantities)) {
    data <- data |>
      dplyr::rows_update(quantities[[i]],
                         by = setdiff(names(quantities[[i]]), "quantity"))
  }

  data |>
    timbr::traverse(\(x, y) {
      quantities <- util_demand(y$utility[[1]], x$price,
                                utility = y$quantity)
      x$quantity <- quantities
      x
    },
    .climb = TRUE)
}

#' Produce goods recursively
#'
#' @param data A `econ_goods` object.
#' @param f A function that returns a data frame with columns `quantity`.
#' @param ... Additional arguments passed to `stats::optim()`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_produce_recursively <- function(data, f, ...) {
  quantities <- f(data)

  quantity <- quantities$quantity
  produce <- function(quantity) {
    quantities$quantity <- quantity
    data <- goods_produce(data, quantities)
    quantities_new <- f(data)
    quantities_new$quantity
  }
  quantity <- fixed_point_positive(produce, quantity, ...)

  quantities$quantity <- quantity
  goods_produce(data, quantities)
}
