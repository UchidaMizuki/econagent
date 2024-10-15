#' Consume goods
#'
#' @param data A `econ_goods` object.
#' @param incomes A data frame of a list of data frames that contains columns
#' `income`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_consume <- function(data,
                          incomes = NULL) {
  if (is.data.frame(incomes)) {
    incomes <- list(incomes)
  }

  data <- data |>
    dplyr::mutate(income = .data$price * .data$quantity)
  for (i in seq_along(incomes)) {
    data <- data |>
      dplyr::rows_update(incomes[[i]],
                         by = setdiff(names(incomes[[i]]), "income"))
  }

  data |>
    dplyr::mutate(quantity = .data$income / .data$price) |>
    dplyr::select(!"income") |>
    timbr::traverse(\(x, y) {
      quantities <- util_demand(y$utility[[1]], x$price,
                                utility = y$quantity)
      x$quantity <- quantities
      x
    },
    .climb = TRUE)
}

#' Consume goods recursively
#'
#' @param data A `econ_goods` object.
#' @param f A function that returns a data frame with columns `income`.
#' @param ... Additional arguments passed to `stats::optim()`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_consume_recursively <- function(data, f, ...) {
  incomes <- f(data)

  income <- incomes$income
  consume <- function(income) {
    incomes$income <- income
    data <- goods_consume(data, incomes)
    incomes_new <- f(data)
    incomes_new$income
  }
  income <- FixedPoint::FixedPoint(consume, income, ...) |>
    purrr::chuck("FixedPoint")

  incomes$income <- income
  goods_consume(data, incomes)
}
