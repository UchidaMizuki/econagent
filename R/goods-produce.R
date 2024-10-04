#' Produce goods
#'
#' @param data A `econ_goods` object.
#' @param quantities A data frame of a list of data frames that contains columns
#' `quantity`.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#'
#' @return A `econ_goods` object.
goods_produce <- function(data,
                          quantities = NULL,
                          gradient = FALSE) {
  if (gradient) {
    data <- data |>
      dplyr::mutate(gradient = vctrs::vec_init(double()))
  }

  if (is.data.frame(quantities)) {
    quantities <- list(quantities)
  }

  for (i in seq_along(quantities)) {
    if (gradient) {
      quantities[[i]] <- quantities[[i]] |>
        tibble::add_column(gradient = 1)
    }

    data <- data |>
      dplyr::rows_update(quantities[[i]],
                         by = setdiff(names(quantities[[i]]), c("quantity", "gradient")))
  }

  data |>
    timbr::traverse(\(x, y) {
      quantities <- util_demand(y$utility[[1]], x$price,
                                utility = y$quantity)
      x$quantity <- quantities

      if (gradient) {
        if (!inherits(y$utility[[1]], "util_homothetic")) {
          cli::cli_abort("The utility function must be homothetic.")
        }

        gradient_new <- quantities / y$quantity

        x$gradient <- gradient_new * y$gradient
      }
      x
    },
    .climb = TRUE)
}

#' Produce goods recursively
#'
#' @param data A `econ_goods` object.
#' @param f A function that returns a data frame with columns `quantity`.
#' @param gradient A function that returns a data frame with columns `gradient`.
#' @param ... Additional arguments passed to `stats::optim()`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_produce_recursively <- function(data, f,
                                      gradient = NULL, ...) {
  quantities <- f(data)

  par <- quantities$quantity
  fn <- function(par) {
    quantities$quantity <- par
    data <- goods_produce(data, quantities)
    quantities_new <- f(data)

    sum((quantities_new$quantity - par) ^ 2)
  }
  if (is.null(gradient)) {
    gr <- NULL
  } else {
    gr <- function(par) {
      quantities$quantity <- par
      data <- goods_produce(data, quantities,
                            gradient = TRUE)
      quantities_new <- f(data) |>
        dplyr::left_join(gradient(data),
                         by = setdiff(names(quantities), "quantity"))

      2 * (quantities_new$quantity - par) * (quantities_new$gradient - 1)
    }
  }
  par <- stats::optim(par = par,
                      fn = fn,
                      gr = gr,
                      method = "BFGS", ...) |>
    purrr::chuck("par")

  quantities$quantity <- par
  goods_produce(data, quantities)
}
