#' Create goods
#'
#' @param .data A data frame that contains columns `price` and `quantity`. If
#' `price` is not available, it will be created with a value of 1.
#' @param ... Variables to group by.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_by <- function(.data, ...) {
  if (!"price" %in% names(.data)) {
    .data <- .data |>
      tibble::add_column(price = 1)
  }

  .data |>
    timbr::forest_by(...) |>
    dplyr::select("price", "quantity") |>
    dplyr::mutate(utility = list(NULL)) |>
    add_goods_class()
}

#' Compose goods
#'
#' @param data A `econ_goods` object.
#' @param utility A `econ_util` object or a data frame that contains columns
#' `utility`.
#' @param node A node name for composition goods. By default, it is `NULL`.
#' @param utility_negative A `econ_util` object used for quantities with a
#' negative sign, or `NULL` (the default). If not `NULL`, `utility` (which
#' must be a single `econ_util` object) is wrapped with
#' [util_sign_split()] so that mixed-sign quantities can be calibrated;
#' negative quantities are calibrated with `utility_negative` after being
#' sign-flipped.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_compose <- function(data, utility, node = NULL, utility_negative = NULL) {
  if (!is.null(utility_negative)) {
    if (!inherits(utility, "econ_util")) {
      cli::cli_abort(
        "{.arg utility_negative} requires {.arg utility} to be a single {.cls econ_util} object."
      )
    }
    utility <- util_sign_split(utility, utility_negative = utility_negative)
  }

  data <- data |>
    dplyr::summarise(
      prices = .data$price |>
        rlang::set_names(timbr::node_value()) |>
        list(),
      quantities = .data$quantity |>
        rlang::set_names(timbr::node_value()) |>
        list(),
      .node = node
    )

  if (inherits(utility, "econ_util")) {
    data <- data |>
      dplyr::mutate(utility = list(.env$utility))
  } else {
    data <- data |>
      dplyr::rows_update(utility, by = setdiff(names(utility), "utility"))
  }

  data |>
    dplyr::mutate(
      utility = list(.data$utility, .data$prices, .data$quantities) |>
        purrr::pmap(\(utility, prices, quantities) {
          util_calibrate(utility, prices, quantities)
        }),
      price = list(.data$utility, .data$prices, .data$quantities) |>
        purrr::pmap_dbl(\(utility, prices, quantities) {
          util_price(utility, prices, quantities)
        }),
      quantity = list(.data$utility, .data$quantities) |>
        purrr::pmap_dbl(\(utility, quantities) {
          utility(quantities)
        })
    ) |>
    dplyr::select(!c("prices", "quantities")) |>
    add_goods_class()
}

#' @export
tbl_sum.econ_goods <- function(x, ...) {
  out <- NextMethod()
  names(out)[[1]] <- "Goods"
  out
}
