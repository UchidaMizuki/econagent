#' Create goods
#'
#' @param .data A data frame that contains columns `price` and `quantity`.
#' @param ... Variables to group by.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_by <- function(.data, ...) {
  .data |>
    timbr::forest_by(...) |>
    dplyr::select("price", "quantity") |>
    add_goods_class()
}

#' Compose goods
#'
#' @param data A `econ_goods` object.
#' @param utility A `econ_util` object.
#' @param node A node name for composition goods. By default, it is `NULL`.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_compose <- function(data, utility,
                          node = NULL) {
  data |>
    dplyr::summarise(prices = .data$price |>
                       rlang::set_names(timbr::node_value()) |>
                       list(),
                     quantities = .data$quantity |>
                       rlang::set_names(timbr::node_value()) |>
                       list(),
                     .node = node) |>
    dplyr::mutate(utility = ifelse(inherits(.env$utility, "econ_util"),
                                   list(.env$utility),
                                   unname(.env$utility[timbr::node_value()]))) |>
    dplyr::mutate(
      utility = list(.data$utility, .data$prices, .data$quantities) |>
        purrr::pmap(\(utility, prices, quantities) {
          util_calibrate(utility, prices, quantities)
        }),
      quantity = list(.data$utility, .data$quantities) |>
        purrr::pmap_dbl(\(utility, quantities) {
          utility(quantities)
        }),
      price = list(.data$prices, .data$quantities, .data$quantity) |>
        purrr::pmap_dbl(\(prices, quantities, quantity) {
          sum(prices * quantities) / quantity
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
