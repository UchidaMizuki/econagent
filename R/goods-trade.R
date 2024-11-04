#' Iceberg trade cost
#'
#' @param data A `econ_goods` object.
#' @param trade A `tibble` object with `cost` column.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_trade_iceberg <- function(data, trade) {
  utility <- data |>
    dplyr::select(!c("price", "quantity")) |>
    dplyr::mutate(cost = 1) |>
    dplyr::rows_update(trade,
                       by = setdiff(names(trade), "cost")) |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::mutate(utility = .data$cost |>
                    purrr::map(util_trade_iceberg),
                  .keep = "unused") |>
    dplyr::rename_with(~ "trade",
                       dplyr::last_col(1))
  data$roots <- data$roots |>
    dplyr::ungroup() |>
    dplyr::mutate(trade = utility$trade) |>
    dplyr::relocate(!".") |>
    dplyr::grouped_df(c(dplyr::group_vars(data), "trade"))

  data |>
    goods_compose(utility)
}
