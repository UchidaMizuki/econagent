#' Update goods data
#'
#' @param data A `econ_goods` object.
#' @param value A data frame with new values.
#'
#' @return A `econ_goods` object.
#'
#' @export
goods_update <- function(data, value) {
  by <- timbr_common_by(x = data,
                        y = value)
  names_data <- timbr_names(data)
  names_value <- names(value)

  names_value_utility <- setdiff(names_value, c(by, names_data))
  for (name in names_value_utility) {
    data <- data |>
      goods_update_utility(name, value[c(by, name)])
  }

  names_value <- intersect(names_value, names_data)
  data <- data |>
    dplyr::rows_update(value[c(by, names_value)],
                       by = by)
  data
}

goods_update_utility <- function(data, name, value) {
  value[[name]] <- as.list(value[[name]])
  data <- data |>
    dplyr::mutate(!!name := .data$utility |>
                    purrr::map(\(x) {
                      x[[name]]
                    })) |>
    dplyr::rows_update(value,
                       by = setdiff(names(value), name))
  data$graph <- data$graph |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(utility = list(.data$utility, .data[[name]]) |>
                    purrr::pmap(\(utility, value) {
                      utility[[name]] <- value
                      utility
                    })) |>
    dplyr::select(!dplyr::all_of(name))
  data
}
