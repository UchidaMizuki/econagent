add_goods_class <- function(x) {
  structure(x,
            class = c("econ_goods", setdiff(class(x), "econ_goods")))
}

fixed_point_positive <- function(f, x, ...) {
  FixedPoint::FixedPoint(\(x, ...) log(f(exp(x), ...)),
                         Inputs = log(x),
                         ...) |>
    purrr::chuck("FixedPoint") |>
    exp()
}

timbr_names <- function(x) {
  names <- x$graph |>
    tidygraph::activate("nodes") |>
    tibble::as_tibble() |>
    names()
  setdiff(names, ".")
}

timbr_common_by <- function(by = NULL,
                            x, y) {
  if (!is.null(by)) {
    return(by)
  }
  names_x <- c(setdiff(names(x$roots), "."),
               data$graph |>
                 dplyr::mutate(node_name = timbr::node_name()) |>
                 dplyr::pull("node_name"))
  intersect(names(y), names_x)
}
