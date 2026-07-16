add_goods_class <- function(x) {
  structure(x, class = c("econ_goods", setdiff(class(x), "econ_goods")))
}

fixed_point_positive <- function(f, x, ...) {
  FixedPoint::FixedPoint(\(x, ...) log(f(exp(x), ...)), Inputs = log(x), ...) |>
    purrr::chuck("FixedPoint") |>
    exp()
}

timbr_names <- function(x) {
  timbr:::get_nodes(x) |>
    timbr:::drop_node() |>
    names()
}
