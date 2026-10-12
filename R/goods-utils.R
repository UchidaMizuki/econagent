add_goods_class <- function(x) {
  structure(x, class = c("econ_goods", setdiff(class(x), "econ_goods")))
}

# Zero elements are kept fixed at 0, since they cannot be iterated on the log
# scale.
fixed_point_positive <- function(f, x, ...) {
  positive <- x > 0
  if (!any(positive)) {
    return(x)
  }

  f_positive <- function(x_positive) {
    x[positive] <- x_positive
    f(x)[positive]
  }
  x[positive] <- FixedPoint::FixedPoint(
    \(x) log(f_positive(exp(x))),
    Inputs = log(x[positive]),
    ...
  ) |>
    purrr::chuck("FixedPoint") |>
    exp()
  x
}

timbr_names <- function(x) {
  timbr:::get_nodes(x) |>
    timbr:::drop_node() |>
    names()
}
