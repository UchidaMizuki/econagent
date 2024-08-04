add_goods_class <- function(x) {
  structure(x,
            class = c("econ_goods", setdiff(class(x), "econ_goods")))
}
