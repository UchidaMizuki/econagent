#' Create a new homothetic utility function
#'
#' @param f A function that returns the utility and its gradient.
#' It has a `gradient` argument that allows logical input.
#' @param ... Parameters to be passed to `f`.
#' @param class Name of subclass.
#'
#' @return A `util_homothetic` object.
#'
#' @export
new_util_homothetic <- function(f, ...,
                                class = character()) {
  new_util(f, ...,
           class = c(class, "util_homothetic"))
}

#' @export
util_demand_hicksian.util_homothetic <- function(f, prices, utility,
                                                 gradient = FALSE,
                                                 ...) {
  quantities <- util_demand_marshallian(f, prices, 1, ...)

  if (gradient) {
    gradient_quantities <- util_demand_marshallian(f, prices, 1, gradient = TRUE, ...)
    utility * sweep(gradient_quantities, 1, (f(quantities) - quantities * f(quantities, gradient = TRUE)) / f(quantities) ^ 2, "*")
  } else {
    utility * quantities / f(quantities)
  }
}
