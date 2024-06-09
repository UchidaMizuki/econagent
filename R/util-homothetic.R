#' Create a new homothetic utility function
#'
#' @param f A function that returns the utility.
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
  quantities <- util_demand_marshallian(f, prices,
                                        income = 1,
                                        ...)

  if (gradient) {
    gradient_quantities <- util_demand_marshallian(f, prices,
                                                   income = 1,
                                                   gradient = TRUE,
                                                   ...)

    sweep(gradient_quantities, 1, utility / f(quantities), "*") +
      outer(quantities, as.double(-utility / f(quantities) ^ 2 * util_gradient(f, quantities) %*% gradient_quantities))
  } else {
    quantities * utility / f(quantities)
  }
}
