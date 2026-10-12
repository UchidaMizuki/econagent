#' Create a new linearly homogeneous utility function
#'
#' A linearly homogeneous (homogeneous of degree one) utility function
#' satisfies `f(t * quantities) == t * f(quantities)` for `t > 0`. This allows
#' closed-form Hicksian demand.
#'
#' @param f A function that returns the utility.
#' @param ... Parameters to be passed to `f`.
#' @param class Name of subclass.
#'
#' @return A `util_linear_homogeneous` object.
#'
#' @export
new_util_linear_homogeneous <- function(f, ..., class = character()) {
  new_util(f, ..., class = c(class, "util_linear_homogeneous"))
}

#' @export
util_demand_hicksian.util_linear_homogeneous <- function(
  f,
  prices,
  utility,
  gradient = FALSE,
  ...
) {
  quantities <- util_demand_marshallian(f, prices, income = 1, ...)

  if (gradient) {
    gradient_quantities <- util_demand_marshallian(
      f,
      prices,
      income = 1,
      gradient = TRUE,
      ...
    )

    sweep(gradient_quantities, 1, utility / f(quantities), "*") +
      outer(
        quantities,
        as.double(
          -utility /
            f(quantities)^2 *
            util_gradient(f, quantities) %*% gradient_quantities
        )
      )
  } else {
    quantities * utility / f(quantities)
  }
}
