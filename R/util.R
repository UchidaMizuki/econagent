#' Create a new utility function
#'
#' @param f A function that returns the utility and its gradient.
#' It has a `grad` argument that allows logical input.
#' @param ... Parameters to be passed to `f`.
#' @param class Name of subclass.
#'
#' @return A `util` object.
#'
#' @export
new_util <- function(f, ...,
                     class = character()) {
  args <- rlang::list2(...)
  partialised::new_partialised(f, args,
                               class = c(class, "util"))
}

#' Calibrate a utility function
#'
#' @param f A `util` object.
#' @param quantities A numeric vector of quantities.
#' @param prices A numeric vector of prices.
#' @param ... Additional arguments.
#'
#' @return A `util` object with calibrated parameters.
#'
#' @export
util_calibrate <- function(f, quantities, prices, ...) {
  UseMethod("util_calibrate")
}

#' Marshallian demand function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param income A scalar numeric of income.
#' @param grad Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `grad = FALSE`, a numeric vector of quantities. When
#' `grad = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand_marshallian <- function(f, prices, income,
                                    grad = FALSE,
                                    ...) {
  UseMethod("util_demand_marshallian")
}

#' Hicksian demand function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param utility A scalar numeric of utility level.
#' @param grad Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `grad = FALSE`, a numeric vector of quantities. When
#' `grad = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand_hicksian <- function(f, prices, utility,
                                 grad = FALSE,
                                 ...) {
  UseMethod("util_demand_hicksian")
}

#' Demand function
#'
#' This function works as Marshallian demand function when `income` is input, and
#' as Hicksian demand function when `utility` is input.
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param income A scalar numeric of income. If `NULL`, `utility` must be
#' provided.
#' @param utility A scalar numeric of utility level. If `NULL`, `income` must be
#' provided.
#' @param grad Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `grad = FALSE`, a numeric vector of quantities. When
#' `grad = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand <- function(f, prices,
                        income = NULL,
                        utility = NULL,
                        grad = FALSE,
                        ...) {
  if (!is.null(income) && !is.null(utility)) {
    cli::cli_abort("Either {.arg income} or {.arg utility} must be NULL.")
  } else if (!is.null(income)) {
    util_demand_marshallian(f, prices, income,
                            grad = grad,
                            ...)
  } else if (!is.null(utility)) {
    util_demand_hicksian(f, prices, utility,
                         grad = grad,
                         ...)
  } else {
    cli::cli_abort("Either {.arg income} or {.arg utility} must be provided.")
  }
}

#' Expenditure function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param utility A scalar numeric of utility level.
#' @param grad Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return When `grad = FALSE`, a scalar numeric of expenditure. When
#' `grad = TRUE`, a numeric vector of gradients of expenditure related to
#' prices.
#'
#' @export
util_expenditure <- function(f, prices, utility,
                             grad = FALSE,
                             ...) {
  if (grad) {
    util_demand_hicksian(f, prices, utility, ...) + prices * util_demand_hicksian(f, prices, utility, grad = TRUE, ...)
  } else {
    sum(prices * util_demand_hicksian(f, prices, utility, ...))
  }
}

#' Indirect utility function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param income A scalar numeric of income.
#' @param grad Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return When `grad = FALSE`, a scalar numeric of utility level. When
#' `grad = TRUE`, a numeric vector of gradients of utility level related to
#' prices.
#'
#' @export
util_indirect <- function(f, prices, income,
                          grad = FALSE,
                          ...) {
  if (grad) {
    f(util_demand_marshallian(f, prices, income), grad = TRUE) * util_demand_marshallian(f, prices, income, grad = TRUE)
  } else {
    f(util_demand_marshallian(f, prices, income))
  }
}
