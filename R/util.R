#' Create a new utility function
#'
#' @param f A function that returns the utility and its gradient.
#' It has a `gradient` argument that allows logical input.
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
#' @param prices A numeric vector of prices.
#' @param quantities A numeric vector of quantities.
#' @param ... Additional arguments.
#'
#' @return A `util` object with calibrated parameters.
#'
#' @export
util_calibrate <- function(f, prices, quantities, ...) {
  UseMethod("util_calibrate")
}

#' Marshallian demand function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param income A scalar numeric of income.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `gradient = FALSE`, a numeric vector of quantities. When
#' `gradient = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand_marshallian <- function(f, prices, income,
                                    gradient = FALSE,
                                    ...) {
  UseMethod("util_demand_marshallian")
}

#' Hicksian demand function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param utility A scalar numeric of utility level.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `gradient = FALSE`, a numeric vector of quantities. When
#' `gradient = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand_hicksian <- function(f, prices, utility,
                                 gradient = FALSE,
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
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return when `gradient = FALSE`, a numeric vector of quantities. When
#' `gradient = TRUE`, a numeric matrix of gradients of quantities related to prices.
#'
#' @export
util_demand <- function(f, prices,
                        income = NULL,
                        utility = NULL,
                        gradient = FALSE,
                        ...) {
  if (!is.null(income) && !is.null(utility)) {
    cli::cli_abort("Either {.arg income} or {.arg utility} must be NULL.")
  } else if (!is.null(income)) {
    util_demand_marshallian(f, prices, income,
                            gradient = gradient,
                            ...)
  } else if (!is.null(utility)) {
    util_demand_hicksian(f, prices, utility,
                         gradient = gradient,
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
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return When `gradient = FALSE`, a scalar numeric of expenditure. When
#' `gradient = TRUE`, a numeric vector of gradients of expenditure related to
#' prices.
#'
#' @export
util_expenditure <- function(f, prices, utility,
                             gradient = FALSE,
                             ...) {
  if (gradient) {
    util_demand_hicksian(f, prices, utility, ...) + prices * colSums(util_demand_hicksian(f, prices, utility, gradient = TRUE, ...))
  } else {
    sum(prices * util_demand_hicksian(f, prices, utility, ...))
  }
}

#' Indirect utility function
#'
#' @param f A `util` object.
#' @param prices A numeric vector of prices.
#' @param income A scalar numeric of income.
#' @param gradient Logical input to return the gradient. By default, `FALSE`.
#' @param ... Additional arguments.
#'
#' @return When `gradient = FALSE`, a scalar numeric of utility level. When
#' `gradient = TRUE`, a numeric vector of gradients of utility level related to
#' prices.
#'
#' @export
util_indirect <- function(f, prices, income,
                          gradient = FALSE,
                          ...) {
  if (gradient) {
    as.double(f(util_demand_marshallian(f, prices, income), gradient = TRUE) %*% util_demand_marshallian(f, prices, income, gradient = TRUE))
  } else {
    f(util_demand_marshallian(f, prices, income))
  }
}
