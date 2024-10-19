#' Create a new utility function
#'
#' @param f A function that returns the utility.
#' @param ... Parameters to be passed to `f`.
#' @param class Name of subclass.
#'
#' @return A `econ_util` object.
#'
#' @export
new_util <- function(f, ...,
                     class = character()) {
  dots <- rlang::list2(...)
  partialised::new_partialised(f, dots,
                               class = c(class, "econ_util"))
}

#' Gradient of a utility function
#'
#' @param f A `econ_util` object.
#' @param quantities A numeric vector of quantities.
#' @param ... Additional arguments.
#'
#' @return A numeric vector of gradients.
#'
#' @export
util_gradient <- function(f, quantities, ...) {
  UseMethod("util_gradient")
}

#' Calibrate a utility function
#'
#' @param f A `econ_util` object.
#' @param prices A numeric vector of prices.
#' @param quantities A numeric vector of quantities.
#' @param ... Additional arguments.
#'
#' @return A `econ_util` object with calibrated parameters.
#'
#' @export
util_calibrate <- function(f, prices, quantities, ...) {
  UseMethod("util_calibrate")
}

#' Marshallian demand function
#'
#' @param f A `econ_util` object.
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
  vctrs::vec_check_size(income, 1)
  UseMethod("util_demand_marshallian")
}

#' Hicksian demand function
#'
#' @param f A `econ_util` object.
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
  vctrs::vec_check_size(utility, 1)
  UseMethod("util_demand_hicksian")
}

#' @export
util_demand_hicksian.econ_util <- function(f, prices, utility,
                                           gradient = FALSE,
                                           ...) {
  income <- FixedPoint::FixedPoint(\(income, ...) income / util_indirect(f, prices, income, ...) * utility,
                                   Inputs = utility,
                                   ...)$FixedPoint

  dots <- rlang::list2(...)
  dots <- dots[!names(dots) %in% rlang::fn_fmls_names(FixedPoint::FixedPoint)]

  quantities <- rlang::exec(util_demand_marshallian, f, prices, income, !!!dots)

  if (gradient) {
    # TODO: Implement gradient
    cli::cli_abort("Gradient is not available.")
  } else {
    quantities
  }
}

#' Demand function
#'
#' This function works as Marshallian demand function when `income` is input, and
#' as Hicksian demand function when `utility` is input.
#'
#' @param f A `econ_util` object.
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
#' @param f A `econ_util` object.
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
    util_demand_hicksian(f, prices, utility, ...) +
      as.double(prices %*% util_demand_hicksian(f, prices, utility, gradient = TRUE, ...))
  } else {
    sum(prices * util_demand_hicksian(f, prices, utility, ...))
  }
}

#' Indirect utility function
#'
#' @param f A `econ_util` object.
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
    as.double(util_gradient(f, util_demand_marshallian(f, prices, income, ...)) %*%
                util_demand_marshallian(f, prices, income, gradient = TRUE, ...))
  } else {
    f(util_demand_marshallian(f, prices, income, ...))
  }
}
