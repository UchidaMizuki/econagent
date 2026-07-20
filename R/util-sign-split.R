#' Sign-split utility function
#'
#' Combines a utility function for non-negative quantities
#' (`utility_positive`) with a utility function for negative quantities
#' (`utility_negative`), so that composite goods with mixed-sign quantities
#' (e.g. inventory drawdown, subsidies, statistical discrepancies) can be
#' calibrated without raising negative quantities to fractional powers.
#' `quantities` are partitioned by sign at calibration time; the negative
#' partition is negated before being passed to `utility_negative`, and the
#' resulting scalar is negated back.
#'
#' @param utility_positive A `econ_util` object used for non-negative
#' quantities.
#' @param utility_negative A `econ_util` object used for (sign-flipped)
#' negative quantities. By default, `util_leontief()`.
#'
#' @return A `util_sign_split` object.
#'
#' @export
util_sign_split <- function(
  utility_positive,
  utility_negative = util_leontief()
) {
  f <- function(
    quantities,
    utility_positive,
    utility_negative,
    loc_positive,
    combine
  ) {
    quantity_positive <- if (any(loc_positive)) {
      utility_positive(quantities[loc_positive])
    } else {
      0
    }
    quantity_negative <- if (any(!loc_positive)) {
      -utility_negative(-quantities[!loc_positive])
    } else {
      0
    }
    quantity_positive + quantity_negative
  }

  new_util_homothetic(
    f,
    utility_positive = utility_positive,
    utility_negative = utility_negative,
    loc_positive = logical(),
    combine = util_leontief(),
    class = "util_sign_split"
  )
}

#' @export
util_calibrate.util_sign_split <- function(f, prices, quantities, ...) {
  rlang::check_dots_empty()

  loc_positive <- quantities >= 0
  f$loc_positive <- loc_positive

  if (any(loc_positive)) {
    f$utility_positive <- util_calibrate(
      f$utility_positive,
      prices[loc_positive],
      quantities[loc_positive]
    )
    quantity_positive <- f$utility_positive(quantities[loc_positive])
  } else {
    quantity_positive <- 0
  }

  if (any(!loc_positive)) {
    f$utility_negative <- util_calibrate(
      f$utility_negative,
      prices[!loc_positive],
      -quantities[!loc_positive]
    )
    quantity_negative <- -f$utility_negative(-quantities[!loc_positive])
  } else {
    quantity_negative <- 0
  }

  # `util_calibrate.util_leontief()` rejects negative quantities, but the two
  # branch totals combined here are expected to differ in sign (that is the
  # entire point of the split), so the fixed-proportions weights are derived
  # directly here instead of going through the guarded generic.
  branch_quantities <- c(quantity_positive, quantity_negative)
  combine <- f$combine
  combine$weights <- branch_quantities / sum(branch_quantities)
  combine$weights[is.nan(combine$weights)] <- 0
  combine$efficiency <- sum(branch_quantities) /
    min(branch_quantities / combine$weights, na.rm = TRUE)
  f$combine <- combine

  f
}

#' @export
util_demand_marshallian.util_sign_split <- function(
  f,
  prices,
  income,
  gradient = FALSE,
  ...
) {
  rlang::check_dots_empty()

  if (gradient) {
    cli::cli_abort("Gradient is not available.")
  }

  loc_positive <- f$loc_positive

  # A branch's own price index is the cost of one unit of its utility. An
  # empty branch has zero weight in `combine`, so its price is a dummy that
  # must stay finite (an NA/NaN dummy would propagate through `combine`'s
  # `weights * prices` even when multiplied by a zero weight).
  price_positive <- if (any(loc_positive)) {
    util_expenditure(f$utility_positive, prices[loc_positive], utility = 1)
  } else {
    1
  }
  price_negative <- if (any(!loc_positive)) {
    util_expenditure(f$utility_negative, prices[!loc_positive], utility = 1)
  } else {
    1
  }

  branch_quantities <- util_demand_marshallian(
    f$combine,
    c(price_positive, price_negative),
    income
  )

  quantities <- rep(0, length(prices))
  if (any(loc_positive)) {
    quantities[loc_positive] <- util_demand_hicksian(
      f$utility_positive,
      prices[loc_positive],
      utility = branch_quantities[[1]]
    )
  }
  if (any(!loc_positive)) {
    quantities[!loc_positive] <- -util_demand_hicksian(
      f$utility_negative,
      prices[!loc_positive],
      utility = -branch_quantities[[2]]
    )
  }
  quantities
}

#' @export
type_sum.util_sign_split <- function(x) {
  "Sign split"
}
