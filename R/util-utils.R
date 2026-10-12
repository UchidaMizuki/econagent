big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

check_efficiency_nonnegative <- function(efficiency) {
  if (!is.na(efficiency) && efficiency < 0) {
    cli::cli_abort("{.code efficiency} must be non-negative.")
  }
}

check_weights_nonnegative <- function(weights) {
  if (!rlang::is_empty(weights) && any(weights < 0)) {
    cli::cli_abort("{.code weights} must be non-negative.")
  }
}

check_quantities_nonnegative <- function(
  quantities,
  call = rlang::caller_env()
) {
  if (any(quantities < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg quantities} must be non-negative.", call = call)
  }
}

check_substitution_ces <- function(substitution, call = rlang::caller_env()) {
  if (
    !is_finite_number(substitution) || substitution >= 1 || substitution == 0
  ) {
    cli::cli_abort(
      "{.arg substitution} must be a finite number less than 1 and not equal to 0.",
      call = call
    )
  }
}

check_substitution_cet <- function(substitution, call = rlang::caller_env()) {
  if (!is_finite_number(substitution) || substitution <= 1) {
    cli::cli_abort(
      "{.arg substitution} must be a finite number greater than 1.",
      call = call
    )
  }
}

is_finite_number <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x)
}
