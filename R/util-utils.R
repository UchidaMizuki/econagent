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
