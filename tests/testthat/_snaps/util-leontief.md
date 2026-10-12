# util_calibrate.util_leontief() resolves 0/0 weights to 0

    Code
      f <- util_calibrate(util_leontief(), prices = c(1, 1, 5), quantities = c(0, 0,
        0))
    Condition
      Warning in `min()`:
      no non-missing arguments to min; returning Inf

# util_calibrate.util_leontief() rejects negative quantities

    Code
      util_calibrate(util_leontief(), prices = c(1, 1, 5), quantities = c(2, -2, 0))
    Condition
      Error in `util_calibrate()`:
      ! `quantities` must be non-negative.

