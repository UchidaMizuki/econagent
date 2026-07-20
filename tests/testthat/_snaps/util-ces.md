# util_calibrate.util_ces() rejects negative quantities

    Code
      util_calibrate(f, prices = c(1, 1, 5), quantities = c(2, -2, 0))
    Condition
      Error in `check_quantities_nonnegative()`:
      ! `quantities` must be non-negative.

