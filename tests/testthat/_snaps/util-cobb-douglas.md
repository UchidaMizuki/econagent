# util_calibrate.util_cobb_douglas() rejects negative quantities

    Code
      util_calibrate(util_cobb_douglas(), prices = c(1, 1, 5), quantities = c(2, -2,
        0))
    Condition
      Error in `util_calibrate()`:
      ! `quantities` must be non-negative.

