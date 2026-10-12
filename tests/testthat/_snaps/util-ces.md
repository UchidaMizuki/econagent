# util_calibrate.util_ces() rejects negative quantities

    Code
      util_calibrate(util_ces(substitution = 0.5), prices = c(1, 1, 5), quantities = c(
        2, -2, 0))
    Condition
      Error in `util_calibrate()`:
      ! `quantities` must be non-negative.

# util_ces() validates `substitution`

    Code
      util_ces(0)
    Condition
      Error in `util_ces()`:
      ! `substitution` must be a finite number less than 1 and not equal to 0.
    Code
      util_ces(1)
    Condition
      Error in `util_ces()`:
      ! `substitution` must be a finite number less than 1 and not equal to 0.
    Code
      util_ces(1.5)
    Condition
      Error in `util_ces()`:
      ! `substitution` must be a finite number less than 1 and not equal to 0.
    Code
      util_ces(-Inf)
    Condition
      Error in `util_ces()`:
      ! `substitution` must be a finite number less than 1 and not equal to 0.

