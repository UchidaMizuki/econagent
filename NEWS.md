# econagent (development version)

* econagent now requires R >= 4.1.0, since it uses the native pipe and lambda syntax (#5).

* `goods_consume_recursively()`, `goods_produce_recursively()` and `goods_reprice_recursively()` no longer fail when the values contain zeros; zero elements are now kept fixed at zero (#12).

* `goods_trade_update()` now takes the trade cost from the `cost` column of `trade` and errors if it is missing, instead of renaming the last column to `cost` (#14).

* `new_util_homothetic()` is renamed to `new_util_linear_homogeneous()`, and the class `util_homothetic` to `util_linear_homogeneous`, since the class has always meant homogeneity of degree one rather than homotheticity (#7).

* `util_calibrate()` now errors when `quantities` contains negative values, instead of silently returning `Inf`/`NaN` weights or a meaningless `efficiency` (#2).

* `util_ces()`, `util_ces2()`, `util_cet()`, `util_cet2()` and `util_cobb_douglas()` no longer have a `homothetic` argument. Whether a function is linearly homogeneous is now determined from `homogeneity` (or `sum(weights)` for Cobb-Douglas) (#7).

* `util_ces()` and `util_ces2()` now require `substitution < 1` and `substitution != 0`, and `util_cet()` and `util_cet2()` require `substitution > 1`. Invalid values previously led to division by zero or meaningless calibration results (#9).

* `util_cet()` objects now inherit from `util_ces` (#8).

* `util_expenditure(gradient = TRUE)` now returns the Hicksian demand directly (Shephard's lemma), so it also works for utility functions that are not linearly homogeneous (#10).

* `util_indirect(gradient = TRUE)` now returns a valid gradient for `util_leontief()` at the calibrated point instead of `NA` (#11).
