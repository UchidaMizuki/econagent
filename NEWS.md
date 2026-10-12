# econagent (development version)

* econagent now requires R >= 4.1.0, since it uses the native pipe and lambda syntax (#5).

* `goods_consume_recursively()`, `goods_produce_recursively()` and `goods_reprice_recursively()` no longer fail when the values contain zeros; zero elements are now kept fixed at zero (#12).

* `util_calibrate()` now errors when `quantities` contains negative values, instead of silently returning `Inf`/`NaN` weights or a meaningless `efficiency` (#2).
